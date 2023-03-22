################################################################################
## SETUP ##
################################################################################

library(pacman)
p_load(
  FluHospPrediction,
  sl3,
  e1071,
  data.table,
  stringr,
  foreach,
  doParallel,
  ggplot2,
  ggthemes,
  scico,
  magrittr
)


################################################################################
## MAIN ANALYSIS FITS ##
################################################################################

source(here::here("inst", "06.1_tables-figures-setup.R"))

pkrate_risks <- fmt_risk_table(dir = respr, slug = "sl_pkrate")
pkweek_risks <- fmt_risk_table(dir = respw, slug = "sl_pkweek")
cumhosp_risks <- fmt_risk_table(dir = resch, slug = "sl_cumhosp")

main_ens <- rbindlist(
  list(
    `PeakRate` = pkrate_risks,
    `PeakWeek` = pkweek_risks,
    `CumHosp` = cumhosp_risks
  ), idcol = "Outcome"
)[, -c("DiscreteSL")]


################################################################################
## SUPER LEARNERS FIT ON OBSERVED DATA ##
################################################################################

sldirs <- list.files("results", pattern = "ObsAllSeasons", full.names = T)

slpaths <- sapply(sldirs, list.files, pattern = "slpred", full.names = T)
slnms <- str_extract(slpaths, "(?<=results/).*(?=\\-)")

slp <- rbindlist(lapply(setNames(slpaths, slnms), readRDS), idcol = "outcome")

foldrisks <- slp[, .(risk = mean(abserr)), keyby = .(outcome, week, fold)]

foldrisks[as.numeric(week) > 4] %>%
  ggplot(aes(x = week, y = risk)) +
  geom_point() +
  facet_wrap(vars(outcome), scales = "free") +
  theme_few(base_size = 30)

foldsum <- foldrisks[, .(
  risk = mean(risk),
  sd = sd(risk),
  min = min(risk),
  max = max(risk)
), keyby = .(outcome, week)]


foldsum_se <- merge(
  foldsum,
  slp[, .(N = .N, SE_risk = sd(abserr) / sqrt(.N)), .(outcome, week)],
  by = c("outcome", "week")
)

foldsum_se[, ":=" (
  ll95 = risk - qnorm(0.975) * SE_risk,
  ul95 = risk + qnorm(0.975) * SE_risk
)]

foldsum_se

foldsum_neat <- foldsum_se[, .(Outcome = outcome, Week = week, risk, SE_risk)]

foldsum_neat[, ObservedESL := paste0(
                 format(
                   round(risk, digits = 2),
                   digits = 4, trim = T, big.mark = ","
                 ), " (",
                 format(
                   round(SE_risk, digits = 3),
                   digits = 4, trim = T, big.mark = ","
                 ), ")"
               )]

foldsum_neat[, c("risk", "SE_risk") := NULL]


################################################################################
## TABLE ##
################################################################################

comptab <- merge(main_ens, foldsum_neat, by = c("Outcome", "Week"))

comptabw <- dcast(
  Week ~ Outcome,
  value.var = c("SuperLearnerCV", "ObservedESL"),
  data = comptab
)

compnum <- comptab[, ":=" (
  mainsl    = as.numeric(str_extract(SuperLearnerCV, "^.*(?= )")),
  mainsl_se = as.numeric(str_extract(SuperLearnerCV, "(?<=\\().*(?=\\))")),
  obssl     =
    as.numeric(str_remove(str_extract(ObservedESL, "^.*(?= )"), ",")),
  obssl_se  =
    as.numeric(str_remove(str_extract(ObservedESL, "(?<=\\().*(?=\\))"), ","))
)]

compnum[, ":=" (
  log_mainsl = log(mainsl),
  log_mainsl_se = log(mainsl_se),
  log_obssl = log(obssl),
  log_obssl_se = log(obssl_se)
)]

compnuml <- melt(
  compnum,
  id.vars = c("Outcome", "Week"),
  measure.vars = c("log_mainsl", "log_obssl"),
  variable.name = "sltype",
  value.name = "log_risk"
)


targlabs <- c(`PeakRate` = "Peak rate",
              `PeakWeek` = "Peak week",
              `CumHosp` = "Cumulative\nhospitalizations")

compnuml[, Outcome := factor(Outcome, levels = names(targlabs))]

cnl <- compnuml %>%
  ggplot(aes(x = as.numeric(Week), y = log_risk)) +
  geom_line(aes(group = sltype), size = 0.2) +
  geom_point(
    aes(fill = sltype),
    shape = 21, size = 5
  ) +
  facet_wrap(
    vars(Outcome),
    labeller = labeller(Outcome = targlabs)
  ) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  scale_fill_viridis_d(
    name = "Training data",
    labels = c("Simulated", "Observed"),
    begin = 0.4,
    option = "D"
  ) +
  labs(x = "\nWeek", y = "log(risk)\n") +
  theme_minimal(base_size = 20) +
  theme(
    strip.text = element_text(face = "bold", size = 20),
    panel.grid = element_line(color = "whitesmoke")
  )

plotsave(
  plot = cnl,
  width = 20,
  height = 7,
  name = "Observed-Predict-Simulated"
)
