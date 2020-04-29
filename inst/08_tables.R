# LOAD -------------------------------------------------------------------------

pacman::p_load(
  FluHospPrediction,
  data.table
)

clndir <- here::here("data", "cleaned")
resdir <- here::here("results", "paper_output")


# TABLE ONE: Parameter Targets -------------------------------------------------

emp <- fread(file.path(clndir, "empdat.csv"))

pkht <- emp[, .(pkht = max(weekrate)), by = season]
pkwk <- emp[, .(pkwk = mean(weekint[weekrate == max(weekrate)])), by = season]
cumhosp <- emp[, .(cumrate = cumrates[weekint == max(weekint)]), by = season]

targets <- pkht[pkwk[cumhosp, on = "season"], on = "season"] %>%
  .[, .(Season = season,
        `Peak rate` = pkht,
        `Peak week` = pkwk,
        `Cumulative hospitalizations` = cumrate)]

targets

fwrite(targets, file.path(resdir, "table-01_prediction-targets.csv"))


# TABLE TWO: Parameter Targets -------------------------------------------------

## No analysis output, just LaTeX equations. See manuscript file.


# TABLE THREE: Component Model Summary -----------------------------------------

## No analysis output. Typed summary of component models and tuning parameters.


# TABLES 4-6: Risk Tables for Prediction Targets -------------------------------

## Table 4: Peak Rate Risks ----------------------------------------------------

pkrate_risks <- fmt_risk_table(
  dir = file.path("results", "2020-04-22-Draft02-PeakRate-CompleteJobs"),
  slug = "sl_pkrate"
)

pkrate_risks

fwrite(pkrate_risks, file.path(resdir, "table-04_peakrate-risks.csv"))


## Table 5: Peak Week Risks ----------------------------------------------------

# TODO: temporary kludge to mark jobs that need to be resubmitted (exceeded memory request)

pkweek_risks <- fmt_risk_table(
  dir = file.path("results", "2020-04-24-Draft02-PeakWeek"),
  slug = "sl_pkweek"
)

pkweek_risks

fwrite(pkweek_risks, file.path(resdir, "table-05_peakweek_risks.csv"))


## Table 6: Cumulative Hospitalizations Risks ----------------------------------

# TODO: temporary kludge to mark jobs that need to be resubmitted (exceeded memory request)

cumhosp_risks <- fmt_risk_table(
  dir = file.path("results", "2020-04-24-Draft02-CumHosp"),
  slug = "sl_cumhosp"
)

cumhosp_risks

fwrite(cumhosp_risks, file.path(resdir, "table-06_cumhosp_risks.csv"))


# S2 TABLE: Curves by Season Template ------------------------------------------

sims <- readRDS(file.path(clndir, "hypothetical-curves.Rds"))$outhc

tmpct <- sims[weekint == 1, .N, keyby = .(`Template season` = template)
              ][, N := format(N, big.mark = ",")]

fwrite(tmpct, file.path(resdir, "table-s02_template-counts.csv"))


# S3 FIGURE: Curves by Season Template -----------------------------------------

sim_crvs <- sims[, .(
  cid,
  weekint,
  weekrate = prediction,
  template)
  ][, crvtype := "Simulated"]

emp_crvs <- emp[season != "2009-10", .(
  cid = paste(season),
  weekint,
  weekrate,
  template = season)
  ][, crvtype := "Empirical"]

crv <- rbind(sim_crvs, emp_crvs)

tempsim_plot <- crv %>%
  ggplot(
    aes(
      x = weekint,
      y = weekrate,
      group = cid
    )
  ) +
  geom_line(
    data = crv[crvtype == "Simulated"],
    alpha = 0.1,
    size = 0.5,
    color = "gray"
  ) +
  geom_line(
    data = crv[crvtype == "Empirical"],
    alpha = 1,
    size = 1,
    aes(color = "Empirical")
  ) +
  facet_wrap(~ template) +
  scale_color_manual(name = "", values = "black") +
  theme_base()

tempsim_plot

ggsave(
  plot = tempsim_plot,
  file = "interim-reports/2020-04-28_RA-Meeting/tempsim_plot.svg",
  dev = "svg"
)


# S3-S5 TABLES: Average Risk by Week, Across Component Model -------------------

## Table S3 -------------

riskdist_pkrate <- get_risk_dist("sl_pkrate")
riskdist_pkrate

fwrite(
  riskdist_pkrate,
  file.path(resdir, "table-s03_risk-distbyweek-pkrate.csv")
)


## Table S4 -------------

riskdist_pkweek <- get_risk_dist("sl_pkweek")
riskdist_pkweek

fwrite(
  riskdist_pkweek,
  file.path(resdir, "table-s04_risk-distbyweek-pkweek.csv")
)

## Table S5 -------------

riskdist_cumhosp <- get_risk_dist("sl_cumhosp")
riskdist_cumhosp

fwrite(
  riskdist_cumhosp,
  file.path(resdir, "table-s05_risk-distbyweek-cumhosp.csv")
)


# S6-SN TABLES: Inspect Weight Assignments -------------------------------------

pkrate_weights <- get_learner_weights(
  dir = file.path("results", "2020-04-22-Draft02-PeakRate-CompleteJobs"),
  slug = "sl_pkrate"
)

pkweek_weights <- get_learner_weights(
  dir = file.path("results", "2020-04-24-Draft02-PeakWeek"),
  slug = "sl_pkweek"
)

cumhosp_weights <- get_learner_weights(
  dir = file.path("results", "2020-04-24-Draft02-CumHosp"),
  slug = "sl_cumhosp"
)

sapply(
  list(pkrate_weights, pkweek_weights, cumhosp_weights),
  function(x) length(x) == 30
)

## Risk-and-weight summaries ---------------------

## Peak Rate ---------------------------------------

### Data summary

pkrate_rwsum <- join_learner_stats(
  risktables = sl_pkrate_risktables,
  weights = pkrate_weights
)

pkrate_rwsum

pkrate_lrnr_sel <- summarize_learner_selection(pkrate_rwsum)

pkrate_lrnr_sel

fwrite(
  pkrate_lrnr_sel, file.path("interim-reports/2020-04-28_RA-Meeting/pkrate_lrnr_selection.csv")
)

# theme tweak
th <-
  theme_base(
    base_family = "serif"
  ) +
  theme(
    strip.text = element_text(face = "bold")
  )

pkrate_rw_plot <- ggplot(
  pkrate_rwsum,
  aes(
    x = log(mean_risk),
    y = weight
  )) +
  geom_point(
    aes(
      shape = weight > 0,
      color = weight > 0,
      alpha = weight > 0
  ), size = 2) +
  facet_wrap(
    ~ Week,
    labeller = labeller(Week = function(x) paste("Week", x))
  ) +
  scale_shape_manual(values = c(4, 18)) +
  scale_color_viridis_d(
    direction = -1,
    end = 0.5
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha = FALSE) +
  labs(
    title = "Peak rate",
    y = "ensemble weight"
  ) +
  th

pkrate_rw_plot

ggsave(
  plot = pkrate_rw_plot,
  filename = "interim-reports/2020-04-28_RA-Meeting/pkrate_rw_plot.svg",
  dev = "svg"
)

## Peak Week ---------------------------------------

### Data summary

pkweek_rwsum <- join_learner_stats(
  sl_pkweek_risktables,
  pkweek_weights
)

pkweek_rwsum

pkweek_lrnr_sel <- summarize_learner_selection(pkweek_rwsum)

fwrite(
  pkweek_lrnr_sel, file.path("interim-reports/2020-04-28_RA-Meeting/pkweek_lrnr_selection.csv")
)

### Plot

pkweek_rw_plot <- plotrw(pkweek_rwsum, "Peak week")

ggsave(
  plot = pkweek_rw_plot,
  filename = "interim-reports/2020-04-28_RA-Meeting/pkweek_rw_plot.svg",
  dev = "svg"
)

## Cumulative Hospitalizations

### Data summary

cumhosp_rwsum <- join_learner_stats(
  sl_cumhosp_risktables,
  cumhosp_weights
)

cumhosp_rwsum

cumhosp_lrnr_sel <- summarize_learner_selection(cumhosp_rwsum)

fwrite(
  cumhosp_lrnr_sel, file.path("interim-reports/2020-04-28_RA-Meeting/cumhosp_lrnr_selection.csv")
)

### Plot

cumhosp_rw_plot <- plotrw(cumhosp_rwsum, "Cumulative hospitalizations")

ggsave(
  plot = cumhosp_rw_plot,
  filename = "interim-reports/2020-04-28_RA-Meeting/cumhosp_rw_plot.svg",
  dev = "svg"
)
