# LOAD -------------------------------------------------------------------------

pacman::p_load(
  FluHospPrediction,
  data.table
)

clndir <- here::here("data", "cleaned")
paper_output <- here::here("results", "00_paper_output")

# Slugs
figslug <- "FIG"
tabslug <- "TAB"
valslug <- "VAL"

nicefile <- function(slug, description, ext,
                     date = Sys.Date(),
                     dir = paper_output) {
  file.path(dir, paste0(slug, "_", description, "_", date, ".", ext))
}

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

fwrite(targets, nicefile(tabslug, "Prediction-Targets", "csv"))


# %% RESULTS DIRECTORIES -------------------------------------------------------

resdir <- "results"

## Main analysis files
respr <- file.path(resdir, "PeakRate-LambdaMin")
respw <- file.path(resdir, "PeakWeek-LambdaMin")
resch <- file.path(resdir, "CumHosp-LambdaMin")

## Sensitivity analysis files (alternative trend filter lambda)
respr_1se <- file.path(resdir, "PeakRate-LambdaSE")
respw_1se <- file.path(resdir, "PeakWeek-LambdaSE")
resch_1se <- file.path(resdir, "CumHosp-LambdaSE")

## Simulated curves
# sim_lr <- readRDS(file.path(clndir, "hypothetical-curves.Rds"))
sim_lm <- readRDS(file.path(clndir, "hypothetical-curves_lambda-min.Rds"))
sim_ls <- readRDS(file.path(clndir, "hypothetical-curves_lambda-1se.Rds"))


# %% LEARNER NAME LOOKUP TABLE -------------------------------------------------

# Learner lookup table (get submitted learners from a task)
lid07 <- readRDS(file.path(respr, "sl_pkrate_07.Rds"))

lchar <- data.table(
  lname = names(lid07$sl_pruned$metalearner_fit$coefficients)
)

# Relabel the random forests
relabel_rf(lchar)

# Assign IDs to the component learners and create lookup table
lchar[grepl("glmnet.*(0\\.25|0\\.5|0\\.75)", lname),
      lid := paste0("EN", 1:max(.I))]
lchar[grepl("gam", lname), lid := paste0("GAM", 1:max(.I))]
lchar[grepl("glmnet.*1_100", lname), lid := "LASSO"]
lchar[grepl("loess", lname), lid := paste0("LOESS", 1:max(.I))]
lchar[lname == "Lrnr_mean", lid := "Mean"]
lchar[grepl("nnet", lname), lid := paste0("NNet", 1:max(.I))]
lchar[grepl("polspline", lname), lid := paste0("PMARS", 1:max(.I))]

lchar[grepl("randomForest", lname), lname := relabel_rf(lchar)]
lchar[grepl("randomForest", lname), lid := paste0("RF", 1:max(.I))]

lchar[grepl("glmnet.*0_100", lname), lid := "Ridge"]
lchar[grepl("Pipeline", lname), lid := "ScreenGLM"]
lchar[grepl("svm", lname), lid := paste0("SVM", 1:max(.I))][]

lchar


# %% RISK TABLES ---------------------------------------------------------------

## TABLE: Peak Rate Risks ------------------------------------------------------

pkrate_risks <- fmt_risk_table(dir = respr, slug = "sl_pkrate")
pkrate_risks

fwrite(
  pkrate_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate", "csv")
)


## TABLE: Peak Week Risks ----------------------------------------------------

pkweek_risks <- fmt_risk_table(dir = respw, slug = "sl_pkweek")
pkweek_risks

fwrite(
  pkweek_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakWeek", "csv")
)


## TABLE: Cumulative Hospitalizations Risks ----------------------------------

cumhosp_risks <- fmt_risk_table(dir = resch, slug = "sl_cumhosp")
cumhosp_risks

fwrite(
  cumhosp_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_CumHosp", "csv")
)


# TABLE: Curves by Season Template ------------------------------------------

## Check how many observations affected by curve transformation
pretrans_sims <- lapply(1:length(sim_lm$hc), function(x) {
  cid <- rep(x, 30)
  original_sim <- unname(unlist(sim_lm$hc[[x]]$eq$fi_pluserr))
  data.table(cid = cid, weekint = 1:30, original_sim)
}) %>% rbindlist

setnames(pretrans_sims, "V1", "origsim")
pretrans_sims

sims <- sim_lm$outhc

tmpct <- sims[weekint == 1, .N, keyby = .(`Template season` = template)
              ][, N := format(N, big.mark = ",")]


fwrite(tmpct, nicefile(tabslug, "Simulation-Template-Counts", "csv"))


# Reported in methods: proportion of weeks negative and set to 0

transcomp <- sims[pretrans_sims, on = c("cid", "weekint")]

proptrans <-
  transcomp[, .N, .(origsim < 0)][, P := N / sum(N)][origsim == TRUE, P]

saveRDS(proptrans, nicefile(valslug, "Proportion-Weeks-Transformed", "Rds"))


# FIGURE: Curves by Season Template and Lambda Value ---------------------------

# compare curves by lambda
simlist <- list(
  # lambda.rand = sim_lr$outhc,
  lambda.min = sim_lm$outhc,
  lambda.1se = sim_ls$outhc
)

simdt <- rbindlist(simlist, idcol = "lambda")

simdt

# get summaries of arg_f week sliding
sapply(unique(simdt$lambda), function(x) simdt[lambda == x, summary(week)])

simdt[, .(pkrate = max(prediction)), .(lambda, cid)] %>%
  ggplot(aes(x = lambda, y = pkrate)) +
  geom_boxplot() +
  theme_base()

sim_crvs <- sims[, .(
  cid,
  weekint,
  weekrate = prediction,
  template
)][, crvtype := "Simulated"]


emp_crvs <- emp[season != "2009-10", .(
  cid = paste(season),
  weekint,
  weekrate,
  template = season)
  ][, crvtype := "Empirical"]

crv <- rbind(sim_crvs, emp_crvs)

tempsim_plot <- crv %>%
  ggplot(aes(x = weekint, y = weekrate, group = cid)) +
  geom_line(
    data = crv[crvtype == "Simulated"],
    alpha = 0.03,
    size = 0.5,
    color = "gray"
  ) +
  geom_line(
    data = crv[crvtype == "Empirical"],
    alpha = 1,
    size = 1,
    aes(color = "Empirical")
  ) +
  labs(
    y = "Hospitalization rate (per 100,000 population)",
    x = "Week"
  ) +
  facet_wrap(~ template, ncol = 5) +
  scale_color_manual(name = "", values = "black") +
  theme_base() +
  theme(legend.position = "bottom")

tempsim_plot

ggsave(
  plot = tempsim_plot,
  width = 6,
  height = 6,
  units = "in",
  file = nicefile(figslug, "Simulation-Curves-by-Template", "png"),
  dev = "png"
)


# %% Compare Simulated Curve to their Empirical Templates ----------------------

simvemp <- list(
  empirical = emp[, .(template = season, weekrate, weekint, cid = season)],
  sim.lmin = sim_lm$outhc[, .(template, weekrate = prediction, weekint, cid)],
  sim.l1se = sim_ls$outhc[, .(template, weekrate = prediction, weekint, cid)]
)

simvempl <- rbindlist(simvemp, idcol = "type")

simsum <- copy(simvempl[type != "empirical"])

simsum <- simsum[, .(
  peakrate = max(weekrate),
  peakweek = weekint[weekrate == max(weekrate)]
), .(cid, type, template)]

# summarize peak rate by lambda sim and season shape template
simsum[, .(
  mean = mean(peakrate),
  median = median(peakrate),
  sd = sd(peakrate),
  min = min(peakrate),
  max = max(peakrate)
), keyby = .(type, template)]

# summarize peak week by lambda sim and season shape template
simsum[, .(
  mean = mean(peakweek),
  median = median(peakweek),
  sd = sd(peakweek),
  min = min(peakweek),
  max = max(peakweek)
), keyby = .(type, template)]

simsum[, type := as.factor(type)][, type := relevel(type, ref = "sim.lmin")]

simtemp_bp_theme <-
  theme_base() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))

bp_peakrate <- ggplot(simsum, aes(x = template, y = peakrate)) +
  geom_boxplot(fill = "aliceblue") +
  facet_wrap(~ type) +
  labs(
    x = "Empirical template season",
    y = "Peak hospitalization rate (per 100,000 population)"
  ) +
  simtemp_bp_theme

bp_peakweek <- ggplot(simsum, aes(x = template, y = peakweek)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.5) +
  facet_wrap(~ type) +
  labs(x = "Empirical template season",
       y = "Week of peak hospitalization rate"
  ) +
  simtemp_bp_theme

chsum <- simvempl[
  type != "empirical",
  .(cumhosp = sum(weekrate)), .(cid, type, template)
] 

bp_cumhosp <- ggplot(chsum, aes(x = template, y = cumhosp)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.5) +
  facet_wrap(~ factor(type, levels = c("sim.lmin", "sim.l1se"))) +
  labs(
    x = "Empirical template season",
    y = "Cumulative hospitalization rate (per 100,000 population)"
  ) +
  simtemp_bp_theme

ggsave(
  nicefile(figslug, "Sim-Dists_Peak-Rate", "png"),
  bp_peakrate,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)

ggsave(
  nicefile(figslug, "Sim-Dists_Peak-Week", "png"),
  bp_peakweek,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)

ggsave(
  nicefile(figslug, "Sim-Dists_Cum-Hosp", "png"),
  bp_cumhosp,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)

# FIGURE: Empirical vs. Simulated Prediction Targets ---------------------------

pr_dist <- simvempl[, .(value = max(weekrate)), .(type, cid)]

pw_dist <- simvempl[, .(
  value = weekint[weekrate == max(weekrate)]
), .(type, cid)]

ch_dist <- simvempl[, .(value = sum(weekrate)), .(type, cid)]

distl <- list(peakrate = pr_dist, peakweek = pw_dist, cumhosp = ch_dist)

dl <- rbindlist(distl, idcol = "target")

facetlabs <- c(
  peakrate = "Peak rate\n(per 100,000 pop.)",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations\n(per 100,000 pop.)"
)

simtype <- c("empirical", "sim.lmin", "sim.l1se")

set.seed(1066)
simcompare <- dl[, target := factor(target, levels = names(facetlabs))
   ][, value_jitter := ifelse(
     target == "peakweek" & type %in% c("sim.lmin", "sim.l1se"),
     value + rnorm(.N, 0, 0.05),
     value
   )] %>%
  ggplot(aes(
    x = factor(type, levels = simtype),
    y = value_jitter
  )) +
  geom_point(
    aes(size = type, color = type),
    position = position_jitter(width = 0.3, seed = 1971),
    alpha = 0.3
  ) +
  geom_boxplot(
    fill = "aliceblue",
    alpha = 0.3,
    outlier.size = 0.5
  ) +
  facet_wrap(
    ~ target,
    scales = "free",
    labeller = labeller(target = facetlabs)
  ) +
  scale_color_manual(values = c("black", rep("lightgray", 2))) +
  scale_size_manual(values = c(1, 0.3, 0.3)) +
  guides(size = FALSE, color = FALSE) +
  labs(
    x = "Curve type",
    y = "Value"
  ) +
  theme_base()

ggsave(
  nicefile(figslug, "TargetDists-Emp-vs-Sim", "png"),
  simcompare,
  width = 10,
  height = 5.3,
  unit = "in"
)

# TABLES: Average Risk by Week, Across Component Models ------------------------

## Peak rate
riskdist_pkrate <- get_risk_dist("sl_pkrate")
riskdist_pkrate

fwrite(riskdist_pkrate, nicefile(tabslug, "Risk-Week_Peak-Rate", "csv"))

## Peak week
riskdist_pkweek <- get_risk_dist("sl_pkweek")
riskdist_pkweek

fwrite(riskdist_pkweek, nicefile(tabslug, "Risk-Week_Peak-Week", "csv"))


## Cum. hosp.
riskdist_cumhosp <- get_risk_dist("sl_cumhosp")
riskdist_cumhosp

fwrite(riskdist_cumhosp, nicefile(tabslug, "Risk-Week_Cum-Hosp", "csv"))


# FIGURES: Inspect Weight Assignments ------------------------------------

pkrate_weights <- get_learner_weights(
  dir = respr,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

pkweek_weights <- get_learner_weights(
  dir = respw,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

cumhosp_weights <- get_learner_weights(
  dir = resch,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

# check that we have all 30 weeks for each prediction target
sapply(
  list(pkrate_weights, pkweek_weights, cumhosp_weights),
  function(x) length(x) == 30
)


## Peak Rate ---------------------------------------

### Data summary
pkrate_rwsum <- join_learner_stats(
  risktables = sl_pkrate_risktables,
  weights = pkrate_weights
)

pkrate_rwsum

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum)

### Risk tile plot
prt_pr <- plot_risktiles(pkrate_rwsum, titlestring = "Peak rate")

prt_pr_out <- prt_pr +
  guides(alpha = F) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.0", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate", "png"),
  prt_pr_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pr <- plot_ensemble_performance(
  pkrate_rwsum,
  sl_pkrate_risktables,
  titlestring = "Peak rate"
)

mn_pkrate_lmin <- simsum[type == "sim.lmin", mean(peakrate)]

pkr_mnrisks <- simsum[
  type == "sim.lmin",
  .(risks = mean(abs(mn_pkrate_lmin - peakrate))),
  template
]

pkr_mnrisks[, mean(risks)]

pep_pr_out <- pep_pr +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.8,
    end = 0.4,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  guides(shape = FALSE) + 
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate", "png"),
  pep_pr_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkrate_lrnr_sel <- summarize_learner_selection(pkrate_rwsum)[order(-P)]
pkrate_lrnr_sel

fwrite(
  pkrate_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Peak-Rate", "csv")
)

pkrate_lrnr_sel[, lid := as.factor(lid)
                ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, shape = lclass)) +
#  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(size = 2, color = "maroon") +
  scale_shape_manual(values = c(0:5, 7, 12, 9, 13)) +
  # scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (peak rate)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "right"
  )


## Peak Week ---------------------------------------

### Data summary
pkweek_rwsum <- join_learner_stats(
  sl_pkweek_risktables,
  pkweek_weights
)

pkweek_rwsum

### Relabel randomForest learners
apply_rf_relabel(pkweek_rwsum)

### Risk tile plot
prt_pw <- plot_risktiles(pkweek_rwsum, titlestring = "Peak week")
prt_pw

prt_pw_out <- prt_pw +
  guides(alpha = F) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week", "png"),
  prt_pw_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

# sparkline dat
slpw_spark <- lapply(sl_pkweek_risktables, function(x) {
  x[learner == "SuperLearner", .(learner, mean_risk)]
}) %>% rbindlist(, idcol = "Week") %>%
  .[, Week := stringr::str_pad(Week, 2, "left", "0")]

setnames(slpw_spark, "learner", "ensemble_sl")

pkweek_rwsum %>%
  ggplot(aes(x = Week, y = log(mean_risk), group = lid)) +
  geom_line(size = 1) +
  facet_wrap(~lid) +
  geom_line(
    data = slpw_spark,
    aes(x = Week, y = log(mean_risk), group = ensemble_sl),
    linetype = "dashed",
    color = "red"
  ) +
  theme_tufte()


### Super Learner performance
pep_pw <- plot_ensemble_performance(
  pkweek_rwsum,
  sl_pkweek_risktables,
  titlestring = "Peak week"
)

pep_pw

pep_pw_out <- pep_pw +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week", "png"),
  pep_pw_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel <- summarize_learner_selection(pkweek_rwsum)
pkweek_lrnr_sel[, .N, lclass]

fwrite(
  pkweek_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Peak-Week", "csv")
)

pkweek_lrnr_sel[, lid := as.factor(lid)
  ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, fill = lclass)) +
  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (peak week)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "bottom"
  )


## Cumulative Hospitalizations --------------------

### Data summary
cumhosp_rwsum <- join_learner_stats(
  sl_cumhosp_risktables,
  cumhosp_weights
)

cumhosp_rwsum

### Relabel randomForest learners
apply_rf_relabel(cumhosp_rwsum)

### Risk tile plot
prt_ch <- plot_risktiles(
  cumhosp_rwsum,
  titlestring = "Cumulative hospitalizations"
)

prt_ch_out <- prt_ch +
  guides(alpha = F) +
  scale_size_continuous(
   name = "Weight",
   labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp", "png"),
  prt_ch_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch <- plot_ensemble_performance(
  cumhosp_rwsum,
  sl_cumhosp_risktables,
  titlestring = "Cumulative hospitalizations"
)

pep_ch

pep_ch_out <- pep_ch +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_Cum-Hosp", "png"),
  pep_ch_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel <- summarize_learner_selection(cumhosp_rwsum)

fwrite(
  cumhosp_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Cum-Hosp", "csv")
)


cumhosp_lrnr_sel[, lid := as.factor(lid)
                ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, fill = lclass)) +
  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (cumulative hospitalizations)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "bottom"
  )


# %% SENSITIVITY ANALYSES ------------------------------------------------------

## PEAK RATE -------------------------------------------

pkrate_risks_1se <- fmt_risk_table(
  dir = respr_1se,
  slug = "sl_pkrate",
  altslug = "1se"
)

pkrate_risks_1se

fwrite(
  pkrate_risks_1se,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-L1SE", "csv")
)

pkrate_weights_1se <- get_learner_weights(
  dir = respr_1se,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_1se <- join_learner_stats(
  risktables = sl_pkrate_risktables_1se,
  weights = pkrate_weights_1se
)

pkrate_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_1se)

### Risk tile plot
prt_pr_1se <- plot_risktiles(
  pkrate_rwsum_1se,
  titlestring = "Peak rate (alternate trend filter penalty)"
)

prt_pr_1se_out <- prt_pr_1se +
  guides(alpha = F) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate-L1SE", "png"),
  prt_pr_1se_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_pr_1se <- plot_ensemble_performance(
  pkrate_rwsum_1se,
  sl_pkrate_risktables_1se,
  titlestring = "Peak rate (alternate trend filter penalty)"
)

pep_pr_1se_out <- pep_pr_1se +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate-L1SE", "png"),
  pep_pr_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)



### Summarize learner selection
pkrate_lrnr_sel_1se <- summarize_learner_selection(pkrate_rwsum_1se)
pkrate_lrnr_sel_1se

fwrite(
  pkrate_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_1se.csv"))
)


## PEAK WEEK -------------------------------------------

pkweek_risks_1se <- fmt_risk_table(
  dir = respw_1se,
  slug = "sl_pkweek",
  altslug = "1se"
)

pkweek_risks_1se

fwrite(
  pkweek_risks_1se,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-L1SE", "csv")
  )
)

pkweek_weights_1se <- get_learner_weights(
  dir = respw_1se,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_1se <- join_learner_stats(
  risktables = sl_pkweek_risktables_1se,
  weights = pkweek_weights_1se
)

pkweek_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_1se)

### Risk tile plot
prt_pw_1se <- plot_risktiles(
  pkweek_rwsum_1se,
  titlestring = "Peak week (alternate trend filter penalty)"
)

prt_pw_1se_out <- prt_pw_1se +
  guides(alpha = F) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.0")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week-L1SE", "png"),
  prt_pw_1se_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pw_1se <- plot_ensemble_performance(
  pkweek_rwsum_1se,
  sl_pkweek_risktables_1se,
  titlestring = "Peak week (alternate trend filter penalty)"
)

pep_pw_1se_out <- pep_pw_1se +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week-L1SE", "png"),
  pep_pw_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel_1se <- summarize_learner_selection(pkweek_rwsum_1se)
pkweek_lrnr_sel_1se


## CUMULATIVE HOSPITALIZATIONS ----------------------------------------

cumhosp_risks_1se <- fmt_risk_table(
  dir = resch_1se,
  slug = "sl_cumhosp",
  altslug = "1se"
)

cumhosp_risks_1se

fwrite(
  cumhosp_risks_1se,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-L1SE", "csv"))
)

cumhosp_weights_1se <- get_learner_weights(
  dir = resch_1se,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_1se <- join_learner_stats(
  risktables = sl_cumhosp_risktables_1se,
  weights = cumhosp_weights_1se
)

cumhosp_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_1se)

### Risk tile plot
prt_ch_1se <- plot_risktiles(
  cumhosp_rwsum_1se,
  titlestring = "Cumulative hospitalizations (alternate trend filter penalty)"
)

prt_ch_1se_out <- prt_ch_1se +
  guides(alpha = F) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.20", "0.40", "0.60", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp-L1SE", "png"),
  prt_ch_1se_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch_1se <- plot_ensemble_performance(
  cumhosp_rwsum_1se,
  sl_cumhosp_risktables_1se,
  titlestring = "Cumulative hospitalizations (alternate trend filter penalty)"
)

pep_ch_1se_out <- pep_ch_1se +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_CumHosp-L1SE", "png"),
  pep_ch_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel_1se <- summarize_learner_selection(cumhosp_rwsum_1se)
cumhosp_lrnr_sel_1se

