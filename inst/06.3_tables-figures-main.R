source(here::here("inst", "06.1_tables-figures-setup.R"))


################################################################################
## RISK TABLES ##
################################################################################

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


################################################################################
## TABLES: AVERAGE RISK BY WEEK, ACROSS COMPONENT MODELS ##
################################################################################

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


################################################################################
## SETUP: INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

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


################################################################################
## FIGURES: PEAK RATE, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

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
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.0", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_out

plotsave(
  name = "Risktiles_Peak-Rate",
  plot = prt_pr_out,
  width = 12,
  height = 6.5
)

### Super Learner performance
pep_pr <- plot_ensemble_performance(
  pkrate_rwsum,
  sl_pkrate_risktables,
  titlestring = "Peak rate"
)

pep_pr_out <- pep_pr +
  geom_hline(
    aes(
      yintercept = pr_medrisk$log_mean_risk,
      linetype = "Median benchmark risk"
    ),
    color = "red"
  ) +
  scale_linetype_manual(values = c("dashed"), name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.4,
    end = 0.4,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  guides(shape = FALSE) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_out

plotsave(
  name = "Ensemble-Summary_Peak-Rate",
  plot = pep_pr_out,
  width = 11,
  height = 8
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


################################################################################
## FIGURES: PEAK WEEK, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

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
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

plotsave(
  name = "Risktiles_Peak-Week",
  plot = prt_pw_out,
  width = 12,
  height = 6.5
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

## Median (naive) risk.
pkw_w1 <- readRDS(file.path(respw, "sl_pkweek_01.Rds"))
pkw_w1 <- pkw_w1$task$get_data()

pep_pw_out <- pep_pw +
  geom_hline(
    aes(
      yintercept = pw_medrisk$log_mean_risk,
      linetype = "Median prediction risk"
    ),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pw_out

plotsave(
  name = "Ensemble-Summary_Peak-Week",
  plot = pep_pw_out,
  width = 11,
  height = 8
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


################################################################################
## FIGURES: CUMULATIVE HOSPITALIZATIONS, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

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
  guides(alpha = FALSE) +
  scale_size_continuous(
   name = "Weight",
   labels = c("> 0.0", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_out

plotsave(
  name = "Risktiles_Cum-Hosp",
  plot = prt_ch_out,
  width = 12,
  height = 6.5
)


### Super Learner performance
pep_ch <- plot_ensemble_performance(
  cumhosp_rwsum,
  sl_cumhosp_risktables,
  titlestring = "Cumulative hospitalizations"
)

pep_ch

## Median prediction risk.
cmh_w1 <- readRDS(file.path(resch, "sl_cumhosp_01.Rds"))
cmh_w1 <- cmh_w1$task$get_data()

pep_ch_out <- pep_ch +
  geom_hline(
    aes(
      yintercept = ch_medrisk$log_mean_risk,
      linetype = "Median prediction risk"
    ),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_out

plotsave(
  name = "Ensemble-Summary_Cum-Hosp",
  plot = pep_ch_out,
  width = 11,
  height = 8
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


################################################################################
## COMBINED ENSEMBLE PERFORMANCE ##
################################################################################

## Bind component learner data.
pep_main_weights <- rbindlist(
  idcol = "target",
  list(
    peakrate = pep_pr_out$data,
    peakweek = pep_pw_out$data,
    cumhosp = pep_ch_out$data
  )
)

pep_main_weights

## Bind ensemble risk data and calculate 95% confidence limits.
pep_main_ens <- rbindlist(
  idcol = "target",
  list(
    peakrate = rbindlist(sl_pkrate_risktables),
    peakweek = rbindlist(sl_pkweek_risktables),
    cumhosp = rbindlist(sl_cumhosp_risktables)
  )
)[learner == "SuperLearner"]

pep_main_ens[, ":="(
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

pep_main_ens

## Bind together the risks of the naive predictions
pep_main_naive_log <- data.table(
  target = c("peakrate", "peakweek", "cumhosp"),
  naiverisk = c(
    pr_medrisk$log_mean_risk,
    pw_medrisk$log_mean_risk,
    ch_medrisk$log_mean_risk
  )
)

pep_main_naive <- data.table(
  target = c("peakrate", "peakweek", "cumhosp"),
  naiverisk = c(
    pr_medrisk$mean_risk,
    pw_medrisk$mean_risk,
    ch_medrisk$mean_risk
  )
)

## Make nice lables for each facet and set facet order.
facetlabs <- c(
  peakrate = "Peak rate",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations"
)

forder <- names(facetlabs)

pep_main_weights[, target := factor(target, levels = forder, ordered = TRUE)]
pep_main_ens[, target := factor(target, levels = forder, ordered = TRUE)]
pep_main_naive[, target := factor(target, levels = forder, ordered = TRUE)]

pep_main_wts_no_ch30 <- pep_main_weights[!(target == "cumhosp" & Week == "30")]
pep_main_ens_no_ch30 <- pep_main_ens[!(target == "cumhosp" & Week == "30")]


## Plot ensemble performance across all prediction targets (LOG SCALE).
pep_main_all <- pep_main_wts_no_ch30 %>%
  ggplot(aes(x = Week, y = log(mean_risk))) +
  geom_point(
    aes(size = weight, color = "Component"),
    shape = 21
  ) +
  geom_hline(
    data = data.table(
      logmeanrisk = c(
        pr_medrisk$log_mean_risk,
        pw_medrisk$log_mean_risk,
        ch_medrisk$log_mean_risk
      ),
      target = factor(forder, levels = forder)
    ),
    aes(
      yintercept = logmeanrisk,
      color = "Naive (median)"
    ),
    linetype = "dashed"
  ) +
  geom_pointrange(
    data = pep_main_ens_no_ch30,
    aes(
      x = Week, y = log(mean_risk),
      ymin = log(ll95), ymax = log(ul95),
      color = "Ensemble"
    ),
    size = 0.1,
    shape = 21,
    fill = "black",
    key_glyph = "pointrange"
  ) +
  facet_wrap(
    ~ target,
    ncol = 2,
    scales = "free_y",
    labeller = labeller(target = facetlabs)
  ) +
  labs(y = "Mean prediction risk (natural log scale)") +
  scale_color_manual(
    name = "Prediction",
    values = c("#dddddd", "black", "black")
  ) +
  scale_x_discrete(breaks = week_breaks) +
  scale_size(name = "Component weight") +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(21, 21, NA),
        fill = c("white", "black", "black"),
        linetype = c(0, 0, 2)
      ))) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 8),
    plot.background = element_blank(),
    panel.spacing = unit(0.5, "in"),
    legend.box = "vertical",
    legend.position = c(0.75, 0.25),
    legend.box.background = element_blank(),
  )

pep_main_all

plotsave(
  name = "Ensemble-Summary_All-Targets",
  plot = pep_main_all,
  width = 10,
  height = 10
)


## Plot ensemble performance across all prediction targets (STANDARD SCALE).
pep_main_all_rs <- pep_main_wts_no_ch30 %>%
  ggplot(aes(x = Week, y = mean_risk)) +
  geom_point(
    aes(size = weight, color = "Component"),
    shape = 21
  ) +
  geom_hline(
    data = data.table(
      logmeanrisk = c(
        pr_medrisk$mean_risk,
        pw_medrisk$mean_risk,
        ch_medrisk$mean_risk
      ),
      target = c("peakrate", "peakweek", "cumhosp")
    ),
    aes(
      yintercept = logmeanrisk,
      color = "Naive (median)"
    ),
    linetype = "dashed"
  ) +
  geom_pointrange(
    data = pep_main_ens_no_ch30,
    aes(x = Week, y = mean_risk,
        ymin = ll95, ymax = ul95,
        color = "Ensemble"
        ),
    size = 0.5,
    shape = 21,
    fill = "black",
    key_glyph = "pointrange"
  ) +
  facet_wrap(
    ~ target,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(target = facetlabs)
  ) +
  scale_color_manual(
    name = "Prediction",
    values = c("#aaaaaa", "black", "black")
  ) +
  scale_x_discrete(breaks = week_breaks) +
  scale_size(name = "Component weight") +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(21, 21, 22),
        fill = c("white", "black", "white"),
        linetype = c(0, 1, 0)
      ))) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 8)
  )

pep_main_all_rs

plotsave(
  name = "Ensemble-Summary_All-Targets_Reg-Scale-Components",
  plot = pep_main_all,
  width = 6,
  height = 10
)
