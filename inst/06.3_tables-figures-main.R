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
## REFERENCE: SAVE DATA SET WITH NAIVE AND CV ENSEMBLE RISKS##
################################################################################

ensemble_risk_compare <- rbindlist(
  list(
    PeakRate = rbindlist(sl_pkrate_risktables),
    PeakWeek = rbindlist(sl_pkweek_risktables),
    CumHosp  = rbindlist(sl_cumhosp_risktables)
  ), idcol = "target"
)[learner %like% "^SuperLearner"
  ][, -c("coefficients")][, analysis := "LambdaMin"]

fwrite(
  ensemble_risk_compare,
  file.path(resdir, "Ensemble-Optimism-LambdaMin.csv")
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
prt_pr <- plot_risktiles(pkrate_rwsum, titlestring = "Peak rate") +
  theme_risktile_titles

prt_pr_out <- prt_pr +
  guides(alpha = FALSE) +
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
prt_pw <- plot_risktiles(pkweek_rwsum, titlestring = "Peak week") +
  theme_risktile_titles

prt_pw

prt_pw_out <- prt_pw +
  guides(alpha = FALSE) +
  theme(axis.text.y = element_text(hjust = 0.5))

plotsave(
  name = "Risktiles_Peak-Week",
  plot = prt_pw_out,
  width = 12,
  height = 6.5
)

# sparkline dat
slpw_spark <- lapply(sl_pkweek_risktables, function(x) {
  x[learner == "SuperLearnerCV", .(learner, mean_risk)]
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
  theme_tufte(base_size = 30)


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
) + theme_risktile_titles

prt_ch_out <- prt_ch +
  guides(alpha = FALSE) +
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
)[learner == "SuperLearnerCV"]

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
make_pep_main_all_panel <- function(targ, data = pep_main_wts_no_ch30,
                                    base_font_size = 10) {
  ylimits <- data[target == targ,
                  unlist(.(floor(min(log(mean_risk))),
                           ceiling(max(log(mean_risk)))))]
  plot <- data[target == targ] %>%
    ggplot(aes(x = Week, y = log(mean_risk))) +
    geom_point(
      aes(size = weight, color = "Component"),
      shape = 21
    ) +
    geom_hline(
      data = data.table(
        logmeanrisk = switch(targ,
          peakrate = pr_medrisk$log_mean_risk,
          peakweek = pw_medrisk$log_mean_risk,
          cumhosp = ch_medrisk$log_mean_risk
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
      data = pep_main_ens_no_ch30[target == targ],
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
    labs(x = "Week", y = "Log(Mean Prediction Risk)") +
    scale_color_manual(
      name = expression(underline(Prediction)),
      values = c("#bbbbbb", "black", "black")
    ) +
    scale_x_discrete(breaks = week_breaks) +
    scale_y_continuous(
      limits = ylimits,
      labels = scales::label_number(style_negative = "minus")
    ) +
    scale_size(
      name = expression(underline(Component~Weight)),
      breaks = c(0.2, 0.4, 0.6, 0.8)
    )

    if (targ == "cumhosp") {
      plot <- plot +
        guides(
          color = guide_legend(
            override.aes = list(
              shape = c(21, 21, NA),
              fill = c("white", "black", "black"),
              linetype = c(0, 0, 2)
            )
          )
        ) +
        scale_x_discrete(breaks = c(week_breaks[-length(week_breaks)], "29"))
    } else {
      plot <- plot +
        guides(color = F, size = F)
    }

  plot +
    theme_tufte(base_family = global_plot_font, base_size = base_font_size) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text = element_text(size = base_font_size),
      axis.title.x = element_text(
        margin = ggplot2::margin(t = 0.2, unit = "in")
      ),
      axis.title.y = element_text(
        margin = ggplot2::margin(r = 0.1, l = 0.1, unit = "in")
      ),
      axis.line = element_line(),
      plot.background = element_blank(),
      legend.box = "vertical",
      legend.spacing = unit(0.1, "in"),
      legend.position = c(0.27, 0.28),
      legend.title = element_text(hjust = 0.5),
      legend.text = element_text(size = base_font_size, lineheight = 0),
      legend.key.height = unit(10, "pt"),
      legend.box.background = element_rect(color = "black"),
      legend.box.margin = ggplot2::margin(0.05, 0.05, 0.05, 0.05, "in")
    )
}

pep_main_all_list <- list(
  peakrate = make_pep_main_all_panel("peakrate", base_font_size = 16),
  peakweek = make_pep_main_all_panel("peakweek", base_font_size = 16),
  cumhosp = make_pep_main_all_panel("cumhosp", base_font_size = 16)
)

pep_main_all <- cowplot::plot_grid(
  plotlist = pep_main_all_list,
  labels = paste0(LETTERS[1:3], ")"),
  label_fontface = "plain",
  label_size = pep_main_all_list[[1]]$theme$text$size,
  nrow = 1,
  hjust = 0.5,
  vjust = 0
) + theme(plot.margin = unit(rep(0.2, 4), "in"))

pep_main_all

plotsave(
  name = "Ensemble-Summary_All-Targets",
  plot = pep_main_all,
  width = 17.5,
  height = 6.45
)

## save individual panels
lapply(seq_along(pep_main_all_list), function(x) {
  plotsave(
    name = paste0("Ensemble-Summary_All-Targets-Panel-", LETTERS[x]),
    plot = pep_main_all_list[[x]],
    width = 17.5 / 3,
    height = 6.45
  )
})

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


################################################################################
## FIGURE: SCATTERPLOTS OF WEIGHT BY MEAN_RISK ##
################################################################################

plot_weight_coef <- function(data, titleslug) {
  ggplot(
    data[!(learner %like% "SuperLearner$|SuperLearnerCV")],
    aes(x = mean_risk, coefficients)
  ) +
    geom_vline(
      data = data[learner == "SuperLearnerCV"],
      aes(xintercept = mean_risk, color = "Ensemble risk"),
      linetype = "dashed"
    ) +
    geom_point(size = 0.75, alpha = 0.5) +
    facet_wrap(~ Week, scales = "free_x") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(values = "red") +
    ggtitle(titleslug) +
    theme_base(base_size = 12) +
    theme(legend.position = "bottom")
}

pr_scatter <- plot_weight_coef(
  rbindlist(sl_pkrate_risktables),
  "Peak rate (main analysis)"
)

pw_scatter <- plot_weight_coef(
  rbindlist(sl_pkweek_risktables),
  "Peak week (main_analysis)"
)

ch_scatter <- plot_weight_coef(
  rbindlist(sl_cumhosp_risktables),
  "Cumulative rate (main analysis)"
)

plotsave(
  name = "Scatter_WeightxRisk_PeakRate_LambdaMin",
  plot = pr_scatter,
  width = 12,
  height = 10
)

plotsave(
  name = "Scatter_WeightxRisk_PeakWeek_LambdaMin",
  plot = pw_scatter,
  width = 12,
  height = 10
)

plotsave(
  name = "Scatter_WeightxRisk_CumHosp_LambdaMin",
  plot = ch_scatter,
  width = 12,
  height = 10
)
