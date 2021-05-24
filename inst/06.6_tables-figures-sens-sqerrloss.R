source(here::here("inst", "06.1_tables-figures-setup.R"))

simdist_files <- list.files(
  here::here("results", "00_paper_output"),
  pattern = "TAB_Simulated",
  full.names = TRUE
)

pr_dist <- fread(simdist_files[grep("PeakRate", simdist_files)])
pw_dist <- fread(simdist_files[grep("PeakWeek", simdist_files)])
ch_dist <- fread(simdist_files[grep("CumHosp", simdist_files)])


################################################################################
## SENSITIVITY ANALYSIS: PEAK RATE, SQUARED ERROR LOSS ##
################################################################################

sqeslug <- "Squared Error Loss"

pkrate_risks_sqe <- fmt_risk_table(
  dir = respr_sqe,
  slug = "sl_pkrate",
  altslug = "sqe"
)

pkrate_risks_sqe

fwrite(
  pkrate_risks_sqe,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-SQE", "csv")
)

pkrate_weights_sqe <- get_learner_weights(
  dir = respr_sqe,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_sqe <- join_learner_stats(
  risktables = sl_pkrate_risktables_sqe,
  weights = pkrate_weights_sqe
)

pkrate_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_sqe)

### Risk tile plot
prt_pr_sqe <- plot_risktiles(
  pkrate_rwsum_sqe,
  titlestring = paste("Peak rate:", sqeslug)
)

prt_pr_sqe_out <- prt_pr_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_sqe_out

plotsave(
  name = "Risktiles_Peak-Rate-SQE",
  plot = prt_pr_sqe_out,
  width = 12,
  height = 4.5
)


### Super Learner performance
pep_pr_sqe <- plot_ensemble_performance(
  pkrate_rwsum_sqe,
  sl_pkrate_risktables_sqe,
  titlestring = paste("Peak rate:", sqeslug)
)

pep_pr_sqe_out <- pep_pr_sqe +
  geom_hline(
    aes(
      yintercept = pr_mnrisk$log_mean_risk,
      linetype = "Mean prediction risk"
    ),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_sqe_out

plotsave(
  name = "Ensemble-Summary_Peak-Rate-SQE",
  plot = pep_pr_sqe_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkrate_lrnr_sel_sqe <- summarize_learner_selection(pkrate_rwsum_sqe)
pkrate_lrnr_sel_sqe

fwrite(
  pkrate_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_sqe.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, SQUARED ERROR LOSS ##
################################################################################

sqe_slug <- "Squared error loss"

pkweek_risks_sqe <- fmt_risk_table(
  dir = respw_sqe,
  slug = "sl_pkweek",
  altslug = "sqe"
)

pkweek_risks_sqe

fwrite(
  pkweek_risks_sqe,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-SQE", "csv")
  )
)

pkweek_weights_sqe <- get_learner_weights(
  dir = respw_sqe,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_sqe <- join_learner_stats(
  risktables = sl_pkweek_risktables_sqe,
  weights = pkweek_weights_sqe
)

pkweek_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_sqe)

### Risk tile plot
prt_pw_sqe <- plot_risktiles(
  pkweek_rwsum_sqe,
  titlestring = paste("Peak week:", sqeslug)
)

prt_pw_sqe_out <- prt_pw_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_sqe_out

plotsave(
  name = "Risktiles_Peak-Week-SQE",
  plot = prt_pw_sqe_out,
  width = 12,
  height = 4.5
)

### Super Learner performance
pep_pw_sqe <- plot_ensemble_performance(
  pkweek_rwsum_sqe,
  sl_pkweek_risktables_sqe,
  titlestring = paste("Peak week:", sqe_slug)
)

pep_pw_sqe_out <- pep_pw_sqe +
  geom_hline(
    aes(yintercept = pw_mnrisk$log_mean_risk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pw_sqe_out

plotsave(
  name = "Ensemble-Summary_Peak-Week-SQE",
  plot = pep_pw_sqe_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkweek_lrnr_sel_sqe <- summarize_learner_selection(pkweek_rwsum_sqe)
pkweek_lrnr_sel_sqe

fwrite(
  pkweek_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_sqe.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUMULATIVE HOSPITALIZATIONS, SQUARED ERROR LOSS ##
################################################################################

cumhosp_risks_sqe <- fmt_risk_table(
  dir = resch_sqe,
  slug = "sl_cumhosp",
  altslug = "sqe"
)

cumhosp_risks_sqe

fwrite(
  cumhosp_risks_sqe,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-SQE", "csv"))
)

cumhosp_weights_sqe <- get_learner_weights(
  dir = resch_sqe,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_sqe <- join_learner_stats(
  risktables = sl_cumhosp_risktables_sqe,
  weights = cumhosp_weights_sqe
)

cumhosp_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_sqe)

### Risk tile plot
prt_ch_sqe <- plot_risktiles(
  cumhosp_rwsum_sqe,
  titlestring = paste("Cumulative hospitalizations:", sqeslug)
)

prt_ch_sqe_out <- prt_ch_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_sqe_out

plotsave(
  name = "Risktiles_Cum-Hosp-SQE",
  plot = prt_ch_sqe_out,
  width = 12,
  height = 4.5
)


### Super Learner performance
pep_ch_sqe <- plot_ensemble_performance(
  cumhosp_rwsum_sqe,
  sl_cumhosp_risktables_sqe,
  titlestring = paste("Cumulative hospitalizations:", sqeslug)
)

pep_ch_sqe_out <- pep_ch_sqe +
  geom_hline(
    aes(
      yintercept = ch_mnrisk$log_mean_risk,
      linetype = "Mean prediction risk"
    ),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_sqe_out

plotsave(
  name = "Ensemble-Summary_CumHosp-SQE",
  plot = pep_ch_sqe_out,
  width = 11,
  height = 8
)


### Summarize learner selection
cumhosp_lrnr_sel_sqe <- summarize_learner_selection(cumhosp_rwsum_sqe)
cumhosp_lrnr_sel_sqe

fwrite(
  cumhosp_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_sqe.csv"))
)


################################################################################
## COMBINED ENSEMBLE SUMMARY PLOT: SQUARED ERROR LOSS ##
################################################################################

## Bind component learner data.
pep_sqe_weights <- rbindlist(
  idcol = "target",
  list(
    peakrate = pep_pr_sqe_out$data,
    peakweek = pep_pw_sqe_out$data,
    cumhosp = pep_ch_sqe_out$data
  )
)

pep_sqe_weights

## Bind ensemble risk data and calculate 95% confidence limits.
pep_sqe_ens <- rbindlist(
  idcol = "target",
  list(
    peakrate = rbindlist(sl_pkrate_risktables_sqe),
    peakweek = rbindlist(sl_pkweek_risktables_sqe),
    cumhosp = rbindlist(sl_cumhosp_risktables_sqe)
  )
)[learner == "SuperLearner"]

pep_sqe_ens[, ":="(
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

pep_sqe_ens

## Bind together the risks of the naive predictions
pep_sqe_naive <- data.table(
  target = c("peakrate", "peakweek", "cumhosp"),
  naiverisk = c(
    pr_mnrisk$log_mean_risk,
    pw_mnrisk$log_mean_risk,
    ch_mnrisk$log_mean_risk
  )
)

## Make nice lables for each facet and set facet order.
facetlabs <- c(
  peakrate = "Peak rate",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations"
)

forder <- names(facetlabs)

pep_sqe_weights[, target := factor(target, levels = forder, ordered = TRUE)]
pep_sqe_ens[, target := factor(target, levels = forder, ordered = TRUE)]
pep_sqe_naive[, target := factor(target, levels = forder, ordered = TRUE)]

pep_sqe_weights_no_ch30 <- pep_sqe_weights[!(target == "cumhosp" & Week == "30")]
pep_sqe_ens_no_ch30 <- pep_sqe_ens[!(target == "cumhosp" & Week == "30")]

pep_sqe_all <- pep_sqe_weights_no_ch30 %>%
  ggplot(aes(x = Week, y = log(mean_risk))) +
  geom_point(
    aes(size = weight, color = "Component"),
    shape = 21
  ) +
  geom_pointrange(
    data = pep_sqe_ens_no_ch30,
    aes(x = Week, y = log(mean_risk),
        ymin = log(ll95), ymax = log(ul95),
        color = "Ensemble"
        ),
    size = 0.3,
    shape = 21,
    fill = "white"
  ) +
  geom_hline(
    data = pep_sqe_naive,
    aes(
      yintercept = naiverisk,
      color = "Mean predictor"
    ),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ target,
    ncol = 2,
    scales = "free_y",
    labeller = labeller(target = facetlabs)
  ) +
  labs(y = "Mean prediction risk (natural log scale)") +
  scale_color_manual(
    name = "Prediction source",
    values = c("#dddddd", "#990000", "black")
  ) +
  scale_x_discrete(breaks = week_breaks) +
  scale_size(name = "Weight") +
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
    legend.box.background = element_blank()
  )

pep_sqe_all

plotsave(
  name = "Ensemble-Summary_All-Targets-SQE",
  plot = pep_sqe_all,
  width = 10,
  height = 10
)
