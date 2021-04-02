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
## SENSITIVITY ANALYSIS: PEAK RATE, ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

erfslug <- "Elastic Net, Random Forest Learner Subset"

pkrate_risks_erf <- fmt_risk_table(
  dir = respr_erf,
  slug = "sl_pkrate",
  altslug = "erf"
)

pkrate_risks_erf

fwrite(
  pkrate_risks_erf,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-ERF", "csv")
)

pkrate_weights_erf <- get_learner_weights(
  dir = respr_erf,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_erf <- join_learner_stats(
  risktables = sl_pkrate_risktables_erf,
  weights = pkrate_weights_erf
)

pkrate_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_erf)

### Risk tile plot
prt_pr_erf <- plot_risktiles(
  pkrate_rwsum_erf,
  titlestring = paste("Peak rate:", erfslug)
)

prt_pr_erf_out <- prt_pr_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_erf_out

plotsave(
  name = "Risktiles_Peak-Rate-ERF",
  plot = prt_pr_erf_out,
  width = 12,
  height = 6.5
)


### Super Learner performance
pep_pr_erf <- plot_ensemble_performance(
  pkrate_rwsum_erf,
  sl_pkrate_risktables_erf,
  titlestring = paste("Peak rate:", erfslug)
)

## Median prediction risk.
pr_erf_medrisk <- pr_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_pr_erf_out <- pep_pr_erf +
  geom_hline(
    aes(yintercept = pr_erf_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_erf_out

plotsave(
  name = "Ensemble-Summary_Peak-Rate-ERF",
  plot = pep_pr_erf_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkrate_lrnr_sel_erf <- summarize_learner_selection(pkrate_rwsum_erf)
pkrate_lrnr_sel_erf

fwrite(
  pkrate_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_erf.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

pkweek_risks_erf <- fmt_risk_table(
  dir = respw_erf,
  slug = "sl_pkweek",
  altslug = "erf"
)

pkweek_risks_erf

fwrite(
  pkweek_risks_erf,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-ERF", "csv")
  )
)

pkweek_weights_erf <- get_learner_weights(
  dir = respw_erf,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_erf <- join_learner_stats(
  risktables = sl_pkweek_risktables_erf,
  weights = pkweek_weights_erf
)

pkweek_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_erf)

### Risk tile plot
prt_pw_erf <- plot_risktiles(
  pkweek_rwsum_erf,
  titlestring = paste("Peak week:", erfslug)
)

prt_pw_erf_out <- prt_pw_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.0")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_erf_out

plotsave(
  name = "Risktiles_Peak-Week-ERF",
  plot = prt_pw_erf_out,
  width = 12,
  height = 6.5
)

### Super Learner performance
pep_pw_erf <- plot_ensemble_performance(
  pkweek_rwsum_erf,
  sl_pkweek_risktables_erf,
  titlestring = paste("Peak week:", erfslug)
)

## Median prediction risk.
pw_erf_medrisk <- pw_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_pw_erf_out <- pep_pw_erf +
  geom_hline(
    aes(yintercept = pw_erf_medrisk, linetype = "Median prediction risk"),
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

pep_pw_erf_out

plotsave(
  name = "Ensemble-Summary_Peak-Week-ERF",
  plot = pep_pw_erf_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkweek_lrnr_sel_erf <- summarize_learner_selection(pkweek_rwsum_erf)
pkweek_lrnr_sel_erf

fwrite(
  pkweek_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_erf.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUM. HOSP., ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

cumhosp_risks_erf <- fmt_risk_table(
  dir = resch_erf,
  slug = "sl_cumhosp",
  altslug = "erf"
)

cumhosp_risks_erf

fwrite(
  cumhosp_risks_erf,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-ERF", "csv"))
)

cumhosp_weights_erf <- get_learner_weights(
  dir = resch_erf,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_erf <- join_learner_stats(
  risktables = sl_cumhosp_risktables_erf,
  weights = cumhosp_weights_erf
)

cumhosp_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_erf)

### Risk tile plot
prt_ch_erf <- plot_risktiles(
  cumhosp_rwsum_erf,
  titlestring = paste("Cumulative hospitalizations:", erfslug)
)

prt_ch_erf_out <- prt_ch_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_erf_out

plotsave(
  name = "Risktiles_Cum-Hosp-ERF",
  plot = prt_ch_erf_out,
  width = 12,
  height = 6.5
)


### Super Learner performance
pep_ch_erf <- plot_ensemble_performance(
  cumhosp_rwsum_erf,
  sl_cumhosp_risktables_erf,
  titlestring = paste("Cumulative hospitalizations:", erfslug)
)

## Median prediction risk.
ch_erf_medrisk <- ch_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_ch_erf_out <- pep_ch_erf +
  geom_hline(
    aes(yintercept = ch_erf_medrisk, linetype = "Median prediction risk"),
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

pep_ch_erf_out

plotsave(
  name = "Ensemble-Summary_CumHosp-ERF",
  plot = pep_ch_erf_out,
  width = 11,
  height = 8
)


### Summarize learner selection
cumhosp_lrnr_sel_erf <- summarize_learner_selection(cumhosp_rwsum_erf)
cumhosp_lrnr_sel_erf

fwrite(
  cumhosp_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_erf.csv"))
)
