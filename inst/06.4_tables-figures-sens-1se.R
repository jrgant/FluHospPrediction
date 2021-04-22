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
## SENSITIVITY ANALYSIS: PEAK RATE, LAMBDA 1SE ##
################################################################################

l1se_slug <- "Alternate trend filter penalty"

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
  titlestring = paste("Peak rate:", l1se_slug)
)

prt_pr_1se_out <- prt_pr_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_1se_out

plotsave(
  name = "Risktiles_Peak-Rate-L1SE",
  plot = prt_pr_1se_out,
  width = 12,
  height = 6
)

### Super Learner performance
pep_pr_1se <- plot_ensemble_performance(
  pkrate_rwsum_1se,
  sl_pkrate_risktables_1se,
  titlestring = paste("Peak rate:", l1se_slug)
)

## Median prediction risk.
pr_1se_medrisk <- pr_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_pr_1se_out <- pep_pr_1se +
  geom_hline(
    aes(yintercept = pr_1se_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_1se_out

plotsave(
  name = "Ensemble-Summary_Peak-Rate-L1SE",
  plot = pep_pr_1se_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkrate_lrnr_sel_1se <- summarize_learner_selection(pkrate_rwsum_1se)
pkrate_lrnr_sel_1se

fwrite(
  pkrate_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_1se.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, LAMBDA 1SE ##
################################################################################

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
  titlestring = paste("Peak week:", l1se_slug)
)

prt_pw_1se_out <- prt_pw_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.0")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_1se_out

plotsave(
  name = "Risktiles_Peak-Week-L1SE",
  plot = prt_pw_1se_out,
  width = 12,
  height = 6.5
)

### Super Learner performance
pep_pw_1se <- plot_ensemble_performance(
  pkweek_rwsum_1se,
  sl_pkweek_risktables_1se,
  titlestring = paste("Peak week:", l1se_slug)
)

## Median prediction risk.
pw_1se_medrisk <- pw_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_pw_1se_out <- pep_pw_1se +
  geom_hline(
    aes(yintercept = pw_1se_medrisk, linetype = "Median prediction risk"),
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

plotsave(
  name = "Ensemble-Summary_Peak-Week-L1SE",
  plot = pep_pw_1se_out,
  width = 11,
  height = 8
)


### Summarize learner selection
pkweek_lrnr_sel_1se <- summarize_learner_selection(pkweek_rwsum_1se)
pkweek_lrnr_sel_1se

fwrite(
  pkweek_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_1se.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUMULATIVE HOSPITALIZATIONS, LAMBDA 1SE ##
################################################################################

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
  titlestring = paste("Cumulative hospitalizations:", l1se_slug)
)

prt_ch_1se_out <- prt_ch_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_1se_out

plotsave(
  name = "Risktiles_Cum-Hosp-L1SE",
  plot = prt_ch_1se_out,
  width = 12,
  height = 6.5
)


### Super Learner performance
pep_ch_1se <- plot_ensemble_performance(
  cumhosp_rwsum_1se,
  sl_cumhosp_risktables_1se,
  titlestring = paste("Cumulative hospitalizations:", l1se_slug)
)

## Median prediction risk.
ch_1se_medrisk <- ch_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_ch_1se_out <- pep_ch_1se +
  geom_hline(
    aes(yintercept = ch_1se_medrisk, linetype = "Median prediction risk"),
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

pep_ch_1se_out

plotsave(
  name = "Ensemble-Summary_CumHosp-L1SE",
  plot = pep_ch_1se_out,
  width = 11,
  height = 8
)


### Summarize learner selection
cumhosp_lrnr_sel_1se <- summarize_learner_selection(cumhosp_rwsum_1se)
cumhosp_lrnr_sel_1se

fwrite(
  cumhosp_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_1se.csv"))
)