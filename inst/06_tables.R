# LOAD -------------------------------------------------------------------------

pacman::p_load(
  FluHospPrediction,
  data.table
)

clndir <- here::here("data", "cleaned")
paper_output <- here::here("results", "paper_output")


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

fwrite(targets, file.path(paper_output, "table-01_prediction-targets.csv"))


# %% RESULTS DIRECTORIES -------------------------------------------------------

resdir <- "results"

## Main analysis files
respr <- file.path(resdir, "ArrayID-12980667-Target-PeakRate-Lambda-Min")
respw <- file.path(resdir, "ArrayID-12980671-Target-PeakWeek-Lambda-Min")
resch <- file.path(resdir, "ArrayID-12980674-Target-CumHosp-Lambda-Min")

## Sensitivity analysis files (alternative trend filter lambda)
respr_1se <- file.path(resdir, "ArrayID-12980676-Target-PeakRate-Lambda-1SE")
respw_1se <- file.path(resdir, "ArrayID-12980677-Target-PeakWeek-Lambda-1SE")


# %% LEARNER NAME LOOKUP TABLE ------------------------------------------------

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


# TABLE TWO: Parameter Targets -------------------------------------------------

## No analysis output, just LaTeX equations. See manuscript file.


# TABLE THREE: Component Model Summary -----------------------------------------

## No analysis output. Typed summary of component models and tuning parameters.


# TABLES 4-6: Risk Tables for Prediction Targets -------------------------------

## Table 4: Peak Rate Risks ----------------------------------------------------

pkrate_risks <- fmt_risk_table(dir = respr, slug = "sl_pkrate")
pkrate_risks

fwrite(
  pkrate_risks,
  file.path(paper_output, paste0(Sys.Date(), "_table-04_peakrate-risks.csv"))
)


## Table 5: Peak Week Risks ----------------------------------------------------

pkweek_risks <- fmt_risk_table(dir = respw, slug = "sl_pkweek")
pkweek_risks

fwrite(
  pkweek_risks,
  file.path(paper_output, paste0(Sys.Date(), "_table-05_peakweek_risks.csv"))
)


## Table 6: Cumulative Hospitalizations Risks ----------------------------------

cumhosp_risks <- fmt_risk_table(dir = resch, slug = "sl_cumhosp")
cumhosp_risks

fwrite(
  cumhosp_risks,
  file.path(paper_output, paste0(Sys.Date(), "_table-06_cumhosp_risks.csv"))
)


# S2 TABLE: Curves by Season Template ------------------------------------------

sim_lr <- readRDS(file.path(clndir, "hypothetical-curves.Rds"))
sim_lm <- readRDS(file.path(clndir, "hypothetical-curves_lambda-min.Rds"))
sim_ls <- readRDS(file.path(clndir, "hypothetical-curves_lambda-1se.Rds"))

pretrans_sims <- lapply(1:length(sim_lm$hc), function(x) {
  cid <- rep(x, 30)
  original_sim <- unname(unlist(sim_lm$hc[[x]]$eq$fi_pluserr))
  data.table(cid = cid, weekint = 1:30, original_sim)
}) %>% rbindlist

setnames(pretrans_sims, "V1", "origsim")
pretrans_sims

sims <- sim_ls$outhc

tmpct <- sims[weekint == 1, .N, keyby = .(`Template season` = template)
              ][, N := format(N, big.mark = ",")]

fwrite(tmpct, file.path(resdir, "table-s02_template-counts.csv"))

# Reported in methods: proportion of weeks negative and set to 0

transcomp <- sims[pretrans_sims, on = c("cid", "weekint")]

transcomp[, .N, .(origsim < 0)][, P := N / sum(N)][]


# S3 FIGURE: Curves by Season Template -----------------------------------------

# compare curves by lambda
simlist <- list(
  lambda.rand = sim_lr$outhc,
  lambda.min = sim_lm$outhc,
  lambda.1se = sim_ls$outhc
)

simdt <- rbindlist(simlist, idcol = "lambda")

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
  file = "results/paper_output/tempsim_plot.pdf",
  dev = "pdf"
)


# S4-S6 TABLES: Average Risk by Week, Across Component Model -------------------

## Table S4 -------------

riskdist_pkrate <- get_risk_dist("sl_pkrate")
riskdist_pkrate

fwrite(
  riskdist_pkrate,
  file.path(
    paper_output, paste0(Sys.Date(), "_table-s04_risk-distbyweek-pkrate.csv")
  )
)


## Table S5 -------------

riskdist_pkweek <- get_risk_dist("sl_pkweek")
riskdist_pkweek

fwrite(
  riskdist_pkweek,
  file.path(
    paper_output,
    paste0(Sys.Date(), "_table-s05_risk-distbyweek-pkweek.csv")
  )
)

## Table S6 -------------

riskdist_cumhosp <- get_risk_dist("sl_cumhosp")
riskdist_cumhosp

fwrite(
  riskdist_cumhosp,
  file.path(
    paper_output,
    paste0(Sys.Date(), "_table-s06_risk-distbyweek-cumhosp.csv")
  )
)


# S7-S9 FIGURES: Inspect Weight Assignments ------------------------------------

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

### Super Learner performance
pep_pr <- plot_ensemble_performance(
  pkrate_rwsum,
  sl_pkrate_risktables,
  titlestring = "Peak rate"
)

### Summarize learner selection
pkrate_lrnr_sel <- summarize_learner_selection(pkrate_rwsum)
pkrate_lrnr_sel

fwrite(
  pkrate_lrnr_sel,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection.csv"))
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

### Super Learner performance
pep_pw <- plot_ensemble_performance(
  pkweek_rwsum,
  sl_pkweek_risktables,
  titlestring = "Peak week"
)

### Summarize learner selection
pkweek_lrnr_sel <- summarize_learner_selection(pkweek_rwsum)

fwrite(
  pkweek_lrnr_sel,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection.csv"))
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

### Super Learner performance
pep_ch <- plot_ensemble_performance(
  cumhosp_rwsum,
  sl_cumhosp_risktables,
  titlestring = "Cumulative hospitalizations"
)

### Summarize learner selection
cumhosp_lrnr_sel <- summarize_learner_selection(cumhosp_rwsum)

fwrite(
  cumhosp_lrnr_sel,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection.csv"))
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
  file.path(paper_output, paste0(Sys.Date(), "peakrate-risks_lambda-1se.csv"))
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

### Super Learner performance
pep_pr_1se <- plot_ensemble_performance(
  pkrate_rwsum_1se,
  sl_pkrate_risktables_1se,
  titlestring = "Peak rate (alternate trend filter penalty)"
)

### Summarize learner selection
pkrate_lrnr_sel_1se <- summarize_learner_selection(pkrate_rwsum_1se)
pkrate_lrnr_sel_1se

fwrite(
  pkrate_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_1se.csv"))
)


## PEAK WEEK -------------------------------------------

pkweek_risks_1se <- fmt_risk_table(dir = respw_1se, slug = "sl_pkweek")
pkweek_risks_1se

fwrite(
  pkweek_risks_1se,
  file.path(paper_output, paste0(Sys.Date(), "peakweek_risks_1se.csv"))
)

pkweek_weights <- get_learner_weights(
  dir = respw,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)


## CUMULATIVE HOSPITALIZATIONS ----------------------------------------

cumhosp_risks_1se <- fmt_risk_table(dir = resch_1se, slug = "sl_cumhosp")
cumhosp_risks_1se

fwrite(
  cumhosp_risks_1se,
  file.path(paper_output, paste0(Sys.Date(), "cumhosp_risks_1se.csv"))
)
