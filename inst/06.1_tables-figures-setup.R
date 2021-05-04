################################################################################
## LOAD ##
################################################################################

pacman::p_load(
  FluHospPrediction,
  data.table,
  extrafont
)

## Data directories.
clndir <- here::here("data", "cleaned")
paper_output <- here::here("results", "00_paper_output")

## Slugs for table, figure, and value filenames.
figslug <- "FIG"
tabslug <- "TAB"
valslug <- "VAL"

## This function formats filenames during save operations, using both the
## slugs above and the current date. If one file is updated, make sure to
## update all files, as the manuscript Rmd selects files to use based on
## a global date string.
nicefile <- function(slug, description, ext, date = Sys.Date(),
                     dir = paper_output) {
  file.path(dir, paste0(slug, "_", description, "_", date, ".", ext))
}

## Load font database.
loadfonts(device = "pdf")
global_plot_font <- "Gentium Book Basic"

## Universal breaks for week labeling in plots.
week_breaks <- c("01", "05", "10", "15", "20", "25", "30")

## This function saves plots in pdf and png formats.
plotsave <- function(name, plot, width, height) {

  ggsave(
    nicefile(figslug, name, "pdf"),
    plot,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf
  )

  ggsave(
    nicefile(figslug, name, "png"),
    plot,
    width = width,
    height = height,
    units = "in",
    device = "png",
    dpi = 1200
  )

}


################################################################################
## RESULTS DIRECTORIES ##
################################################################################

resdir <- "results"

## Main analysis files (NoLOESS)
respr <- file.path(resdir, "PeakRate-LambdaMin")
respw <- file.path(resdir, "PeakWeek-LambdaMin")
resch <- file.path(resdir, "CumHosp-LambdaMin")

## Sensitivity analysis files (alternative trend filter lambda)
respr_1se <- file.path(resdir, "PeakRate-LambdaSE")
respw_1se <- file.path(resdir, "PeakWeek-LambdaSE")
resch_1se <- file.path(resdir, "CumHosp-LambdaSE")

## Sensitivity analysis files (elastic net, random forest subset)
respr_erf <- file.path(resdir, "PeakRate-ElastNetRF")
respw_erf <- file.path(resdir, "PeakWeek-ElastNetRF")
resch_erf <- file.path(resdir, "CumHosp-ElastNetRF")

## Sensitivity analysis files (squared error loss)
respr_sqe <- file.path(resdir, "PeakRate-SqErrLoss")
respw_sqe <- file.path(resdir, "PeakWeek-SqErrLoss")
resch_sqe <- file.path(resdir, "CumHosp-SqErrLoss")

## Simulated curves
sim_lm <- readRDS(file.path(clndir, "hypothetical-curves_lambda-min.Rds"))
sim_ls <- readRDS(file.path(clndir, "hypothetical-curves_lambda-1se.Rds"))


################################################################################
## LEARNER NAME LOOKUP TABLE ##
################################################################################

# Learner lookup table (get submitted learners from a task)
lid07 <- readRDS(file.path(respr, "sl_pkrate_07.Rds"))

lchar <- data.table(
  lname = names(lid07$sl_pruned$metalearner_fit$coefficients)
)

# Relabel the random forests
relabel_rf(lchar)

# Assign IDs to the component learners and create lookup table
lchar[
  grepl("glmnet.*(0\\.25|0\\.5|0\\.75)", lname),
  lid := paste0("EN", 1:max(.I))
]

lchar[
  grepl("glmnet.*1_100", lname),
  lid := "LASSO"
]

lchar[
  grepl("loess", lname),
  lid := paste0("LOESS", 1:max(.I))
]

lchar[
  grepl("nnet", lname),
  lid := paste0("NNet", str_pad(1:max(.I), 2, "left", "0"))
]

lchar[
  grepl("polspline", lname),
  lid := paste0("PMARS", 1:max(.I))
]

lchar[
  grepl("randomForest", lname),
  lname := relabel_rf(lchar)
]

lchar[
  grepl("randomForest", lname),
  lid := paste0("RF", str_pad(1:max(.I), 2, "left", "0"))
]

lchar[
  grepl("glmnet.*0_100", lname),
  lid := "Ridge"
]

lchar[
  grepl("Pipeline", lname),
  lid := "ScreenGLM"
]

lchar[
  grepl("svm", lname),
  lid := paste0("SVM", 1:max(.I))
][]

lchar


################################################################################
## Median Prediction Risks ##
################################################################################

est_cv_risk_naive <- function(data, outcome, loss = c("abs", "sqe")) {
  if (loss == "abs") {
    fold_risks <- sapply(data[, unique(template)], function(.x) {
      mean(abs(
        data[template != .x, median(get(outcome))] -
        data[template == .x, get(outcome)]
      ))
    })
  } else {
    fold_risks <- sapply(data[, unique(template)], function(.x) {
      mean((
        data[template != .x, mean(get(outcome))] -
        data[template == .x, get(outcome)]
      )^2)
    })
  }

  mean_risk <- mean(fold_risks)
  log_mean_risk <- log(mean_risk)

  list(
    fold_risks = fold_risks,
    mean_risk = mean_risk,
    log_mean_risk = log_mean_risk
  )
}

## LambdaMin
lmin_sim <-
  fread(here::here("data", "cleaned", "sim_dataset_wide_lambda-min.csv"))

pr_medrisk <- est_cv_risk_naive(lmin_sim, "pkrate", "abs")
pw_medrisk <- est_cv_risk_naive(lmin_sim, "pkweek", "abs")
ch_medrisk <- est_cv_risk_naive(lmin_sim, "cumhosp", "abs")

pr_mnrisk <- est_cv_risk_naive(lmin_sim, "pkrate", "sqe")
pw_mnrisk <- est_cv_risk_naive(lmin_sim, "pkweek", "sqe")
ch_mnrisk <- est_cv_risk_naive(lmin_sim, "cumhosp", "sqe")


## LambdaSE
lmin_1se <-
  fread(here::here("data", "cleaned", "sim_dataset_wide_lambda-1se.csv"))

pr_medrisk_1se <- est_cv_risk_naive(lmin_1se, "pkrate", "abs")
pw_medrisk_1se <- est_cv_risk_naive(lmin_1se, "pkweek", "abs")
ch_medrisk_1se <- est_cv_risk_naive(lmin_1se, "cumhosp", "abs")
