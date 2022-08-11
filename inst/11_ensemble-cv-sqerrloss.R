# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))

current_week   <- get_week(slurm = TRUE)
lambda_select  <- Sys.getenv("LAMBDA_SELECT")
learner_select <- Sys.getenv("LEARNER_SELECT")
target_select  <- Sys.getenv("TARGET_SELECT")
holdout_template <- as.numeric(Sys.getenv("HOLDOUT_TEMPLATE"))


# %% SUPER LEARNER ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

# pull the sl3 task for the current week
task <- suppressWarnings(
  fhp_make_task(
    target_select,
    current_week = current_week,
    lambda_type = lambda_select,
    holdout = holdout_template
  )
)

# specify component learners and send to global environment
cat("\n\nLearners in Stack\n")
fhp_spec_learners(
  learner_pat = learner_select,
  verbose = TRUE,
  currtask = task
)

# specify meta learner
fhp_metalearner <- make_learner(
  Lrnr_solnp,
  convex_combination = TRUE,
  learner_function = metalearner_linear,
  loss_function = loss_squared_error
)

# run the super learner algorithm
spec_output_dir <- paste0(
  "~/scratch/ArrayID-",
  Sys.getenv("SLURM_ARRAY_JOB_ID"), "_", Sys.getenv("SLURM_JOB_NAME"),
  "_HOLDOUT_", holdout_template
)

if (!file.exists(spec_output_dir)) dir.create(spec_output_dir)
cat("\n\n\n", "Output will be written to:", spec_output_dir, "\n\n\n")

ft <- fhp_run_sl(
  task,
  write = FALSE,
  returnobj = TRUE,
  results_path = spec_output_dir,
  current_week = current_week,
  metalearner = fhp_metalearner,
  output = "fit"
)

## predict on holdout set
simdat <- readRDS(
  here::here(
      "data/cleaned",
      paste0(
        "sim_dataset_analytic_lambda-min_week_",
        sprintf("%02d", current_week), ".Rds"
      )
  )
)

simdat[, template_numeric := as.numeric(template)]

hdat <- simdat[template_numeric == holdout_template]

holdout_task <- make_sl3_Task(
  data = hdat,
  covariates = names(task$X),
  outcome = target_select,
  outcome_type = "continuous"
)

holdout_pred <- list(
  holdout_season = hdat[, .N, .(template, template_numeric)],
  holdout_outcome = holdout_task$Y,
  sl_pred = ft$predict(holdout_task),
  sl_pred_abserr = abs(ft$predict(holdout_task) - holdout_task$Y)
)

saveRDS(
  holdout_pred,
  file = file.path(
    spec_output_dir,
    paste0(
      "holdout-", sprintf("%02d", holdout_template), "_",
      "week-", sprintf("%02d", current_week),
      "_pred_compare.Rds"
    )
  )
)

cat("WARNING LIST", rep("=", 60), "\n\n", sep = "")
warnings()

devtools::session_info()
