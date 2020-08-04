# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))

current_week   <- get_week(slurm = TRUE)
lambda_select  <- Sys.getenv("LAMBDA_SELECT")
learner_select <- Sys.getenv("LEARNER_SELECT")
target_select  <- Sys.getenv("TARGET_SELECT")


# %% SUPER LEARNER ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

# pull the sl3 task for the current week
task <- suppressWarnings(
  fhp_make_task(
    target_select,
    current_week = current_week,
    lambda_type = lambda_select
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
  loss_function = loss_absolute_error
)

# run the super learner algorithm
spec_output_dir <- paste0(
  "results/ArrayID-",
  Sys.getenv("SLURM_ARRAY_JOB_ID"), "_",
  task$nodes$outcome
)

cat("\n\n\n", "Output will be written to:", spec_output_dir, "\n\n\n")

fhp_run_sl(
  task,
  write = TRUE,
  results_path = spec_output_dir,
  current_week = current_week,
  metalearner = fhp_metalearner,
  keep_extra = TRUE
)

cat("WARNING LIST", rep("=", 60), "\n\n", sep = "")
warnings()

devtools::session_info()
