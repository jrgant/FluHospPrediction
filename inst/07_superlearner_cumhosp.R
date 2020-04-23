# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))


# %% SUPER LEARNER (PEAK RATE) ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

current_week <- get_week(slurm = TRUE)

# pull the sl3 task for the current week
task <- suppressWarnings(
  fhp_make_task(
    "cumhosp",
    current_week = current_week
  )
)

# specify component learners and send to global environment
cat("\n\nLearners in Stack\n")
fhp_spec_learners(verbose = TRUE)

# specify meta learner
fhp_metalearner <- make_learner(
  Lrnr_nnls,
  convex = TRUE,
  metalearner_linear,
  loss_absolute_error
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
