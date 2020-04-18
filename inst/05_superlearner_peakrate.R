# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))


# %% SUPER LEARNER (PEAK RATE) ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

current_week <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# pull the sl3 task for the current week
pkrate_task <- suppressWarnings(
  fhp_make_tasks("pkrate", current_week = current_week)
)

# specify learners and send to global environment
fhp_spec_learners()

# run the super learner algorithm
fhp_run_sl(
  pkrate_task,
  write = TRUE,
  current_week = current_week
)

devtools::session_info()


