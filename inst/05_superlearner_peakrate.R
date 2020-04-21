# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))


# %% SUPER LEARNER (PEAK RATE) ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

get_week <- function(w = NULL, slurm = TRUE) {
  
  if (is.null(w) & slurm) {
    as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  } else {
    w
  }
}

current_week <- get_week(4)

# pull the sl3 task for the current week
pkrate_task <- suppressWarnings(
  fhp_make_task(
    "pkrate",
    current_week = current_week
  )
)

# specify learners and send to global environment
cat("\n\nLearners in Stack\n")
fhp_spec_learners(learner_pat = "glm|lasso|mean", verbose = TRUE)

# run the super learner algorithm
fhp_run_sl(
  pkrate_task,
  write = FALSE,
  current_week = current_week,
  loss_fun = loss_squared_error
)

devtools::session_info()



