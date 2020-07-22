# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))

target <- "pkrate"
current_week <- 5
ltype <- "lambda-min"

# %% SUPER LEARNER (PEAK RATE) ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

task <- suppressWarnings(
  fhp_make_task(
    target,
    current_week = current_week,
    lambda_type = ltype
  )
)

# specify component learners and send to global environment
cat("\n\nLearners in Stack\n")
fhp_spec_learners(
  learner_pat = "elastnet|rf",
  verbose = TRUE,
  currtask = task
)

# specify meta learner
fhp_metalearner <- make_learner(
  Lrnr_solnp,
  convex = TRUE,
  metalearner_linear,
  loss_absolute_error
)

# run the super learner algorithm
fhpl1 <- fhp_run_sl(
  task,
  write = FALSE,
  current_week = current_week,
  metalearner = fhp_metalearner
)

devtools::session_info()
