# %% LOAD AND SETUP ------------------------------------------------------------

suppressMessages(library(FluHospPrediction))


# %% SUPER LEARNER (PEAK RATE) --------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to clustering

pkrate_tasks <- suppressWarnings(fhp_make_tasks("pkrate"))
fhp_spec_learners()
fhs_run_sl(pkrate_tasks, write = TRUE)
