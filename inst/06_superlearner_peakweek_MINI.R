# %% LOAD AND SETUP -----------------------------------------------------------

suppressMessages(library(FluHospPrediction))


# %% SUPER LEARNER (PEAK RATE) ------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

current_week <- get_week(7)

# pull the sl3 task for the current week
task <- suppressWarnings(
  fhp_make_task(
    "pkweek",
    current_week = current_week
  )
)

# specify component learners and send to global environment
## cat("\n\nLearners in Stack\n")

## lrnr_testgam <- Lrnr_gam$new(select = TRUE, fit = FALSE, gamma = 0.1)

## fl <- paste("pkweek ~",
##             sapply(task$nodes$covariates, function(x) paste0("s(", x, ")")) %>% paste(., collapse = " + ")
##             )

## t <- mgcv::gam(
##   pkweek ~ s(hosprate_100k_7) + s(cumhosp_100k_7),
##   data = task$get_data(),
##   by = template_numeric
## )

## tp <- predict(t)
## tpb <- cbind(task$get_data(), tp)
## tpb[, plot(hosprate_100k_1, tp)]
## tpb[, plot(cumhosp_100k_1, tp)]

## tg <- Lrnr_gam$new(
##   formula = as.formula(paste("pkweek ~ s(hosprate_100k_7) + s(cumhosp_100k_7)"))
## )

## tg$train(task)
## tcv <- Lrnr_cv$new(tg)
## tcv$train(task)


fhp_spec_learners(
  learner_pat = "glm|mean",
  gamweek = current_week,
  currtask = task,
  verbose = TRUE
)

# specify meta learner
fhp_metalearner <- make_learner(
  Lrnr_solnp,
  convex_combination = TRUE,
  learner_function = metalearner_linear,
  loss_function = FluHospPrediction::loss_absolute_error
)

# run the super learner algorithm

spec_output_dir_oscar <- paste0(
  "results/ArrayID-",
  Sys.getenv("SLURM_ARRAY_JOB_ID"), "_",
  task$nodes$outcome
)

spec_output_dir_local <- paste0(
  "results/testrun_solnp_metalearner"
)

cat(
  "\n\n\n",
  "Output will be written to:",
  spec_output_dir_oscar,
  "\n\n\n",
  sep = ""
)

fhp_run_sl(
  task,
  write = FALSE,
  results_path = spec_output_dir_local,
  current_week = current_week,
  metalearner = fhp_metalearner,
  keep_extra = TRUE
)

cat("WARNING LIST", rep("=", 60), "\n\n")
warnings()

devtools::session_info()
