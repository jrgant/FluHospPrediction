# %% LOAD AND SETUP ------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

# code to run while testing
## Sys.setenv(
## "SLURM_ARRAY_TASK_ID" = 5,
## "LAMBDA_SELECT" = "lambda-min",
## "LEARNER_SELECT" = "glm|lasso|ridge",
## "TARGET_SELECT" = "pkrate",
## "OBS_SEASON" = "2015-16"
## )

current_week   <- get_week(slurm = TRUE)
lambda_select  <- Sys.getenv("LAMBDA_SELECT")
learner_select <- Sys.getenv("LEARNER_SELECT")
target_select  <- Sys.getenv("TARGET_SELECT")
obs_season     <- Sys.getenv("OBS_SEASON")

# %% SUPER LEARNER -------------------------------------------------------------

# ignore leave-one-out CV warning: specification intended due to- clustering

# pull the standard sl3 task for the current week
task <- suppressWarnings(
  fhp_make_task(
    target_select,
    current_week = current_week,
    lambda_type = lambda_select,
    prosp = obs_season
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
  "~/scratch/ArrayID-",
  Sys.getenv("SLURM_ARRAY_JOB_ID"), "_", Sys.getenv("SLURM_JOB_NAME"),
  "_PROSP-s", obs_season
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
  output = "fit",
  set_keep_extra = TRUE
)

# Format observed season of interest and predict on data -----------------------

# Format the selected empirical season and return an sl3 task so we can predict
# on it.

# This function is adapted from code in inst/03_create_analysis_dataset.R
# that formats the simulated curves.
format_empirical_season <- function(week, predict_on, origtask = task) {

  dt <- fread(here::here("data", "cleaned", "empdat.csv"))

  # get the naive predictions from seasons prior to the current season
  # designated in predict_on
  md <- dt[
    as.numeric(substring(season, 1, 4)) <
    as.numeric(substring(predict_on, 1, 4))
  ]

  md_extract <- md[, .(
    pkrate = max(weekrate),
    # if season has peak rate that's the same in more than one week, we take the
    # average the indicated weeks
    pkweek = mean(weekint[weekrate == max(weekrate)]),
    cumhosp = cumrates[weekint == 30]
  ), by = season]

  md_preds <- md_extract[, .(
    pkrate = median(pkrate),
    pkweek = median(pkweek),
    cumhosp = median(cumhosp)
  )]

  print(md_extract)
  print(md_preds)

  sub <- dt[season == predict_on][, .(
             weekint, hosprate_100k = weekrate, cumhosp_100k = cumrates
           )]

  sub_obs_targets <- sub[, .(
    pkrate = hosprate_100k[hosprate_100k == max(hosprate_100k)],
    pkweek = weekint[hosprate_100k == max(hosprate_100k)],
    cumhosp = cumhosp_100k[weekint == 30]
  )]

  pdat <- dcast(
    ... ~ weekint,
    data = sub,
    value.var = list("hosprate_100k", "cumhosp_100k"),
    subset = .(weekint <= week)
  )[, -c(".")]

  fmtdat <- cbind(sub_obs_targets, pdat)

  # create variables for lagged differences
  if (week <= 5 & week > 1) {

    diffcols_hosprate <- paste0("diff_hosprate_lag", 1:(week - 1))
    diffcols_cumhosp <- paste0("diff_cumhosp_lag", 1:(week - 1))

    for (i in 1:(week - 1)) {

      fmtdat[, diffcols_hosprate[i] := (get(paste0("hosprate_100k_", week)) -
                                     get(paste0("hosprate_100k_", week - i))
      )]

      fmtdat[, diffcols_cumhosp[i] := (get(paste0("cumhosp_100k_", week)) -
                                    get(paste0("cumhosp_100k_", week - i))
      )]
    }

  } else if (week > 5) {

    diffcols_hosprate <- paste0("diff_hosprate_lag", 1:5)
    diffcols_cumhosp <- paste0("diff_cumhosp_lag", 1:5)

    for (i in 1:5) {

      fmtdat[, diffcols_hosprate[i] := (get(paste0("hosprate_100k_", week)) -
                                     get(paste0("hosprate_100k_", week - i))
      )]

      fmtdat[, diffcols_cumhosp[i] := (get(paste0("cumhosp_100k_", week)) -
                                    get(paste0("cumhosp_100k_", week - i))
      )]
    }
  }

  if (week > 1) {
    currnames <- names(fmtdat)

    # get all variable names beginning with "hosprate" and save
    # the name of the current week's hosprate separately
    hr_vars <- currnames[grepl("^hosprate", currnames)]
    hr_curr <- hr_vars[length(hr_vars)]

    # do the same for cumhosp variable
    ch_vars <- currnames[grepl("^cumhosp\\_100", currnames)]
    ch_curr <- ch_vars[length(ch_vars)]

    # get all diff variables
    hr_diff <- currnames[grepl("^diff_hosprate", currnames)]
    ch_diff <- currnames[grepl("^diff_cumhosp", currnames)]

    # create interactions between hr_curr and cumhosp diffs
    hr_ixnames <- paste0("hr", week, "xdchl", 1:length(ch_diff))
    ch_ixnames <- paste0("ch", week, "xdhrl", 1:length(hr_diff))

    fmtdat[, (hr_ixnames) :=
               lapply(hr_diff, function(x) get(x) * get(ch_diff))]

    fmtdat[, (ch_ixnames) :=
               lapply(ch_diff, function(x) get(x) * get(hr_diff))]
  }

  # predicting on new data requires that new data to be fed to
  # the fitted SL as an sl3 task
  fmtdat_task <- make_sl3_Task(
    data = fmtdat,
    covariates = names(origtask$X),
    outcome = target_select,
    outcome_type = "continuous"
  )

  out <- list(
    task = fmtdat_task,
    median_pred = unname(unlist(md_preds[, ..target_select]))
  )

  out
}

ptask <- format_empirical_season(
  week = current_week,
  predict_on = obs_season
)

pred_compare <- list(
  obs_season = obs_season,
  obs_outcome = ptask$task$Y,
  sl_pred = ft$predict(ptask$task),
  sl_pred_abserr = abs(ft$predict(ptask$task) - ptask$task$Y),
  md_pred = ptask$median_pred,
  md_pred_abserr = abs(ptask$median_pred - ptask$task$Y)
)

## predictions
saveRDS(
  pred_compare,
  file = file.path(
    spec_output_dir,
    paste0(
      "s", obs_season,
      "_w", sprintf("%02d", current_week),
      "_pred_compare.Rds"
    )
  )
)

## component model fits and risks
comps <- list(
  cvrisks = ft$cv_risk(loss_absolute_error),
  learner_fits = ft$learner_fits
)
saveRDS(
  comps,
  file = file.path(
    spec_output_dir,
    paste0(
      "s", obs_season,
      "_w", sprintf("%02d", current_week),
      "_compfits.Rds"
    )
  )
)


cat("WARNING LIST", rep("=", 60), "\n\n", sep = "")
warnings()

devtools::session_info()
