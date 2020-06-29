#' Super Learner Procedure
#'
#' Functions used to specify and execute the enesemble learning procedure.
#'

#' @param target One of "pkrate", "pkweek", or "cumhosp". No default.
#' @param current_week Week of flu season for which to make a task.
#' @param lambda_type Pick the curves simulated based on trendfilter lambda. Should be one of "lambda-min" or "lambda-1se".
#' 
#' @return A list containing the sl3 tasks for each week of the flu season.
#'
#' @describeIn super_learner_proc Specify learning tasks for each week.
#'
#' @import origami sl3 stringr
#' @export fhp_make_task

fhp_make_task <- function(target, current_week, lambda_type) {

  clndir <- here::here("data", "cleaned")

  datfile <- list.files(
    clndir,
    pattern = paste0(
      "sim_dataset_analytic_",
      lambda_type, "_week_",
      str_pad(current_week, 2, "left", "0")
    ),
    full.names = TRUE
  )

  # read in data for the week
  curr <- readRDS(datfile)

  # season templates (to use as fold ids in cross-validation)
  season_template_ids <- unique(curr$template)
  curr$template_numeric <- as.numeric(curr$template)

  covar_exclude_pat <- "pkrate|pkweek|cumhosp$|template|cid"
  covars <- names(curr)[!grepl(covar_exclude_pat, names(curr))]

  # set folds to 15 (equivalent to leave-one-out CV, by template id)
  fold_scheme <- make_folds(
    cluster_ids = season_template_ids,
    V = 15
  )

  task_spec <- make_sl3_Task(
    data = curr,
    covariates = covars,
    outcome = target,
    outcome_type = "continuous",
    folds = fold_scheme,
    id = "template_numeric"
  )


  return(task_spec)

}


#' @param learner_pat Regular expression specifying which learners to include in the learner stack. Defaults
#'                    to "^lrnr_", which adds any object whose name starts with "lrnr" in the global environment
#'                    to the learner stack. To select a specific subset of learners, provide appropriate regex.
#' @param gamweek The week for which the super learner algorithm is being run. Used to limit the variables fed to smoothing functions in the generalized additive models (GAMs, \code{mgcv} package).
#' @param currtask Machine learning task as produced by the sl3 package. Used to build custom model formulas for the GAMs.
#' @param verbose Logical indicating whether to print the full component learner stack. Defaults to FALSE.
#'
#' @describeIn super_learner_proc Specify all the component learners and assign them to the global environment.
#'
#' @export fhp_spec_learners

fhp_spec_learners <- function(learner_pat = "^lrnr_", gamweek, currtask, verbose = FALSE) {

  # GLM
  scrn_glm <- Lrnr_screener_corP$new()
  lrnr_glm_gauss <- Lrnr_glm$new(family = gaussian())
  lrnr_screen_glm <<- Pipeline$new(scrn_glm, lrnr_glm_gauss)

  # Specify polymars learners
  pmars_tune <- seq(2, 10, 2)

  for (i in seq_along(pmars_tune)) {
    assign(
      paste0("lrnr_polymars_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_polspline$new(gcv = pmars_tune[i]),
      envir = .GlobalEnv
    )
  }

  # Specify random forest learners
  numcovs <- length(currtask$nodes$covariates)
  rf_tune <- expand.grid(
    ntree = c(50, 100, 500), # number of trees
    nodesize = c(5, 10, 50),  # minimum number of observations in terminal nodes
    mtry = c(numcovs / 3, numcovs / 2, numcovs / 1.5)
  )

  for (i in 1:nrow(rf_tune)) {
    assign(
      paste0("lrnr_rf_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_randomForest$new(
        ntree = rf_tune[i, 1],
        nodesize = rf_tune[i, 2],
        mtry = round(rf_tune[i, 3])
        ),
      envir = .GlobalEnv
    )
  }

  # Specify neural network variants
  nnet_tune <- expand.grid(
    nodes = c(5, 10, 25, 50, 75, 100),
    penalty = c(0, 0.005, 0.1, 0.2, 0.4)
  )

  for (i in 1:nrow(nnet_tune)) {
    assign(
      paste0("lrnr_nnet_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_pkg_SuperLearner$new(
        SL_wrapper = "SL.nnet",
        size = nnet_tune[i, 1],
        decay = nnet_tune[i, 2]
      ),
      envir = .GlobalEnv
    )
  }

  # Specify SVM variants
  lrnr_svm_radial <<- Lrnr_svm$new(kernel = "radial")
  lrnr_svm_poly1  <<- Lrnr_svm$new(kernel = "polynomial", degree = 1)
  lrnr_svm_poly2  <<- Lrnr_svm$new(kernel = "polynomial", degree = 2)
  lrnr_svm_poly3  <<- Lrnr_svm$new(kernel = "polynomial", degree = 3)

  # Specify GAM learners

  ## build GAM model formula
  if (missing(gamweek) | missing(currtask)) {

    warning("Current week or task was not specified. The GAM learners will try smoothing parameters for all continuous predictors, per sl3 defaults. To build custom model formulas, specify both gamweek and currtask arguments.")

  } else {

    outcome <- currtask$nodes$outcome
    covs <- currtask$nodes$covariates

    smoothed_cov <- c(
      paste0("hosprate_100k_", gamweek),
      paste0("cumhosp_100k_", gamweek)
    )

    smoothed_terms <- paste(
      sapply(smoothed_cov, function(x) paste0("s(", x, ", k = 7)")),
      collapse = "+"
    )

    unsmoothed_terms <- paste(covs[!covs %in% smoothed_cov], collapse = "+")

    fml <- paste(outcome, "~", smoothed_terms)
  }

  gam_tune <- list(gamma = c(1, 5, 10))

  for (i in 1:length(gam_tune$gamma)) {
    assign(
      paste0("lrnr_gam_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_gam$new(
        formula = as.formula(fml),
        select = TRUE, # allow penalization to drop parameters from the model
        gamma = gam_tune$gamma[i]
      ),
      envir = .GlobalEnv
    )
  }

  # Specify Elastic Net learners
  glmnet_folds <- 10 # n folds to use for glmnet's internal cross-validation

  lrnr_lasso <<- Lrnr_glmnet$new(
    alpha = 1,
    standardize = TRUE,
    nfolds = glmnet_folds
  )

  lrnr_ridge <<- Lrnr_glmnet$new(
    alpha = 0,
    standardize = TRUE,
    nfolds = glmnet_folds
  )

  elastic_tune <- c(0.25, 0.5, 0.75)

  for (i in seq_along(elastic_tune)) {
    assign(
      paste0("lrnr_elastnet_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_glmnet$new(alpha = elastic_tune[i]),
      envir = .GlobalEnv
    )
  }

  # Specify LOESS learners
  loess_tune <- c(0.25, 0.5, 0.75, 1)

  for (i in seq_along(loess_tune)) {
    assign(
      paste0("lrnr_loess_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_pkg_SuperLearner$new(
        SL_wrapper = "SL.loess",
        span = loess_tune[i]
      ),
      envir = .GlobalEnv
    )
  }

  # define learner stack
  stack_full <<- Stack$new(
    lapply(
      ls(pattern = learner_pat, envir = .GlobalEnv),
      get
    )
  )

  if (verbose) print(stack_full)
}


#' @param task A learning task created by `fhp_make_task()`.
#' @param write A logical indicating whether to write results to a file. Defaults to TRUE.
#' @param results_path Relative to project root, where to save the results files.
#' @param current_week The week number at which predictions are made.
#' @param ... Pass arguments to Lrnr_sl$new(...)
#'
#' @describeIn super_learner_proc Runs the parallelized super learner procedure based on `fhp_make_tasks()` and `fhp_spec_learners()`.
#'
#' @import delayed future sl3 tictoc
#' @export fhp_run_sl

fhp_run_sl <- function(task, write = TRUE, results_path = "results", current_week, ...) {

  # specify the super learner
  sl <- Lrnr_sl$new(learners = stack_full, ...)

  plan(multiprocess)

  sl_fit <- delayed_learner_train(sl, task)

  task_sched <- Scheduler$new(
    sl_fit,
    FutureJob,
    nworkers = future::nbrOfWorkers()  # specify number of cores to use
  )

  tic("Ensemble Super Learner Runtime:")
  sl_trained <- task_sched$compute()
  toc()

  ## select a subset of the super learner outputs to reduce file size:
  ## component, cross-validated, and other learners save numerous fit objects
  ## that contain multiple copies of the underlying datasets (unneeded for analysis)
  sl_pruned <- list(
    is_trained = sl_trained$is_trained,
    params = sl_trained$params,
    fit_uuid = sl_trained$fit_uuid,
    learner_uuid = sl_trained$learner_uuid,
    metalearner_fit = sl_trained$metalearner_fit()
  )

  # get cross-validated risk
  risk <- sl_trained$cv_risk(loss_absolute_error)

  # get ensemble predictions for each fold (season template)
  meta_preds <- sl_trained$fit_object$cv_meta_fit$predict()
  full_preds <- sl_trained$fit_object$full_fit$predict()

  out <- list(
      task = task,
      sl_pruned = sl_pruned,
      cv_risk_abserr = risk,
      meta_preds = meta_preds,
      full_preds = full_preds
  )
  #out <- sl_trained
  target <- task$nodes$outcome

  slug <- paste0(
    "sl_", target, "_",
    stringr::str_pad(current_week, width = 2, "left", pad = "0")
  )

  if (write) {
    if(!file.exists(results_path)) dir.create(results_path)
    saveRDS(out, here::here(results_path, paste0(slug, ".Rds")))
  } else {
    assign(slug, out, envir = .GlobalEnv)
  }

  print(warnings())

}

#' @param w Numerical integer week (1--30). Used in script testing.
#' @param slurm Logical indicating whether job is submitted via batch script in SLURM. If TRUE, will take job ID from job array.
#'
#' @export get_week
#' @describeIn super_learner_proc Specifies the week for which to run the super learner algorithm.
#'
get_week <- function(w = NULL, slurm = TRUE) {
  if (is.null(w) & slurm) {
    as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  } else {
    w
  }
}
