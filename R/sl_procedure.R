#' Super Learner Procedure
#'
#' Functions used to specify and execute the enesemble learning procedure.
#'
#'

#' @param target One of "pkrate", "pkweek", or "cumhosp". No default.
#'
#' @return A list containing the sl3 tasks for each week of the flu season.
#'
#' @describeIn super_learner_proc Specify learning tasks for each week.
#'
#' @import origami sl3
#' @export fhp_make_tasks

fhp_make_tasks <- function(target) {

  clndir <- here::here("data", "cleaned")

  datfiles <- list.files(
    clndir,
    pattern = "sim_dataset_analytic_week",
    full.names = TRUE
  )

  tasklist <- lapply(datfiles, function(x) {

    # read in data for the week
    curr <- readRDS(x)

    # season templates (to use as fold ids in cross-validation)
    season_template_ids <- unique(curr$template)
    curr$template_numeric <- as.numeric(curr$template)
    curr[, .N, .(template, template_numeric)][order(template)]

    covars <- names(curr)[!grepl("pkrate|pkweek|cumhosp$", names(curr))]

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

  })

  return(tasklist)

}


#'
#'
#' @describeIn super_learner_proc Specify all the component learners and assign them to the global environment. Takes no arguments.
#' 
#' @export fhp_spec_learners

fhp_spec_learners <- function() {

  # Mean learner
  lrnr_mean <<- Lrnr_mean$new()

  # GLM
  scrn_glm <<- Lrnr_pkg_SuperLearner_screener$new(SL_wrapper = "screen.glmnet")
  lrnr_glm_gauss <<- Lrnr_glm$new(family = gaussian())
  lrnr_screen_glm <<- Pipeline$new(scrn_glm, lrnr_glm_gauss)


  # @NOTE 2020-04-08:
  # - I think Andrew was right about the weirdness of the Poisson and
  #   quasi-Poisson results... I'm not actually modeling a count, I'm
  #   modeling a peak count, so these two models probably don't make sense

  # lrnr_glm_pois <- Lrnr_glm$new(family = poisson())
  # lrnr_glm_qpois <- Lrnr_glm$new(family = quasipoisson())

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
  rf_tune <- expand.grid(
    ntree = c(50, 100, 200, 500), # number of trees
    nodesize = c(3, 5, 10) # minimum number of observations in terminal nodes
  )

  for (i in 1:nrow(rf_tune)) {
    assign(
      paste0("lrnr_rf_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_randomForest$new(ntree = rf_tune[i, 1], nodesize = rf_tune[i, 2]),
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

  # TODO: Specify bagged learners
  # lrnr_bag <-

  # Specify GAM learners
  gam_tune <- list(gamma = 1:5)

  for (i in 1:length(gam_tune$gamma)) {
    assign(
      paste0("lrnr_gam_", stringr::str_pad(i, width = 2, pad = "0")),
      Lrnr_gam$new(
        select = TRUE, # allow penalization to drop parameters from the model
        gamma = gam_tune$gamma[i]
      ),
      envir = .GlobalEnv
    )
  }

  # Specify Elastic Net learners
  glmnet_folds <- 15 # n folds to use for glmnet's internal cross-validation

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

  # TODO: Specify GBM (use xgboost)
  # lrnr_gbm <-

  # define learner stack
  stack_full <<- Stack$new(
    lapply(
      ls(pattern = "^lrnr", envir = .GlobalEnv),
      get
    )
  )

  # specify the metalearner (ensemble model)
  metalearner <<- Lrnr_nnls$new(convex = TRUE)

}



#' @param tasklist A list of learning tasks created by `fhp_make_tasks()`.
#' @param write A logical indicating whether to write results to a file. Defaults to TRUE.
#' @param results_path Relative to project root, where to save the results files.
#'
#' @describeIn super_learner_proc Runs the parallelized super learner procedure based on `fhp_make_tasks()` and `fhp_spec_learners()`.
#'
#' @import delayed future sl3 tictoc
#' @export fhp_run_sl

fhp_run_sl <- function(tasklist, write = TRUE, results_path = "results") {

  # specify the super learner
  sl <<- Lrnr_sl$new(
    learners = stack_full,
    metalearner = metalearner,
    )

  # specify the super learner for all tasks
  sl_fits <<- lapply(tasklist, function(x) {
    delayed_learner_train(sl, x)
  })

  plan(multiprocess)

  for (i in 1:length(sl_fits)) {
    task_sched <- Scheduler$new(
      sl_fits[[i]],
      FutureJob,
      nworkers = future::nbrOfWorkers()  # specify number of cores to use
    )

    tic("Ensemble Super Learner Runtime:")
    sl_trained <- task_sched$compute()
    toc()

    # select a subset of the super learner outputs to reduce file size:
    # component, cross-validated, and other learners save numerous fit objects
    # that contain multiple copies of the underlying datasets (unneeded for analysis)
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
      sl_pruned = sl_pruned,
      cv_risk_abserr = risk,
      meta_preds = meta_preds,
      full_preds = full_preds
    )

    target <- tasklist[[i]]$nodes$outcome

    slug <- paste0(
      "sl_", target, "_", stringr::str_pad(i, width = 2, "left", pad = "0"))

    if (write) {
      saveRDS(out, here::here(results_path, paste0(slug, ".Rds")))
    } else {
      assign(slug, out, envir = .GlobalEnv)
    }
  }

}
