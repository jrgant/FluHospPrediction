# %% LOAD AND SETUP ------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
  ggthemes,
  sl3,
  delayed,
  future,
  SuperLearner,
  origami,
  forcats
)

clndir <- here::here("data", "cleaned")

datfiles <- list.files(
  clndir,
  pattern = "sim_dataset_analytic_week",
  full.names = TRUE
)

dat <- lapply(datfiles, readRDS)

# season templates (to use as fold ids in cross-validation)
season_template_ids <- unique(dat[[1]]$template)


# %% SUPER LEARNER (PEAK HEIGHT) -----------------------------------------------

temp <- dat[[9]]
temp$template_numeric <- as.numeric(temp$template)

temp[, .N, .(template, template_numeric)][order(template)]



covars <- names(temp)[!grepl("pkrate|pkweek|cumhosp$", names(temp))]
print(covars)


# set folds to 15 (equivalent to leave-one-out CV, by template id)
fold_scheme <- make_folds(
  cluster_ids = season_template_ids,
  V = 15
)

task_target_pkrate <- make_sl3_Task(
  data = temp,
  covariates = covars,
  outcome = "pkrate",
  outcome_type = "continuous",
  folds = fold_scheme,
  id = "cid"
)

task_target_pkrate

# Mean learner
lrnr_mean <- Lrnr_mean$new()

# GLM
scrn_glm <- Lrnr_pkg_SuperLearner_screener$new(SL_wrapper = "screen.glmnet")
lrnr_glm_gauss <- Lrnr_glm$new(family = gaussian())
lrnr_screen_glm <- Pipeline$new(scrn_glm, lrnr_glm_gauss)


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
    Lrnr_polspline$new(gcv = pmars_tune[i])
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
    Lrnr_randomForest$new(ntree = rf_tune[i, 1], nodesize = rf_tune[i, 2])
  )
}

rf_lrnr_list <- ls(pattern = "lrnr_rf")

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
    )
  )
}

# Specify SVM variants
lrnr_svm_radial <- Lrnr_svm$new(kernel = "radial")
lrnr_svm_poly1  <- Lrnr_svm$new(kernel = "polynomial", degree = 1)
lrnr_svm_poly2  <- Lrnr_svm$new(kernel = "polynomial", degree = 2)
lrnr_svm_poly3  <- Lrnr_svm$new(kernel = "polynomial", degree = 3)

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
    )
  )
}

# Specify Elastic Net learners
glmnet_folds <- 15 # n folds to use for glmnet's internal cross-validation

lrnr_lasso <- Lrnr_glmnet$new(
  alpha = 1,
  standardize = TRUE,
  nfolds = glmnet_folds
)

lrnr_ridge <- Lrnr_glmnet$new(
  alpha = 0,
  standardize = TRUE,
  nfolds = glmnet_folds
)

elastic_tune <- c(0.25, 0.5, 0.75)

for (i in seq_along(elastic_tune)) {
  assign(
    paste0("lrnr_elastnet_", stringr::str_pad(i, width = 2, pad = "0")),
    Lrnr_glmnet$new(alpha = elastic_tune[i])
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
    )
  )
}

ls()
# TODO: Specify GBM (use xgboost)
# lrnr_gbm <-

# define learner stack
stack_all_pkrate <- Stack$new(lapply(ls(pattern = "^lrnr"), get))

stack_test_pkrate <- Stack$new(
  lrnr_mean,
  lrnr_screen_glm,
  lrnr_svm_radial,
  lrnr_nnet_01,
  lrnr_loess_01
)

# specify the metalearner (ensemble model)
metalearner <- Lrnr_nnls$new(convex = TRUE)

# @TODO 2020-03-31: write the absolute error loss function
#     L : (O, Qbar): |Y - Qbar(A,W)|

# Source:
# Polley EC, Rose S, van der Laan MJ. Super Learning. In: van der Laan MJ, Rose
# S, eds. Targeted Learning: Causal Inference for Observational and Experimental
# Data. New York, NY: Springer New York;
# 2011:43–66.(https://doi.org/10.1007/978-1-4419-9782-1_3)

# specify the super learner
sl <- Lrnr_sl$new(
  learners = stack_test_pkrate,
  metalearner = metalearner
)

# show task schedule
sl_fit <- delayed_learner_train(sl, task_target_pkrate)

# parallelize SL fit
plan(multiprocess)  # makes multiple cores available

sched_pkrate <- Scheduler$new(
  sl_fit,
  FutureJob,
  nworkers = 8  # specify number of cores to use
)

tictoc::tic("Ensemble Super Learner Fit")
cv_fit <- sched_pkrate$compute()
tictoc::toc()

# make sure all learners were trained
sapply(cv_fit$learner_fits, . %>% .$is_trained)

cv_fit$fit_object$full_fit
cv_fit$fit_object$cv_meta_fit

# stack_preds <- cv_fit$predict()
#
# tpreds <- cbind(temp, stack_preds)
# head(tpreds)

# infold_preds <- ggplot(tpreds, aes(x = stack_preds, y = pkrate)) +
#   geom_point(alpha = 0.4) +
#   geom_smooth() +
#   facet_wrap(~template) +
#   theme_base()
#
# infold_preds

# ggsave(
#   plot = infold_preds,
#   device = "png",
#   filename = here::here(
#   "interim-reports/2020-04-06_Prelim-Results",
#   "infold_preds.png"
#   )
#   )

risk <- cv_fit$cv_risk(loss_squared_error)
risk

meta_preds <- cv_fit$fit_object$cv_meta_fit$predict()
full_preds <- cv_fit$fit_object$full_fit$predict()

print(meta_preds)
print(full_preds)

out <- list(
  cv_fit = cv_fit,
  risk = risk,
  meta_preds = meta_preds
)

prediction_plot(cv_fit)

object.size(out)

cp <- cbind(
  temp,
  full_preds
)

head(cp)

# saveRDS(out, "results/test_sl.Rds")
