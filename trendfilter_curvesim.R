# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol
# 2015;11:e1004382. doi:10.1371/journal.pcbi.1004382.


# Load packages ---------------------------------------------------------

# %% Setup
pacman::p_load(
  glmgen,
  ggplot2,
  ggthemes,
  viridis,
  purrr,
  tidyr,
  data.table,
  forcats
)

source("R/simcrv_funs.R")

# Load data -------------=----------------------------------------------

# %% Setup
ed <- readRDS("data/empdat.Rds")
class(ed)
print(head(ed))

# check for missing data (all 0 = no missing data)
ed[, lapply(.SD, is.na)][, lapply(.SD, sum)] %>% print

# @NOTE: If able to simulate by season severity eventually, can drop seasons
#        without a severity rating (i.e., 2018-19)

# %% Drop Pandemic Flu
ed <- ed[season != "2009-10"]
table(ed$season)

# View Historical Curves ------------------------------------------------
sublab <- "2003–2019, excludes 2009–2010 season"
sourcecap <- "Source: FluSurv-NET"

theme_tweak <-
    theme_clean(base_size = 15) +
    theme(
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 10, face = "italic"),
      plot.background = element_blank(),
      plot.margin = margin(rep(0.5, 4),  unit = "cm")
    )

emp_hosp <- ggplot(ed, aes(x = weekint, y = weekrate)) +
  geom_line(
    aes(group = season),
    size = 1,
    alpha = 0.4
  ) +
  labs(
    x = "Week",
    y = "Hospitalization rate (per 100,000)",
    title = "Empirical weekly hospitalizations",
    subtitle = sublab,
    caption = sourcecap
  ) +
  theme_tweak

emp_hosp

emp_cumr <- ggplot(ed, aes(x = weekint, y = cumrates)) +
  geom_line(
    aes(group = season),
    alpha = 0.6
  ) +
  geom_text(
    data = ed[weekint == max(weekint)],
    aes(x = 33, label = season),
    size = 2
  ) +
  labs(
    x = "Week",
    y = "Empirical cumulative hospitalizations (per 100,000)",
    title = "Cumulative hospitalization rates",
    subtitle = sublab,
    caption = sourcecap
  ) +
  theme_tweak

emp_cumr

# Quadratic Trend Filter ------------------------------------------------

# Split Observed Seasons
# each gets its own data.frame
seas_obs <- split(ed, ed$season)
print(seas_obs)

# run trendfilter on each observed season
tf_seas <- lapply(seas_obs, function(x) {
  trendfilter(x = x$weekint, y = x$weekrate, k = 1, family = "gaussian")
})

# view model summaries
lapply(tf_seas, summary)

# Predict
# predict the weekly count (y) and error (tau) for each season
pred_fun <- function(x, y, lambda_val) {
  predict(y, x.new = x, lambda = y$lambda[lambda_val])
}

# @NOTE 2019-08-21:
#   All subsequent trendfilter predictions use the lambda set here.
sel_lambda <- 20

# %%
# @TODO 2019-08-12:
#   Consider making this a function to call from R/simcrv_funs.R
tf_pred <- lapply(
  setNames(names(tf_seas), names(tf_seas)),
  function(x, lambda_insert = sel_lambda) {

    pred.hosp <- pred_fun(seas_obs[[x]]$x, tf_seas[[x]], lambda_insert)
    obs.hosp1 <- tf_seas[[x]]$y
    obs.hosp2 <- seas_obs[[x]]$weekrate
    check.obs <- obs.hosp1 - obs.hosp2

    # calculate tau^2 for each season
    sqerr <- (as.vector(pred.hosp) - obs.hosp1)^2

    if (sum(check.obs) != 0) stop("Observed hospitalizations don't match!")

    list(
      dat = data.frame(
        season = x,
        week = 1:length(pred.hosp),
        pred.hosp = as.vector(pred.hosp),
        obs.hosp1,
        obs.hosp2,
        check.obs,
        sqerr,
        severity = seas_obs[[x]]$sev2
      ),
      # take the mean of the squared error
      mean.tau.sq = mean(sqerr),
      tau = sqrt(mean(sqerr)),
      # record lambda value used for predictions
      lambda = lambda_insert
    )
  }
)

sapply(tf_pred, function(x) class(x$dat))
print(tf_pred)

# %%
# @NOTE: Suppressing 'unequal factor levels' warnings.
#        A benign side effect of using dplyr::bind_rows().
tfp <- suppressWarnings(map_dfr(tf_pred, function(x) x[[1]]))
setDT(tfp)

# plot predictions vs. empirical data
ggplot(tfp, aes(x = week)) +
  geom_line(aes(y = pred.hosp), color = "red") +
  geom_point(
    aes(y = obs.hosp1),
    alpha = 0.5,
    size = 0.8
  ) +
  facet_wrap(~season) +
  scale_color_viridis_d() +
  labs(
    x = "Week",
    y = "Hospitalizations (per 100,000)",
    title = "Trend filter, predicted hospitalizations vs. observed",
    caption = paste("lambda = ", tf_pred[[1]]$lambda)
  ) +
  theme_clean(base_size = 15) +
  theme(
    plot.caption = element_text(face = "italic", size = 10),
    axis.title = element_text(face = "bold", color = "slategray"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Generate hypothetical curves -----------------------------------------

# %%
# record peak weeks ()
dist_peaks <-
  ed[, .(
    pkhosp = max(weekrate),
    pkweek = weekint[weekrate == max(weekrate)]
    ),
  by = "season" ]

print(dist_peaks)

# Generate Hypothetical Curves ------------------------------------------

library(tictoc)

# number of curves to simulate
reps <- 10000

# %% Simulate Curves
# time the simulations
tic(paste0("Curve simulations (n = ", reps, ")"))

# @NOTE: Suppressing warnings for predictions out of range. Expected behavior
#        due to the curve stretching. R is being asked to predict
#        hospitalizations outside the range [1, 31]. We restrict the prediction
#        time periods and plots to this range.
hhc <- suppressWarnings(
  simdist(
    nreps = reps,
    seed = 1983745,
    gimme = "everything",
    sim_args = list(
      severity2 = NULL,
      lamb_val = sel_lambda,
      hstdat = ed
    )
  )
)
toc()

# %%
names(hhc)
head(hhc$hc)

# check to make sure each season has 31 predicted periods
veclengths <- sapply(hhc$hc, function(x) length(x$eq$arg_f))
table(veclengths == 31)

# map prediction to i (1:31)
hhc$outhc[, weekmap := rep(1:31, length(unique(cid)))]
print(hhc$outhc, topn = 40)

# %%
# view curves
simsub <- 30

hyp_hosp_p <-
  ggplot(
    hhc$outhc[cid %in% sample(cid, size = simsub)],
    aes(x = weekmap, y = prediction)
  ) +
  geom_line(
    aes(group = cid, color = factor(cid)),
    alpha = 0.6
  ) +
  labs(
    x = "Week",
    y = "Predicted hospitalizations (per 100,000)",
    title = "Hypothetical Hospitalization Curves",
    subtitle = paste(simsub, "simulations"),
    caption = paste0("\u03BB index", " = ", sel_lambda)
  ) +
  theme_tweak +
  theme(legend.position = "none")

# @BUG 2019-09-03:
#   Inspection of random subsets of simulated curves reveals some unrealistic
#   patterns at time i = 0 (e.g., seasons beginning well above 0/100,000 people)
hyp_hosp_p

# Training and Test Sets ------------------------------------------------

# %%
# split hypothetical curves into training and test sets
# summarize training and test sets
hhc_sets <- c("train", "test")
train_cids <- with(hhc$outhc, min(cid):(0.5 * max(cid)))
test_cids <- with(hhc$outhc, unique(cid[!cid %in% train_cids]))

# check ranges
cat("Range of training set simulation IDs:", range(train_cids), "\n")
cat("Range of test set simulation IDs:", range(test_cids), "\n")

# split simulated curves
hhc_splits <- lapply(setNames(hhc_sets, hhc_sets), function(x) {
  if (x == "train") {
    df <- hhc$outhc[cid %in% train_cids, ]
  } else {
      df <- hhc$outhc[cid %in% test_cids, ]
  }

  # summarize hypothetical curves by prediction target
  # calculate weights to account for missing data due to curve shifting
  df[, .(pkht = max(prediction),
         pkwk = week[prediction == max(prediction)],
         pkwk_int = weekmap[prediction == max(prediction)],
         cumhosp = max(cumsum(prediction)),
         n = .N),
     by = "cid"] %>%
     .[, weight := n / sum(n)]
})

lapply(hhc_splits, nrow)

# %%
# Single-Season Test Set Summaries

# Unweighted
lapply(hhc_splits$train, summary)

# Weighted
target_params <- lapply(setNames(hhc_sets, hhc_sets), function(x) {
  hhc_splits[[x]][, .(pkht_w = weighted.mean(pkht, weight),
                      pkwk_w = weighted.mean(pkwk, weight),
                      cumhosp_w = weighted.mean(cumhosp, weight))]
})

hhc$train <- list(
  trainset = hhc$outhc[cid %in% train_cids, ],
  trainset_sum = hhc_splits[["train"]],
  train_targets = target_params[["train"]]
)

hhc$test <- list(
  testset = hhc$outhc[cid %in% test_cids, ],
  testset_sum = hhc_splits[["test"]],
  test_targets = target_params[["test"]]
)

print(hhc$train)
print(hhc$test)


# Write Hypothetical curves ---------------------------------------------

# %%
saveRDS(hhc, "data/hypothetical-curves.Rds")


# View and Write Plots --------------------------------------------------

library(grid)
library(gridExtra)

curve_grid <- grid.arrange(emp_hosp, emp_cumr, hyp_hosp_p, nrow = 1)
grid::grid.draw(curve_grid)

## plots
ggsave("analysis_plan/curve_grid.pdf", curve_grid,
       width = 3, height = 1, scale = 6)

ggsave("analysis_plan/curve_grid.jpg", curve_grid,
       width = 3, height = 1, scale = 6)
