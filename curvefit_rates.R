# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol
# 2015;11:e1004382. doi:10.1371/journal.pcbi.1004382.


# Load packages -------------------------------------------------------------

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

# Load data -----------------------------------------------------------------

ed <- readRDS("empdat.Rds")
print(head(ed))

# convert all data frames to data.tables
sapply(ed, setDT)
sapply(names(ed), function(x) sapply(ed[[x]], class))

ed$whsp_rt[, ("season") := lapply(.SD, as.character), .SDcols = "season"]
ed$whsp_rt <- ed$whsp_rt[agecat == "Overall", ]

# only High/Moderate seasons available for rates
ed$whsp_rt[mmwr_week == 40]

# subset whsp_ct to desired seasons and epiweeks
# drop pandemic flu
# @NOTE: If able to simulate by season severity eventually, can drop seasons
#        without a severity rating (i.e., 2018-19)
drop_seas <- c("2009-10")

ew_order <- c(40:53, 1:17)
ed$whsp_rt <- ed$whsp_rt[!season %in% drop_seas & mmwr_week %in% ew_order]

names(ed$whsp_rt)[names(ed$whsp_rt) == "season"] <- "seas"

# @DEV 2019-08-16: Move to data-cleaning.R
# create a week variable that matches epiweek to integers
# trandfilter() does not take factors
ed$whsp_rt[, week := c(1:31)[match(mmwr_week, ew_order)]]

# view variable classes in all datasets
classes <- sapply(ed, function(x) sapply(x, class))
classes

print(ed)
ed

# View Historical Curves ----------------------------------------------------
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

emp_hosp <- ggplot(ed$whsp_rt, aes(x = week, y = weekrate)) +
  geom_line(
    aes(group = seas),
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

emp_cumr <- ggplot(ed$whsp_rt, aes(x = week, y = cumrates)) +
  geom_line(
    aes(group = seas),
    alpha = 0.6
  ) +
  geom_text(
    data = ed$whsp_rt[week == max(week)],
    aes(x = 33, label = seas),
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

# Quadratic Trend Filter ----------------------------------------------------

# Split Observed Seasons
# each gets its own data.frame
seas_obs <- split(ed$whsp_rt, ed$whsp_rt$seas)
print(seas_obs)
seas_obs

# run trendfilter on each observed season
tf_seas <- lapply(seas_obs, function(x) {
  trendfilter(x = x$week, y = x$weekrate, k = 2)
})

# view model summaries
lapply(tf_seas, summary)

# Predict
# predict the weekly count (y) and error (tau) for each season
pred_fun <- function(x, y, lambda_val) {
  predict(y, x.new = x, lambda = y$lambda[lambda_val])
}

tf_pred <- lapply(
  setNames(names(tf_seas), names(tf_seas)),
  function(x, lambda_insert = 25) {
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

print(tf_pred)

# @NOTE: Suppressing 'unequal factor levels' warnings. A benign side effect of #        using bind_rows().
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
  scale_x_continuous("Epiweek", labels = c(0, ew_order[c(10, 20, 30)])) +
  labs(
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

# Generate hypothetical curves -----------------------------------

# record peak weeks ()
dist_peaks <-
  ed$whsp_rt[, .(
    pkhosp = max(weekrate),
    pkweek = week[weekrate == max(weekrate)]
  ),
  by = "seas"
  ]

print(dist_peaks)
print(ed)

# Generate High/Moderate-Severity Curves ------------------------------------

library(tictoc)

# number of curves to simulate
reps <- 3000

# time the simulations
tic(paste0("Curve simulations (n = ", reps, ")"))

sel_lambda <- 25

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
      hstdat = ed$whsp_rt
    )
  )
)
toc()

names(hhc)
print(hhc$outhc)

# view curves
simsub <- 30
hyp_hosp <- ggplot(hhc$outhc[cid %in% 1:simsub],
                   aes(x = week, y = prediction)) +
  geom_vline(
    data = data.frame(x = c(1, 31)),
    aes(xintercept = x),
    color = "red"
  ) +
  geom_text(
    aes(x = 16, y = 14),
    label = "Prediction window",
    color = "red"
  ) +
  geom_line(aes(group = cid), alpha = 0.2) +
  labs(
    x = "Week",
    y = "Predicted hospitalizations (per 100,000)",
    title = "Hypothetical Hospitalization Curves",
    subtitle = paste(simsub, "simulations"),
    caption = paste0("\u03BB index", " = ", sel_lambda)
  ) +
  # remove weeks outside the CDC season window
  # avoids including extrapolations well past the data
  coord_cartesian(xlim = c(1, 31), ylim = c(0, 10)) +
  theme_tweak

hyp_hosp

library(grid)
library(gridExtra)
curve_grid <- grid.arrange(emp_hosp, emp_cumr, hyp_hosp, nrow = 1)
grid::grid.draw(curve_grid)

ggsave("curve_grid.pdf", curve_grid, width = 3, height = 1, scale = 6)
ggsave("curve_grid.jpg", curve_grid, width = 3, height = 1, scale = 6)
