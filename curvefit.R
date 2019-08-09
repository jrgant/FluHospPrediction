# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol 2015;11:e1004382.
# doi:10.1371/journal.pcbi.1004382.


# Load packages -------------------------------------------------------------

pacman::p_load(glmgen, ggplot2, ggthemes, viridis, purrr, tidyr, data.table)

source("R/simcrv_funs.R")

# Load data -----------------------------------------------------------------

ed <- readRDS("empdat.Rds")

# convert data.frames in list to data.tables
sapply(ed, setDT)

fct_to_char <- c("seas", "severity", "sev2")
ed$whsp_ct[, (fct_to_char) := lapply(.SD, as.character), .SDcols = fct_to_char]

# subset whsp_ct to deisred seasons and epiweeks
# drop pandemic flu and seasons with missing severity data
drop_seas <- c("2009-10", "2018-19")
ed$cdc_svr <- ed$cdc_svr[!season %in% drop_seas]

ew_order <- c(40:53, 1:17)
ed$whsp_ct <- ed$whsp_ct[!seas %in% drop_seas & epiweek %in% ew_order]

# create a week variable that matches epiweek to integers
# trandfilter() does not take factors
ed$whsp_ct[, week := c(1:31)[match(epiweek, ew_order)]]

# view variable classes in both datasets
sapply(ed, function(x) sapply(x, class))
print(ed)

# Quadratic Trend Filter ----------------------------------------------------

# Split Observed Seasons
# each gets its own data.frame
seas_obs <- split(ed$whsp_ct, ed$whsp_ct$seas)
print(seas_obs)

# run trendfilter on each observed season
tf_seas <- lapply(seas_obs, function(x) {
  trendfilter(x = x$week, y = x$inf.tot, k = 2)
})

print(tf_seas)

# view model summaries
sapply(tf_seas, summary, simplify = FALSE)

# Predict
# predict the weekly count (y) and error (tau) for each season
pred_fun <- function(x, y) {
  predict(y, x.new = x, lambda = y$lambda[25])
}

# @NOTE: predict() throws a warning saying saying:
#        "Predict called at new x values out of the original range."
#
#        Don't think it's a problem:
#          Leap years produce coefficients for weeks that aren't present
#          in other years. Therefore, I believe the model extrapolates into
#          the missing week when generating predictions for non-leap years.

tf_pred <- lapply(
  setNames(names(tf_seas), names(tf_seas)),
  function(x) {
    pred.hosp <- pred_fun(seas_obs[[x]]$x, tf_seas[[x]])
    obs.hosp1 <- tf_seas[[x]]$y
    obs.hosp2 <- seas_obs[[x]]$inf.tot
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
      tau = sqrt(mean(sqerr))
    )
  }
)

print(tf_pred)

tfp <- map_dfr(tf_pred, function(x) x[[1]])
setDT(tfp)

tfp %>%
  ggplot(aes(x = week, group = season, col = season)) +
  geom_line(aes(y = pred.hosp)) +
  geom_point(aes(y = obs.hosp1), shape = 21) +
  facet_grid(~severity) +
  scale_color_viridis_d() +
  scale_x_continuous("Epiweek", labels = c(0, ew_order[c(10, 20, 30)])) +
  labs(
    y = "Hospitalizations (n)",
    title = "Trend filter, predicted hospitalizations vs. observed"
  ) +
  theme_clean(base_size = 15) +
  theme(
    plot.caption = element_text(face = "italic", size = 10),
    axis.title = element_text(face = "bold", color = "slategray")
  )


# Generate hypothetical curves -----------------------------------

# record peak weeks ()
dist_peaks <-
  ed$whsp_ct[, .(
    pkhosp = max(inf.tot),
    pkweek = week[inf.tot == max(inf.tot)]
  ),
  by = "seas"
  ]

print(dist_peaks)
print(ed)

ed$whsp_ct[sev2 == "Low"][, unique(seas)]
ed$whsp_ct[sev2 == "High/Moderate"][, unique(seas)]
print(ed$whsp_ct[seas == "2014-15", ])


# Generate High/Moderate-Severity Curves ------------------------------------

hmhc <- simdist(
  nreps = 10000,
  seed = 1983745,
  sim_args = list(
    severity2 = "High/Moderate",
    lamb_val = 40
  )
)

# view curves
ggplot(hmhc, aes(x = week, y = prediction)) +
  geom_line(aes(group = cid), alpha = 0.5) +
  labs(
    title = "Hypothetical Hospitalization Curves",
    subtitle = "High-to-moderate severity seasons"
  ) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())


# Generate Low-Severity Curves ----------------------------------------------

lhc <- simdist(
  nreps = 10000,
  seed = 98370,
  sim_args = list(
    severity2 = "Low",
    lamb_val = 40
  )
)

# view curves
ggplot(lhc, aes(x = week, y = prediction)) +
  geom_line(aes(group = cid), alpha = 0.5) +
  labs(
    title = "Hypothetical Hospitalization Curves",
    subtitle = "Low-severity seasons"
  ) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())

# Compare H/M and Low Curve Sets --------------------------------------------

hmhc[, sevtype := "High/Moderate"]
lhc[, sevtype := "Low"]

all_hc <- as.data.table(rbind(hmhc, lhc))
all_hc[, mean(prediction), by = c("sevtype", "week")]

ggplot(all_hc, aes(x = week, y = prediction)) +
  geom_line(aes(group = paste(sevtype, cid))) +
  geom_smooth(method = "loess") +
  facet_wrap(~sevtype) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())
