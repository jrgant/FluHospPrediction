# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol 2015;11:e1004382.
# doi:10.1371/journal.pcbi.1004382.


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

# @BUG 2019-08-10: Temporary fix for mislabeled sev2
#    Put in data-cleaning.R when possible
ed$whsp_rt[, sev2 := fct_collapse(severity,
  `High/Moderate` = c("High", "Moderate"),
  Low = "Low"
)]

fct_to_char <- c("season", "severity", "sev2")
ed$whsp_rt[, (fct_to_char) := lapply(.SD, as.character), .SDcols = fct_to_char]
ed$whsp_rt <- ed$whsp_rt[agecat == "Overall", ]

# only High/Moderate seasons available for rates
ed$whsp_rt[mmwr_week == 40]


# subset whsp_ct to desired seasons and epiweeks
# drop pandemic flu and seasons with missing severity data
drop_seas <- c("2009-10", "2018-19")
ed$cdc_svr <- ed$cdc_svr[!season %in% drop_seas]

ew_order <- c(40:53, 1:17)
ed$whsp_rt <- ed$whsp_rt[!season %in% drop_seas & mmwr_week %in% ew_order]

# create a week variable that matches epiweek to integers
# trandfilter() does not take factors
ed$whsp_rt[, week := c(1:31)[match(mmwr_week, ew_order)]]

# view variable classes in all datasets
sapply(ed, function(x) sapply(x, class))
print(ed)
ed

# View Historical Curves ----------------------------------------------------
ggplot(ed$whsp_rt, aes(x = week, y = weekrate)) +
  geom_line(
    aes(group = season, color = sev2),
    size = 1.5, alpha = 0.7
  ) +
  facet_wrap(~sev2) +
  scale_color_viridis_d() +
  theme_clean(base_size = 15) +
  theme(
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

# Quadratic Trend Filter ----------------------------------------------------

# Split Observed Seasons
# each gets its own data.frame
seas_obs <- split(ed$whsp_rt, ed$whsp_rt$season)
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
pred_fun <- function(x, y) {
  predict(y, x.new = x, lambda = y$lambda[15])
}

tf_pred <- lapply(
  setNames(names(tf_seas), names(tf_seas)),
  function(x) {
    pred.hosp <- pred_fun(seas_obs[[x]]$x, tf_seas[[x]])
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
      tau = sqrt(mean(sqerr))
    )
  }
)

print(tf_pred)

tfp <- map_dfr(tf_pred, function(x) x[[1]])
setDT(tfp)

ggplot(tfp, aes(x = week, col = severity)) +
  geom_line(aes(y = pred.hosp)) +
  geom_point(aes(y = obs.hosp1), shape = 21, alpha = 0.5) +
  facet_wrap(~season) +
  scale_color_viridis_d() +
  scale_x_continuous("Epiweek", labels = c(0, ew_order[c(10, 20, 30)])) +
  labs(
    y = "Hospitalizations (n)",
    title = "Trend filter, predicted hospitalizations vs. observed"
  ) +
  theme_clean(base_size = 15) +
  theme(
    plot.caption = element_text(face = "italic", size = 10),
    axis.title = element_text(face = "bold", color = "slategray"),
    legend.position = "bottom"
  )

# Generate hypothetical curves -----------------------------------

# record peak weeks ()
dist_peaks <-
  ed$whsp_rt[, .(
    pkhosp = max(weekrate),
    pkweek = week[weekrate == max(weekrate)]
  ),
  by = "season"
  ]

# to make this work with the simcrv() and simdist() funs
names(dist_peaks)[names(dist_peaks) == "season"] <- "seas"
names(ed$whsp_rt)[names(ed$whsp_rt) == "season"] <- "seas"

print(dist_peaks)
print(ed)

ed$whsp_rt[sev2 == "Low"][, unique(seas)]
ed$whsp_rt[sev2 == "High/Moderate"][, unique(seas)]
print(ed$whsp_rt[seas == "2014-15", ])

# Generate High/Moderate-Severity Curves ------------------------------------

hmhc <- simdist(
  nreps = 1000,
  seed = 1983745,
  gimme = "everything",
  sim_args = list(
    severity2 = "High/Moderate",
    lamb_val = 25,
    hstdat = ed$whsp_rt
  )
)

names(hmhc)
hc_sevdraw <- sapply(hmhc$hc, function(x) x$sample$severity)
table(hc_sevdraw)

# view curves
ggplot(hmhc$outhc, aes(x = week, y = prediction)) +
  geom_line(aes(group = cid), alpha = 0.5) +
  labs(
    title = "Hypothetical Hospitalization Curves",
    subtitle = "High-to-moderate severity seasons"
  ) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())


# Generate Low-Severity Curves ----------------------------------------------

lhc <- simdist(
  nreps = 1000,
  seed = 98370,
  gimme = "everything",
  sim_args = list(
    severity2 = "Low",
    lamb_val = 40
  )
)
names(lhc)
lc_sevdraw <- sapply(lhc$hc, function(x) x$sample$severity)
table(lc_sevdraw)

# view curves
ggplot(lhc$outhc, aes(x = week, y = prediction)) +
  geom_line(aes(group = cid), alpha = 0.5) +
  labs(
    title = "Hypothetical Hospitalization Curves",
    subtitle = "Low-severity seasons"
  ) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())

# Compare H/M and Low Curve Sets --------------------------------------------

hmhc$outhc[, sevtype := "High/Moderate"]
lhc$outhc[, sevtype := "Low"]

all_hc <- as.data.table(rbind(hmhc$outhc, lhc$outhc))
all_hc[, .(pred = mean(prediction)), by = c("sevtype", "week")]

ggplot(all_hc, aes(x = week, y = prediction)) +
  geom_line(aes(group = paste(sevtype, cid))) +
  facet_wrap(~sevtype) +
  theme_clean(base_size = 15) +
  theme(axis.ticks.y = element_blank())
