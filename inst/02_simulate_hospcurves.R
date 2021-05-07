# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol
# 2015;11:e1004382. doi:10.1371/journal.pcbi.1004382.


# %% Load packages ----------------------------------------------------------

suppressMessages(library(FluHospPrediction))
library(extrafont)
loadfonts()

# data directories
rawdir <- here::here("data", "raw")
clndir <- here::here("data", "cleaned")


# %% Load data -------------------------------------------------------------

# Setup
ed <- fread(paste0(clndir, "/empdat.csv"))
class(ed)
print(head(ed))

# Drop Pandemic Flu
ed <- ed[season != "2009-10"]
table(ed$season)

# check for missing data (all 0 = no missing data)
ed[, lapply(.SD, is.na)] %>%
  .[, lapply(.SD, sum)] %>%
  melt %>%
  print


# %% View Historical Curves -------------------------------------------------

sublab <- "2003–2019, excludes 2009–2010 season"
sourcecap <- "Source: FluSurv-NET (Emerging Infections Program)"

theme_tweak <-
    theme_clean(base_size = 15) +
    theme(
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 10, face = "italic"),
      plot.background = element_blank()
    )

emp_hosp <- ggplot(ed, aes(x = weekint, y = weekrate)) +
  geom_line(
    aes(group = season),
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


# %% Trend Filter -----------------------------------------------------------

# Split Observed Seasons
# each gets its own data.frame
seas_obs <- split(ed, ed$season)
print(seas_obs)

# run trendfilter on each observed season
tf_seas <- lapply(seas_obs, function(x) {
  trendfilter(
    y = x$weekrate,
    ord = 1  # 1 = linear, 2 = quadratic
  )
})

# view model summaries
lapply(tf_seas, class)

# get lambda values for each season's trendfilter fit
# cv.trendfilter() picks the lambda that minimizes the cross-validated error
# 5 folds used in CV (k = 5)
cv_tf <- lapply(tf_seas, function(x) {
  cv.trendfilter(x, mode = "lambda", k = 5)
})


# %% Predict: Minimizing Lambda ------------------------------------------------

# predict the weekly count (y) and error (tau) for each season
tf_pred <- predict_curves(
  hosp_obs = seas_obs,
  tf_list = tf_seas,
  cv_list = cv_tf,
  lambda_type = "lambda.min"
)

sapply(tf_pred, function(x) class(x$dat))
print(tf_pred)

# make sure correct lambda used for each season
check.lambda <- data.table(
  pred.lambda = sapply(tf_pred, function(x) x$lambda),
  cv.lambda = sapply(cv_tf, function(x) x$lambda.min)
)[, same := pred.lambda == cv.lambda]

check.lambda

tfp <- rbindlist(lapply(tf_pred, function(x) x[[1]]))

# confirm correct weekint numbering
sapply(names(tf_pred), function(x) range(tfp[season == x, range(weekint)]))


# %% Predict: Lambda 1SE -------------------------------------------------------

tf_pred_1se <- predict_curves(
  hosp_obs = seas_obs,
  tf_list = tf_seas,
  cv_list = cv_tf,
  lambda_type = "lambda.1se"
)

sapply(tf_pred_1se, function(x) class(x$dat))

# make sure correct lambda used for each season
check.lambda.1se <- data.table(
  pred.lambda.1se= sapply(tf_pred_1se, function(x) x$lambda),
  cv.lambda.1se = sapply(cv_tf, function(x) x$lambda.1se)
)[, same := pred.lambda.1se == cv.lambda.1se]

tfp_1se <- rbindlist(lapply(tf_pred_1se, function(x) x[[1]]))

# confirm correct weekint numbering
sapply(
  names(tf_pred_1se),
  function(x) range(tfp_1se[season == x, range(weekint)])
)


# %% Prediction Plots ----------------------------------------------------------

plot.tf <- function(data, title) {
  data %>%
    ggplot(aes(x = weekint, y = obs.hosp1)) +
    geom_line(
      aes(y = pred.hosp, color = "Fit"),
      size = 0.4
    ) +
    geom_point(
      aes(y = obs.hosp1, shape = "Empirical"),
      color = "gray32",
      size = 0.7
    ) +
    facet_wrap(~season, ncol = 5) +
    scale_color_manual(name = "", values = "firebrick") +
    scale_shape_manual(name = "", values = 21) +
    labs(
      title = title,
      x = "Week of season",
      y = "Hospitalizations (per 100,000 population)"
    ) +
    theme_base(base_family = "Noto Serif", base_size = 12) +
    theme(
      legend.position = "top",
      plot.background = element_blank(),
      axis.text = element_text(size = 7),
      axis.ticks = element_line(color = "gray32", size = 0.5)
    )
}

tf_fit_facet <- plot.tf(tfp, "")
tf_fit_facet

tf1se_fit_facet <- plot.tf(tfp_1se, "")
tf1se_fit_facet

# S1 Figures

save.tf.plot <- function(plot, lambda.type, ht = 6, wd = 7.5) {

  fp <- file.path(
    "results", "00_paper_output",
    paste0("FIG_TF-Predictions_", lambda.type, "_", Sys.Date())
  )

  ggsave(
    paste0(fp, ".pdf"),
    plot = plot,
    device = "pdf",
    height = ht,
    width = wd,
    units = "in"
  )

  ggsave(
    paste0(fp, ".png"),
    plot = plot,
    device = "png",
    height = ht,
    width = wd,
    units = "in"
  )

}

save.tf.plot(tf_fit_facet, "lambda.min")
save.tf.plot(tf1se_fit_facet, "lambda.1se")


# %% Empirical Peak Rate and Peak Week -----------------------------------------

# record peak weeks from empirical data
dist_emp_peaks <- ed[, .(
    pkhosp = max(weekrate),
    pkweek = weekint[weekrate == max(weekrate)]
    ),
  by = "season" ]

print(dist_emp_peaks)


# %% Simulation Reusables ------------------------------------------------------

## # refit trendfilter but provide a predictor matrix for later use in
## # curve simulation (enables appropriate shifting). cv.trendfilter() requires
## # identity matrix to be used initially.
## tf_seas_pmat <- lapply(seas_obs, function(x) {
##   trendfilter(
##     y = x$weekrate,
##     X = diag(1:30, nrow = 30, ncol = 30),
##     ord = 1
##   )
## })


get_pred_peaks <- function(predlist) {
  data.table(
    season = names(predlist),
    pkhosp = sapply(
      predlist, function(x) max(x$dat[, pred.hosp])
    ) %>% round(., 1),
    pkweek = sapply(
      predlist, function(x) max(x$dat[pred.hosp == max(pred.hosp), weekint])
    )
  )
}

run_curvesim <- function(tf_peaks,
                         predlist,
                         lt,
                         empdat = ed,
                         reps = 15000,
                         seed) {

  # time the simulations
  tic(paste0("Curve simulations (n = ", reps, ")"))

  # @NOTE: Suppressing warnings for predictions out of range. Expected behavior
  #        due to the curve stretching. R is being asked to predict
  #        hospitalizations outside the range of observed weeks, which is what (in
  #        part) produces the curve shifts necessary to generate the hypothetical
  #        hospitalization curves. We map the predictions by index to the weekint
  #        range.
  out <- suppressWarnings(
    simdist(
      nreps = reps,
      seed = seed,
      gimme = "everything",
      sim_args = list(
        peakdist = tf_peaks,
        predfits = predlist,
        fitseas = tf_seas,
        nu.min = 0.75,
        nu.max = 1.25,
        cv_list = cv_tf,
        lambda_type = lt,
        hstdat = empdat
      )
    )
  )
  toc()

  out
}

# %% Generate Hypothetical Curves: Minimizing Lambda ---------------------------

# return peak weeks from trend filter fits
dist_tf_peaks <- get_pred_peaks(tf_pred)
dist_tf_peaks

hhc <- run_curvesim(
  tf_peaks = dist_tf_peaks,
  predlist = tf_pred,
  lt = "lambda.min",
  reps = 15000,
  seed = 5882300
)

names(hhc)
head(hhc$hc)


# check to make sure each season has 30 predicted periods
veclengths <- sapply(hhc$hc, function(x) length(x$eq$arg_f))
table(veclengths == 30)


# %% Generate Hypothetical Curves: Lambda 1SE ----------------------------------

dist_tf1se_peaks <- get_pred_peaks(tf_pred_1se)
dist_tf1se_peaks

hhc_1se <- run_curvesim(
  tf_peaks = dist_tf1se_peaks,
  predlist = tf_pred_1se,
  lt = "lambda.1se",
  reps = 15000,
  seed = 19711998
)

# check to make sure each season has 30 predicted periods
veclengths2 <- sapply(hhc_1se$hc, function(x) length(x$eq$arg_f))
table(veclengths2 == 30)


# %% Inspect Simulated Curves --------------------------------------------------

edsum <- ed[, .(pkht = max(weekrate),
                pkweekint = weekint[weekrate == max(weekrate)],
                cumhosp = sum(weekrate)),
            by = .(cid = season)] %>%
            # in some cases, peak height was the same across weeks
            # in all such cases, weeks were adjacent (take average peak week)
            .[, .(pkht = mean(pkht),
                  pkweekint = mean(pkweekint),
                  cumhosp = mean(cumhosp)),
              by = cid] %>%
            .[, data := "empirical"]

print(edsum)
names(hhc$outhc)
names(hhc_1se$outhc)

simsum <- hhc$outhc[, .(
  pkht = max(prediction),
  pkweekint = weekint[prediction == max(prediction)],
  cumhosp = sum(prediction)
), by = .(cid = as.character(cid))
][, data := "sim.lambda.min"]

simsum

simsum_1se <- hhc_1se$outhc[, .(
  pkht = max(prediction),
  pkweekint = weekint[prediction == max(prediction)],
  cumhosp = sum(prediction)
), by = .(cid = as.character(cid))
][, data := "sim.lambda.1se"]

simsum_1se

edsim <- rbind(edsum, simsum, simsum_1se)

theme_set(theme_clean())

ggplot(edsim, aes(x = data, y = pkht)) +
  geom_boxplot() +
  labs(title = "Peak height")

ggplot(edsim, aes(x = data, y = pkweekint)) +
  geom_boxplot() +
  labs(title = "Peak week")

ggplot(edsim, aes(x = data, y = cumhosp)) +
  geom_boxplot() +
  labs(title = "Cumulative hospitalizations")

# %%
# view curves
simsub <- 15

# every time this code block is run, a different subset of simulations
# is sampled at random
hyp_hosp_p <-
  ggplot(hhc$outhc[cid %in% sample(cid, size = simsub)],
         aes(x = weekint,
             y = prediction)) +
  geom_line(aes(group = cid, color = factor(cid)),
            alpha = 0.6) +
  coord_cartesian(y = c(0, 12)) +
  labs(x = "Week",
       y = "Predicted hospitalizations (per 100,000)",
       title = "Hypothetical Hospitalization Curves",
       subtitle = paste(simsub, "simulations")
      ) +
  theme_tweak +
  theme(legend.position = "none")

# view plot
hyp_hosp_p

# Check for unrealistic predictions at start and end of season
hhc$outhc[
  weekint == 1,
  .(max = max(prediction), min = min(prediction))
]

hhc$outhc[
  weekint == 30,
  .(max = max(prediction), min = min(prediction))
]

hhc$outhc %>%
  ggplot(aes(x = prediction, y = factor(weekint))) +
  geom_density_ridges() +
  theme_ridges()


# %% Compare Simulated Curve to their Empirical Templates ----------------------

simvemp <- list(
  empirical = ed[, .(template = season, weekrate, weekint, cid = season)],
  sim.lmin = hhc$outhc[, .(template, weekrate = prediction, weekint, cid)],
  sim.l1se = hhc_1se$outhc[, .(template, weekrate = prediction, weekint, cid)]
)

simvempl <- rbindlist(simvemp, idcol = "type")

simvempl[type != "sim.l1se"] %>%
  ggplot(aes(x = weekint, y = weekrate, group = cid)) +
  geom_line(
    data = simvempl[type == "sim.lmin"],
    alpha = 0.03,
    size = 0.5,
    color = "gray"
  ) +
  geom_line(
    data = simvempl[type == "empirical"],
    size = 1,
    color = "black"
  ) +
  facet_wrap(~ template)

simvempl[type != "sim.lmin"] %>%
  ggplot(aes(x = weekint, y = weekrate, group = cid)) +
  geom_line(
    data = simvempl[type == "sim.l1se"],
    alpha = 0.03,
    size = 0.5,
    color = "gray"
  ) +
  geom_line(
    data = simvempl[type == "empirical"],
    size = 1,
    color = "black"
  ) +
  facet_wrap(~ template)

simsum <- copy(simvempl[type != "empirical"])

simsum <- simsum[, .(
  peakrate = max(weekrate),
  peakweek = weekint[weekrate == max(weekrate)]
), .(cid, type, template)]

# summarize peak rate by lambda sim and season shape template
simsum[, .(
  mean = mean(peakrate),
  median = median(peakrate),
  sd = sd(peakrate),
  min = min(peakrate),
  max = max(peakrate)
), keyby = .(type, template)]

# summarize peak week by lambda sim and season shape template
simsum[, .(
  mean = mean(peakweek),
  median = median(peakweek),
  sd = sd(peakweek),
  min = min(peakweek),
  max = max(peakweek)
), keyby = .(type, template)]


ggplot(simsum, aes(x = template, y = peakrate)) +
  geom_boxplot() +
  facet_wrap(~type) +
  ggtitle("Peak rates by lambda version and shape template") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))

ggplot(simsum, aes(y = type, x = peakweek, fill = type)) +
  geom_density_ridges() +
  scale_fill_viridis_d(option = "magma") +
  facet_wrap(~template) +
  ggtitle("Peak weeks by lambda version and shape template") +
  theme_ridges()

ggplot(simsum, aes(y = type, fill = type, x = peakrate)) +
  geom_density_ridges() +
  scale_fill_viridis_d(option = "magma") +
  ggtitle("Peakrate by lambda type only") +
  theme_ridges()

ggplot(simsum, aes(y = type, fill = type, x = peakweek)) +
  geom_density_ridges() +
  scale_fill_viridis_d(option = "magma") +
  ggtitle("Peak week by lambda type only") +
  theme_ridges()


# %% Write Hypothetical Curves -------------------------------------------------

saveRDS(hhc, paste0(clndir, "/hypothetical-curves_lambda-min.Rds"))
saveRDS(hhc_1se, paste0(clndir, "/hypothetical-curves_lambda-1se.Rds"))


# %% View and Write Plots ------------------------------------------------------

curve_grid <- grid.arrange(emp_hosp, emp_cumr, hyp_hosp_p, nrow = 3)
