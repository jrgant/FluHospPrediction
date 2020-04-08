# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol
# 2015;11:e1004382. doi:10.1371/journal.pcbi.1004382.


# %% Load packages ----------------------------------------------------------

suppressMessages(library(FluHospPrediction))
library(tictoc)

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
      plot.background = element_blank(),
      plot.margin = margin(rep(0.5, 4),  unit = "cm")
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
    x = x$weekint,
    y = x$weekrate,
    k = 1,
    family = "gaussian"
  )
})

# view model summaries
lapply(tf_seas, summary)

# %% Predict ----------------------------------------------------------------

# predict the weekly count (y) and error (tau) for each season

# choose lambda index (same for each trendfilter fit)
# used from here forward
sel_lambda <- 25

tf_pred <- predict_curves(
  hosp_obs = seas_obs,
  tf_list = tf_seas,
  tf_lambda_index = sel_lambda
)

sapply(tf_pred, function(x) class(x$dat))
print(tf_pred)

tfp <- rbindlist(lapply(tf_pred, function(x) x[[1]]))

# plot predictions vs. empirical data
tf_pred_plotlist <- lapply(unique(tfp$season), function(x) {

  ggplot(tfp[season == x, ], aes(x = weekint)) +
    geom_line(aes(y = pred.hosp), color = "red") +
    geom_point(
      aes(y = obs.hosp1),
      alpha = 0.5,
      size = 0.8
    ) +
    scale_color_viridis_d() +
    labs(
      x = glue::glue("Week of { x }"),
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
})

tf_pred_plotlist


# %% Generate hypothetical curves -------------------------------------------

# record peak weeks
dist_peaks <- ed[, .(
    pkhosp = max(weekrate),
    pkweek = weekint[weekrate == max(weekrate)]
    ),
  by = "season" ]

print(dist_peaks)


# %% Generate Hypothetical Curves ------------------------------------------

# number of curves to simulate
reps <- 15000

# Simulate Curves
# time the simulations (not required)
tic(paste0("Curve simulations (n = ", reps, ")"))

# @NOTE: Suppressing warnings for predictions out of range. Expected behavior
#        due to the curve stretching. R is being asked to predict
#        hospitalizations outside the range of observed weeks, which is what (in
#        part) produces the curve shifts necessary to generate the hypothetical
#        hospitalization curves. We map the predictions by index to the weekint
#        range.
hhc <- suppressWarnings(
  simdist(
    nreps = reps,
    seed = 1983745,
    gimme = "everything",
    sim_args = list(
      lambda_index = sel_lambda,
      hstdat = ed
    )
  )
)
toc()

names(hhc)
head(hhc$hc)

# check to make sure each season has 30 predicted periods
veclengths <- sapply(hhc$hc, function(x) length(x$eq$arg_f))
table(veclengths == 30)

names(ed)

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

simsum <-
  hhc$outhc[, .(pkht = max(prediction),
                pkweekint = weekint[prediction == max(prediction)],
                cumhosp = sum(prediction)), by = .(cid = as.character(cid))] %>%
  .[, data := "simulated"]

print(simsum)

edsim <- rbind(edsum, simsum)

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
  coord_cartesian(y = c(0, 10)) +
  labs(x = "Week",
       y = "Predicted hospitalizations (per 100,000)",
       title = "Hypothetical Hospitalization Curves",
       subtitle = paste(simsub, "simulations"),
       caption = paste0(
         "Linear trend filter \u03BB index",
         " = ", sel_lambda
         )
      ) +
  theme_tweak +
  theme(legend.position = "none")

# view plot
hyp_hosp_p


# @NOTE 2019-09-26:
#  Run some summaries on the hypothetical curves to check for errors
#  May still want to choose different lambda or conduct sensitivity around lambda
ggplot(hhc$outhc[weekint == 1, ], aes(x = prediction)) +
  geom_density() +
  labs(
    title = "Predicted hospitalization rates at Epiweek 40 (integer week 1)"
    ) +
  theme_clean()

summary(hhc$outhc$prediction[hhc$outhc$weekint == 1])

# Check for unrealistic predictions at start and end of season
hhc$outhc[weekint == 1, .(max = max(prediction),
                          min = min(prediction))]

hhc$outhc[weekint == 30, .(max = max(prediction),
                           min = min(prediction))]

hhc$outhc %>%
  ggplot(
    aes(x = prediction, y = factor(weekint))
  ) +
  geom_density_ridges() +
  theme_ridges()

# %% Write Hypothetical Curves ----------------------------------------------

saveRDS(hhc, paste0(clndir, "/hypothetical-curves.Rds"))

# %% View and Write Plots ------------------------------------------------

curve_grid <- grid.arrange(emp_hosp, emp_cumr, hyp_hosp_p, nrow = 3)
