# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol 2015;11:e1004382.
# doi:10.1371/journal.pcbi.1004382.


# Load packages -------------------------------------------------------------

pacman::p_load(glmgen, ggplot2, ggthemes, viridis, purrr, tidyr, data.table)

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

simcrv <- function(
                   print.plot = FALSE,
                   print.samples = FALSE,
                   print.eq = FALSE,
                   verbose = FALSE,
                   peakdist = dist_peaks,
                   hstdat = ed$whsp_ct,
                   severity2 = NULL,
                   predfits = tf_pred,
                   fitseas = tf_seas,
                   lamb_val = 25) {

  # severity2 = one of "High/Moderate", "Low", or NULL

  if (!is.null(severity2)) {
    if (!severity2 %in% c("High/Moderate", "Low")) {
      messaging::emit_error(
        crayon::red(
          'severity2 must be one of "High/Moderate", "Low", or NULL'
        )
      )
    }
    hstdat <- hstdat[sev2 == severity2, ]
  }

  # sample shape (f)
  s <- sample(unique(hstdat$seas), 1)
  max_j <- peakdist[, pkhosp[seas == s]]

  # one season had peak hospitalizations occur in two separate weeks
  # to account for this, we sample one of the peak weeks at random for that
  # season
  pkw <- peakdist[, pkweek[seas == s]]
  argmax_j <- ifelse(
    length(pkw) > 1,
    pkw[sample(x = c(1, 2), size = 1)],
    pkw
  )

  # sample noise (sigma)
  sigma <- predfits[[s]]$tau

  # peak height (theta)
  theta <- runif(1, min(peakdist$pkhosp), max(peakdist$pkhosp))

  # peak week
  mu <- round(runif(1, min(peakdist$pkweek), max(peakdist$pkweek)))

  # pacing (nu)
  nu <- runif(1, 0.75, 1.25)

  slist <- list(
    "season" = s,
    "severity" = hstdat[, sev2[seas == s]] %>% unique(),
    "maxj" = max_j,
    "argmax" = argmax_j,
    "sigma" = sigma,
    "theta" = theta,
    "mu" = mu,
    "nu" = nu
  )

  if (print.samples | verbose) print(slist)

  # Calculate curve equation
  t1 <- theta / max_j
  arg_f <- ((1:31 - mu) / nu) + argmax_j

  f <- predict(fitseas[[s]],
    x.new = arg_f,
    lambda = fitseas[[s]]$lambda[lamb_val]
  )

  err <- rnorm(n = length(f), 0, sd = sqrt(sigma))
  fi <- t1 * f + err

  #  transformation to set the lower function bound to 0
  fi_tf <- sapply(fi, function(x) 0.5 * (abs(x) + x))

  eqlist <- list(
    "term1" = t1,
    "arg_f" = arg_f,
    "predictions" = f,
    "error" = err,
    "fi_pluserr" = fi,
    "fi_tf" = fi_tf
  )

  if (print.eq | verbose) print(eqlist)


  if (print.plot) {
    plot(fi_tf,
      type = "l", col = "red",
      main = substitute(
        paste(
          sigma, " = ", sig, ", ",
          theta, " = ", the, ", ",
          mu, " = ", mus, ", ",
          nu, " = ", nus
        ),
        list(
          sig = round(sigma, 2),
          the = round(theta, 2),
          mus = round(mu, 2),
          nus = round(nu, 2)
        )
      ),
      col.main = "navy",
      font.main = 2
    )
  }

  return(list(
    sample = slist,
    eq = eqlist
  ))
}


# Function to generate multiple curves -------------------------------------

simdist <- function(nreps,
                    seed = 1971,
                    severity,
                    gimme = NULL,
                    check = FALSE,
                    nrow = 10,
                    sim_args = list()) {
  set.seed(seed)

  hc <- replicate(nreps,
    do.call("simcrv",
      args = sim_args
    ),
    simplify = FALSE
  )

  outhc <-
    sapply(hc, function(x) {
      data.frame(week = x$eq$arg_f, prediction = x$eq$fi_tf)
    },
    simplify = FALSE
    ) %>%
    dplyr::bind_rows()

  setDT(outhc)
  outhc[, cid := rep(1:100, each = 31)]

  if (check) print(outhc, topn = nrow)

  if (!is.null(gimme)) {
    if (gimme == "everything") {
      return(list(
        hc = hc,
        outhc = outhc
      ))
    } else if (gimme == "hc") {
      hc
    }
  } else {
    outhc
  }
}


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
