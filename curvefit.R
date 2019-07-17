
# Load packages -------------------------------------------------------------

pacman::p_load(glmgen, ggplot2, ggthemes, viridis, purrr, tidyr)


# Load data -----------------------------------------------------------------

# convert MASS::accdeaths to data frame
ts <- as.data.frame(
  cbind(deaths = as.vector(MASS::accdeaths),
        year = rep(1973:1978, each = 12),
        month = 1:12))

# check variable classes
sapply(ts, class)

# check number of unique factor values
length(unique(ts$year))
length(unique(ts$month))



# Plot ----------------------------------------------------------------------

# Empirical curves
ggplot(ts, aes(x = factor(month),
               y = deaths,
               color = factor(year))) +
  geom_line(aes(group = factor(year))) +
  scale_color_viridis_d(option = "B") +
  labs(title = "Testing trendfilter() using MASS::accdeaths") +
  theme_clean()


# Quadratic Trend Filter ----------------------------------------------------

# k = 2 uses the quadratic trend filter as in Brooks et al.
tf_mod <- trendfilter(x = ts$month, 
                      y = ts$deaths, 
                      k = 2, 
                      verbose = T)       

# Split Seasons 
seas <- split(ts, ts$year)
tf_seas <- lapply(seas, function(x) { 
  trendfilter(x = x$month, y = x$deaths, k = 2) 
  })

# Predict 
# predict the weekly count (y) and error (tau) for each season
tf_pred <- map2(seas, tf_seas, function(x, y) { 
  predict(y, x.new = x$month, lambda = y$lambda[15]) 
  })

# get taus
taus <- map2(seas, tf_pred, function(x, y) {
  
  obs <- x$deaths
  pred <- as.vector(y)
  error <- (obs - pred)^2
  
  df <- data.frame(obs, pred = pred, sqerr = error)
  mnsqerr <- mean(df$sqerr)
  
  list(preds = df, mean.squared.error = mnsqerr)
})

taus

pred_plot_dat <- map2_dfr(taus, 1973:1978, function(x, y) {
  df <- x[[1]]
  df$year <- y
  df$month <- 1:12
  df
})

head(pred_plot_dat)

pgath <- pred_plot_dat %>% gather(type, deaths, -sqerr, -year, -month)

ggplot(pgath, aes(x = factor(month),
                  y = deaths,
                  group = type,
                  color = type)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(size = 0.5, alpha = 0.4) +
  facet_wrap(~year) +
  scale_color_manual(values = c("black", "red")) +
  theme_clean()


# Generate hypothetical curves ----------------------------------------------

tf_mod
