library(glmgen)

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

# check factor levels
levels(ts$year)
levels(ts$month)

library(ggplot2)
library(viridis)
ggplot(ts, aes(x = factor(month),
               y = deaths,
               color = factor(year))) +
  geom_line(aes(group = factor(year))) +
  scale_color_viridis_d(option = "B") +
  labs(title = "Testing trendfilter() using MASS::accdeaths") +
  theme_clean()


# k = 2 uses the quadratic trend filter as in Brook et al.
tf_mod <- trendfilter(x = ts$month, y = ts$deaths, 
                      k = 2, verbose = T)       

seas <- split(ts, ts$year)
tf_seas <- lapply(seas, function(x) trendfilter(x = x$month, y = x$deaths, k = 2))

tf_pred <- purrr::map2(seas, tf_seas, function(x, y) predict(y, x.new = x$month, lambda = y$lambda[15]))

taus <- purrr::map2(seas, tf_pred, function(x, y) {
  obs <- x$deaths
  pred <- as.vector(y)
  error <- (obs - pred)^2
  
  df <- data.frame(obs, pred = pred, sqerr = error)
  mnsqerr <- mean(df$sqerr)
  
  list(preds = df, mean.squared.error = mnsqerr)
})

pred_plot_dat <- map2_dfr(taus, 1973:1978, function(x, y) {
  df <- x[[1]]
  df$year <- y
  df$month <- 1:12
  df
})

pred_plot_dat

ggplot(pred_plot_dat,
       aes(x = factor(month),
           group = factor(year))) +
  geom_line(aes(y = obs), color = "red") + 
  geom_line(aes(y = pred), color = "black") +
  geom_point(aes(y = obs), color = "red") +
  geom_point(aes(y = pred), color = "black") +
  facet_wrap(~factor(year)) +
  labs(x = "Month\n", y = "Deaths", caption = "Black, predicted deaths using quadratic trend filtering; Red, observed deaths") +
  theme_clean() +
  theme(plot.caption = element_text(hjust = 0, 
                                    face = "italic"),
        strip.text = element_text(face = "bold"))
