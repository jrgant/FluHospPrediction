# Setup ----------------------------------------------------------------------

# %% Packages
pacman::p_load(data.table, flumodelr, prophet)

# %% Load Data

# hypothetical curves
hhc <- readRDS("data/hypothetical-curves.Rds")
names(hhc)

setDT(hhc$outhc)
names(hhc$outhc)

# save training data to a global df
tr <- hhc$train
names(tr)
sapply(tr, head)

# Serfling Model -------------------------------------------------------------

# %%
fd <- fludta %>%
  filter(year >= 2000)

fd <- fd %>%
  mutate(
    theta = 2 * week / 52,
    sin_2 = sinpi(theta),
    cos_2 = cospi(theta),
    noninf = rnorm(nrow(.), 0, 5)
  )
print(fd)

# %%
fitlm1 <- lm(fludeaths ~ week + sin_2 + cos_2, data = fd)
summary(fitlm1)


library(MASS)
step1 <- stepAIC(fitlm1, trace = TRUE)

pred1 <- predict(fitlm1, newdata = fd)
fd$yhat1 <- pred1

setDT(fd)
print(fd)

# %%
ggplot(fd, aes(x = week_in_order)) +
  geom_line(aes(y = fludeaths), color = "red") +
  geom_line(aes(y = yhat1)) +
  geom_line(aes(y = yhat2), linetype = "dashed")


# Modified Serfling Model ----------------------------------------------------

## flumodelr::flum

# Prophet Model --------------------------------------------------------------

## prophet::prophet
