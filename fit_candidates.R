# Setup ----------------------------------------------------------------------

# %% Packages
pacman::p_load(data.table, flumodelr, prophet, ggthemes, ggplot2)

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

# %% Chain Seasons
nchains <- 850
cidlist <- 1:max(tr$trainset$cid)


# @NOTE 2019-08-30:
#   Each sample of season IDs stored in a matrix column
set.seed(895093)
tsamp <- replicate(n = nchains, sample(cidlist, size = 6, replace = TRUE))

chains <- lapply(setNames(1:nchains, paste0("chain", 1:nchains)), function(x) {
  schain <- tr$trainset[cid %in% tsamp[, x]]
  schain$week_order <- 1:nrow(schain)
  return(schain)
})

sapply(chains, nrow)
class(chains)

# %% plot
ggplot(chains$chain4, aes(x = week_order, y = prediction)) +
  geom_line() +
  scale_x_continuous("Week", labels = rep(1:31, 6)) +
  theme_clean(base_size = 15)

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
