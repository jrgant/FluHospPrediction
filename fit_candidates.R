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


# Prophet Model --------------------------------------------------------------

## prophet::prophet
