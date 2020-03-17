# %% SETUP ---------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
  ggplot2,
  ggthemes
)

# Data

clndir <- here::here("data", "cleaned")
sim <- readRDS(paste0(clndir, "/hypothetical-curves.Rds"))

str(sim$outhc)

# @TODO 2020-03-17:
# - Get rid of empirical dataset with viral percentage, ILI, etc.
# - Get rid of training and test sets in sim$outhc; using cross-validation now

# %% CHECK WEEKINT RANGES ------------------------------------------------------

sim$outhc[, .(weekrange = paste0(min(weekint), ",", max(weekint))), cid] %>%
  .[, .N, weekrange]


# %% ANALYTIC DATA SETS --------------------------------------------------------

simd <- sim$outhc[, .(cid, weekint, hosprate_100k = prediction)] %>%
  .[, cumhosp_100k := cumsum(hosprate_100k), by = cid]
print(simd)

simd_targ_pkrate <- simd[, .(pkrate = max(hosprate_100k)), keyby = cid]

simd_targ_pkweek <- simd[, .(
  pkweek = weekint[hosprate_100k == max(hosprate_100k)]
  ), keyby = cid]

simd_targ_cumhosp <- simd[, .(
  cumhosp = cumhosp_100k[weekint == 30]
  ), keyby = cid]

print(simd_targ_pkrate)
print(simd_targ_pkweek)
print(simd_targ_cumhosp)

# combine targets
targets <- simd_targ_pkrate[simd_targ_pkweek[simd_targ_cumhosp]]

print(targets)

# plot densities

theme_set(
  theme_clean() +
  theme(axis.title = element_text(face = "bold"))
)

ggplot(targs, aes(x = pkrate)) +
  geom_density(
    fill = "gray",
    color = "gray"
  )

ggplot(targs, aes(x = pkweek)) +
  geom_density(
    fill = "gray",
    color = "gray"
  )

ggplot(targs, aes(x = cumhosp)) +
  geom_density(
    fill = "gray",
    color = "gray"
  )

# Weekly datasets

simd_bycid <- dcast(
  simd,
  cid ~ weekint,
  value.var = c("hosprate_100k", "cumhosp_100k")
)

dat <- merge(simd_bycid, targets, by = "cid")
head(dat)

# %% WRITE DATASET -------------------------------------------------------------

fwrite(dat, here::here("data", "cleaned", "analysis_dataset.csv"))
