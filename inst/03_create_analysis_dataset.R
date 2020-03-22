# %% SETUP ---------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
  ggplot2,
  ggthemes
)

# Data

clndir <- here::here("data", "cleaned")
sim <- readRDS(paste0(clndir, "/hypothetical-curves.Rds"))

names(sim)
sim$hc[[1]]
str(sim$outhc)

# @TODO 2020-03-17:
# - Get rid of empirical dataset with viral percentage, ILI, etc.
# - Get rid of training and test sets in sim$outhc; using cross-validation now


# %% CHECK WEEKINT RANGES ------------------------------------------------------

sim$outhc[, .(weekrange = paste0(min(weekint), ",", max(weekint))), cid] %>%
  .[, .N, weekrange]


sim$outhc[weekint == 1, .N, by = template][, P := N/sum(N)] %>% print


# %% ANALYTIC DATA SETS --------------------------------------------------------

simd <- sim$outhc[, .(cid, template, weekint, hosprate_100k = prediction)] %>%
  .[, cumhosp_100k := cumsum(hosprate_100k), by = cid]

print(simd)

# template lookup table
simd_templates <- simd[, .(template = unique(template)), keyby = cid]

# get peak rates
simd_targ_pkrate <- simd[, .(pkrate = max(hosprate_100k)), keyby = cid]

# get peak weeks
simd_targ_pkweek <- simd[, .(
  pkweek = weekint[hosprate_100k == max(hosprate_100k)]
  ), keyby = cid]

# get cumulative hospitalization rates
simd_targ_cumhosp <- simd[, .(
  cumhosp = cumhosp_100k[weekint == 30]
  ), keyby = cid]

print(simd_templates)
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

ggplot(targets, aes(x = pkrate)) +
  geom_density(
    fill = "gray",
    color = "gray"
  )

ggplot(targets, aes(x = pkweek)) +
  geom_density(
    fill = "gray",
    color = "gray"
  )

ggplot(targets, aes(x = cumhosp)) +
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

print(simd_bycid)

dat_long <- merge(simd, targets, by = "cid")
dat_wide <- merge(simd_bycid[simd_templates], targets, by = "cid")

print(dat_long)
print(dat_wide)


# %% WRITE DATASET -------------------------------------------------------------

fwrite(dat_long, here::here("data", "cleaned", "analysis_dataset_long.csv"))
fwrite(dat_wide, here::here("data", "cleaned", "analysis_dataset_wide.csv"))
