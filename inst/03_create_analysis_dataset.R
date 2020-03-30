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


# %% WIDE AND LONG DATA SETS ---------------------------------------------------

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


# %% AUGMENT DATA_WIDE --------------------------------------------------------

# the index of the list marks the integer week
dat_wide_list <- lapply(1:30, function(x, data = dat_wide) {

  # list columns to select in each iteration
  select_always <- c("cid", "template", "pkrate", "pkweek", "cumhosp")
  select_hosp <- c(paste0("hosprate_100k_", 1:x), paste0("cumhosp_100k_", 1:x))
  select_curr <- c(select_always, select_hosp)

  sub <- data[, ..select_curr]

  # create variables for lagged differences
  if (x <= 5 & x > 1) {

    diffcols_hosprate <- paste0("diff_hosprate_lag", 1:(x - 1))
    diffcols_cumhosp <- paste0("diff_cumhosp_lag", 1:(x - 1))

    for (i in 1:(x - 1)) {

      sub[, diffcols_hosprate[i] := (get(paste0("hosprate_100k_", x)) -
              get(paste0("hosprate_100k_", x - i))
          )]

      sub[, diffcols_cumhosp[i] := (get(paste0("cumhosp_100k_", x)) -
              get(paste0("cumhosp_100k_", x - i))
          )]
    }

  } else if (x > 5) {

      diffcols_hosprate <- paste0("diff_hosprate_lag", 1:5)
      diffcols_cumhosp <- paste0("diff_cumhosp_lag", 1:5)

      for (i in 1:5) {

        sub[, diffcols_hosprate[i] := (get(paste0("hosprate_100k_", x)) -
              get(paste0("hosprate_100k_", x - i))
            )]

        sub[, diffcols_cumhosp[i] := (get(paste0("cumhosp_100k_", x)) -
              get(paste0("cumhosp_100k_", x - i))
            )]
    }
  }

  return(sub)
})

dat_wide_list



# set template to factor
for (i in seq_along(dat_wide_list)) {
  dat_wide_list[[i]][, template := as.factor(template)]
}

for (i in seq_along(dat_wide_list)) {
  str(dat_wide_list[[i]]) %>% print
}

dat_long[, template := as.factor(template)]
dat_wide[, template := as.factor(template)]


# %% WRITE DATASET -------------------------------------------------------------

fwrite(dat_long, here::here("data", "cleaned", "sim_dataset_long.csv"))
fwrite(dat_wide, here::here("data", "cleaned", "sim_dataset_wide.csv"))

saveRDS(
  dat_wide_list,
  here::here("data", "cleaned", "sim_dataset_analytic.rds")
)
