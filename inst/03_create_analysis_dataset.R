# %% SETUP ---------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
  ggplot2,
  ggthemes
)

# Data

clndir <- here::here("data", "cleaned")
sim_lm <- readRDS(paste0(clndir, "/hypothetical-curves_lambda-min.Rds"))
sim_1se <- readRDS(paste0(clndir, "/hypothetical-curves_lambda-1se.Rds"))


# %% CHECK WEEKINT RANGES ------------------------------------------------------

sim_lm$outhc[, .(weekrange = paste0(min(weekint), ",", max(weekint))), cid] %>%
  .[, .N, weekrange]


sim_lm$outhc[weekint == 1, .N, by = template][, P := round(N/sum(N), 4)][]


make_analytic_dataset <- function(sim, lambda_type) {

  # %% WIDE AND LONG DATA SETS -------------------------------------------------

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
    theme_clean(base_size = 18) +
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


  # %% AUGMENT DATA_WIDE ------------------------------------------------------

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

  format(object.size(dat_wide_list), units = "MB")


  # set template to factor
  for (i in seq_along(dat_wide_list)) {
    dat_wide_list[[i]][, template := as.factor(template)]
  }

  for (i in seq_along(dat_wide_list)) {
    str(dat_wide_list[[i]]) %>% print
  }

  dat_long[, template := as.factor(template)]
  dat_wide[, template := as.factor(template)]


  ### hc3xdchl1 = hosprate_100k_3 * diff_cumhosp_lag1
  ### ch3xdhrl3 = cumhosp_100k_3 * diff_hosprate_lag3

  dat_wide_list2 <- lapply(1:30, function(x) {

    # pull dataset for week x
    curr <- dat_wide_list[[x]]

    if (x > 1) {
      #browser()
      currnames <- names(curr)

      # get all variable names beginning with "hosprate" and save
      # the name of the current week's hosprate separately
      hr_vars <- currnames[grepl("^hosprate", currnames)]
      hr_curr <- hr_vars[length(hr_vars)]

      # do the same for cumhosp variable
      ch_vars <- currnames[grepl("^cumhosp\\_100", currnames)]
      ch_curr <- ch_vars[length(ch_vars)]

      # get all diff variables 
      hr_diff <- currnames[grepl("^diff_hosprate", currnames)]
      ch_diff <- currnames[grepl("^diff_cumhosp", currnames)]

      # create interactions between hr_curr and cumhosp diffs
      hr_ixnames <- paste0("hr", x, "xdchl", 1:length(ch_diff))
      ch_ixnames <- paste0("ch", x, "xdhrl", 1:length(hr_diff))

      curr[, (hr_ixnames) :=
               lapply(hr_diff, function(x) get(x) * get(ch_curr))]

      curr[, (ch_ixnames) :=
               lapply(ch_diff, function(x) get(x) * get(hr_curr))]
    }

    return(curr)
  })

  # check names
  sapply(dat_wide_list2, names)


  # %% WRITE DATASET -------------------------------------------------------------

  fwrite(
    dat_long,
    here::here(
      "data",
      "cleaned",
      paste0("sim_dataset_long_", lambda_type, ".csv")
    )
  )

  fwrite(
    dat_wide,
    here::here(
      "data",
      "cleaned",
      paste0("sim_dataset_wide_", lambda_type, ".csv")
    )
  )


  slugs <- stringr::str_pad(1:30, width = 2, "left", pad = "0")

  for (i in 1:30) {
    saveRDS(
      dat_wide_list2[[i]],
      here::here(
        "data", "cleaned",
        paste0(
          "sim_dataset_analytic_", lambda_type, "_week_", slugs[i], ".Rds"
        )
      )
    )
  }

}


# %% Make and Write Datasets ---------------------------------------------------

make_analytic_dataset(sim = sim_lm, "lambda-min")
make_analytic_dataset(sim = sim_1se, "lambda-1se")
