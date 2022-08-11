################################################################################
## SETUP ##
################################################################################

library(pacman)
p_load(
  FluHospPrediction,
  sl3,
  e1071,
  data.table,
  stringr,
  foreach,
  doParallel
)


################################################################################
## READ IN SIMULATED CURVES ##
################################################################################

simfn <- list.files("data/cleaned/", pattern = "lambda-min_w", full.names = T)
simnm <- str_extract(simfn, "week_[0-9]{2}")

sim <- lapply(setNames(simfn, simnm), readRDS)

## x        = integer indicating week of flu season
## target   = prediction target
quicktask <- function(x, target) {
  suppressWarnings(fhp_make_task(target, x, lambda_type = "lambda-min"))
}

lobj <- setNames(seq_along(sim), names(sim))
ptask_pr <- lapply(lobj, quicktask, target = "pkrate")
ptask_pw <- lapply(lobj, quicktask, target = "pkweek")
ptask_ch <- lapply(lobj, quicktask, target = "cumhosp")


################################################################################
## SUPER LEARNERS FIT ON OBSERVED DATA ##
################################################################################

sldirs <- list.files("results", pattern = "ObsAllSeasons", full.names = T)

slfn <- lapply(
  setNames(sldirs, str_extract(sldirs, "(?<=results/).*(?=\\-)")),
  list.files,
  full.names = T
)

## slpaths  = vector of file paths to super learner fits (one for each week)
## ptasks   = vector of prediction tasks (one for each week)
get_sl_preds <- function(slpaths, ptasks) {

  snames <- str_extract(slpaths, "[0-9]{2}")
  pnames <- str_extract(names(ptasks), "[0-9]{2}")

  if (length(snames) == length(pnames)) {
    if (!(all(snames == pnames))) stop("Vector names don't match.")
  } else {
    stop("Vectors not the same length.")
  }

  ## pull files in parallel
  pkglist <- c("sl3", "FluHospPrediction", "e1071", "data.table")
  ncores <- detectCores() - 1
  registerDoParallel(ncores)

  slpreds <-
    foreach(
      i = seq_len(length(snames)),
      .packages = pkglist,
      .combine = rbind
    ) %dopar% {
      slfit <- readRDS(slpaths[i])
      data.table(
        slpred = slfit$predict(ptasks[[i]]),
        ysim = ptasks[[i]]$Y,
        fold = ptasks[[i]]$get_data()[, template_numeric],
        week = sprintf("%02d", i)
      )
    }

  slpreds[, abserr := abs(slpred - ysim)]
  slpreds
}


slp_pr <- get_sl_preds(slfn$PeakRate, ptask_pr)
slp_pw <- get_sl_preds(slfn$PeakWeek, ptask_pw)
slp_ch <- get_sl_preds(slfn$CumHosp, ptask_ch)



saveRDS(slp_pr, file.path(sldirs[sldirs %like% "PeakRate"], "slpreds.Rds"))
saveRDS(slp_pw, file.path(sldirs[sldirs %like% "PeakWeek"], "slpreds.Rds"))
saveRDS(slp_ch, file.path(sldirs[sldirs %like% "CumHosp"], "slpreds.Rds"))
