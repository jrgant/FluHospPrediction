## NOTE
## This is a maintenance file that retains only the discrete superlearners from
## each _compfits-tagged file in the $TARGET-ProspObs/ProspSim/ directories.
## The original files are too large to commit to the Github repo.

pacman::p_load(
          sl3, data.table, magrittr,
          foreach, doParallel,
          stringr
        )

dirs <- here::here(
  "results",
  paste0(c("PeakRate-", "PeakWeek-", "CumHosp-"), "ProspObs/ProspSim")
)

cfits <- unlist(lapply(dirs, list.files, pattern = "compfits", full.names = T))

## This function takes a compfit object, identifies the name of the discrete
## super learner and retains only that model's fit.
prunecf <- function(cfobj) {
  tmp <- readRDS(cfobj)
  risks <- tmp$cvrisks[learner != "SuperLearner"]
  dsl <- risks[mean_risk == min(mean_risk), learner]

  ## If more than one super learner have the minimum prediction risk
  ##  - choose the learner with the lowest maximum risk across folds
  ##    If we still have more than one learner, sample from the remaining
  ##    learners at random.
  if (length(dsl) > 1) {
    dsllomax <- risks[learner %in% dsl
                      ][fold_max_risk == min(fold_max_risk), learner]

    if (length(dsllomax) > 1) {
      dsl <- sample(dsllomax, size = 1)
    } else {
      dsl <- dsllomax
    }
  }

  dslfit <- tmp$learner_fits[[dsl]]

  dslout <- list(
    comprisks = risks,
    fit = dslfit
  )

  dslout
}

## Loop through and process files in parallel

ifelse(detectCores() >= length(cfits)) {
  ncores <- detectCores()
} else {
  stop("Check number of cores.")
}

registerDoParallel(ncores)

pfits <- foreach(i = seq_along(cfits), .packages = "stringr") %dopar% {
  filepath <- cfits[i]
  target <- str_extract(filepath, "PeakRate|PeakWeek|CumHosp")
  season <- str_extract(filepath, "20[0-9]{2}\\-[0-9]{2}")
  week <- str_extract(filepath, "(?<=_w)[0-9]{2}")
  pruneobj <- prunecf(filepath)
  list(target = target, season = season, week = week, dsl = pruneobj)
}

## split into groups by prediction target
tvec <- sapply(pfits, function(.x) .x$target)
pkrate_pfits <- pfits[which(tvec == "PeakRate")]
pkweek_pfits <- pfits[which(tvec == "PeakWeek")]
cumhosp_pfits <- pfits[which(tvec == "CumHosp")]

## check
all(sapply(pkrate_pfits, function(.x) .x$target == "PeakRate"))
all(sapply(pkweek_pfits, function(.x) .x$target == "PeakWeek"))
all(sapply(cumhosp_pfits, function(.x) .x$target == "CumHosp"))

## write files
saveRDS(pkrate_pfits, here::here("results/PeakRate-ProspObs/ProspSim/dslfits.Rds"))
saveRDS(pkweek_pfits, here::here("results/PeakWeek-ProspObs/ProspSim/dslfits.Rds"))
saveRDS(cumhosp_pfits, here::here("results/CumHosp-ProspObs/ProspSim/dslfits.Rds"))
