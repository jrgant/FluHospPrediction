pacman::p_load(data.table, stringr)

jobs <- as.data.table(
  expand.grid(
    analysis = c("min_H", "1se", "ElastNet", "SqErrLoss"),
    target = c("pkrate", "pkweek", "cumhosp"),
    stringsAsFactors = F
  )
)

## Specify folder names

# main
jobs[analysis == "min_H" & target == "pkrate", folder := "PeakRate-LambdaMin"]
jobs[analysis == "min_H" & target == "pkweek", folder := "PeakWeek-LambdaMin"]
jobs[analysis == "min_H" & target == "cumhosp", folder := "CumHosp-LambdaMin"]

# 1se
jobs[analysis == "1se" & target == "pkrate", folder := "PeakRate-LambdaSE"]
jobs[analysis == "1se" & target == "pkweek", folder := "PeakWeek-LambdaSE"]
jobs[analysis == "1se" & target == "cumhosp", folder := "CumHosp-LambdaSE"]

# elastnetrf
jobs[analysis == "ElastNet" & target == "pkrate", folder := "PeakRate-ElastNetRF"]
jobs[analysis == "ElastNet" & target == "pkweek", folder := "PeakWeek-ElastNetRF"]
jobs[analysis == "ElastNet" & target == "cumhosp", folder := "CumHosp-ElastNetRF"]

# sqerrloss
jobs[analysis == "SqErrLoss" & target == "pkrate", folder := "PeakRate-SqErrLoss"]
jobs[analysis == "SqErrLoss" & target == "pkweek", folder := "PeakWeek-SqErrLoss"]
jobs[analysis == "SqErrLoss" & target == "cumhosp", folder := "CumHosp-SqErrLoss"]

## copy jobs from ~/scratch to correct project results folder
for (i in seq_len(nrow(jobs))) {
  sfiles <- list.files(
    "~/scratch",
    pattern = paste0(jobs[i, target], ".*", jobs[i, analysis]),
    full.names = T
  )

  cvfiles <- list.files(sfiles, full.names = T, recursive = T)

  cvdest <- here::here("results", jobs[i, folder], "EnsembleCV")
  if (!dir.exists(cvdest)) dir.create(cvdest)

  file.copy(cvfiles, cvdest)

  cat("Files copied: \n")
  print(cvfiles)
}


## look for bad weeks or incomplete prediction sets
ensemble_cv <- lapply(setNames(jobs[, folder], jobs[, folder]), function(.x) {

  folder <- here::here("results", .x, "EnsembleCV")
  cvf <- list.files(folder)

  preds <- lapply(
    setNames(cvf, str_remove(cvf, "\\.Rds")),
    function(.y) {

      tmp <- readRDS(file.path(folder, .y))

      data.table(
        holdout_season = str_extract(.y, "(?<=out\\-)[0-9]{2}"),
        week = str_extract(.y, "(?<=week\\-)[0-9]{2}"),
        pred = tmp$sl_pred,
        outcome = tmp$holdout_outcome,
        pred_error = tmp$sl_pred_abserr,
        ## original files have squared error in the Squared Error Loss sensitivity
        ## labeled as absolute error
        error_type = ifelse(.x %like% "SqErrLoss", "squared", "absolute")
      )
    }
  )

  rbindlist(preds)
})

ecvl <- rbindlist(ensemble_cv, idcol = "analysis")

npred_byweek <- ecvl[, .N, .(analysis, holdout_season, week)]

nbwl <- dcast(npred_byweek, analysis + holdout_season ~ week, value.var = "N")

nbwl_byah <- split(nbwl, by = c("analysis", "holdout_season"))

missweeks <- lapply(setNames(nbwl_byah, names(nbwl_byah)), function(.x) {
  vec <- unlist(.x[, -c("analysis", "holdout_season")])
  which(is.na(vec))
})

## remove weeks we didn't analyze in SqErrLoss sensitivity
sqenames <- names(missweeks)[grepl("SqErrLoss", names(missweeks))]

for (i in seq_along(sqenames)) {
  nm <- sqenames[i]
  missweeks[[nm]] <- missweeks[[nm]][missweeks[[nm]] %in% c(1, 5, 10, 15, 20, 25, 30)]
}

missnames <- names(missweeks)[which(sapply(missweeks, length) > 0)]

rerun <- rbindlist(
  lapply(
    setNames(missnames, missnames),
    function(.x) {
      data.table(weeks = paste(missweeks[[.x]], collapse = ","))
  }), idcol = "analysis"
)

rerun[, holdout := as.numeric(str_extract(analysis, "[0-9]{2}"))]

rerun[analysis %like% "PeakRate", target := "pkrate"]
rerun[analysis %like% "PeakWeek", target := "pkweek"]
rerun[analysis %like% "CumHosp",  target := "cumhosp"]

# build up a bash command that submits the missing week jobs
# for corresponding analyses
makecmd <- function(sh, holdout, weeks, target, time = "4:30:00") {
  goto <- "cd ~/fhp/scripts &&"
  basecmd <- paste0(
    "cat ", sh, " | egrep '", target, ".*LATE=", holdout, " ' | ",
    "sed 's/GB/GB --array=", weeks,
    ifelse(!is.null(time), paste(" -t", time), ""),
    "/'"
  )

  paste(goto, basecmd, "| sh")
}

# cobble together the bash commands for each job
rerun[
  analysis %like% "LambdaMin",
  cmd := makecmd(sh = "RUN_ENSEMBLE_CV.sh", holdout, weeks, target)
]

rerun[
  analysis %like% "LambdaSE",
  cmd := makecmd(sh = "RUN_ENSEMBLE_CV_1se.sh", holdout, weeks, target)
]
rerun[
  analysis %like% "ElastNetRF",
    cmd := makecmd(sh = "RUN_ENSEMBLE_CV_ElastNetRF.sh", holdout, weeks, target)
]
rerun[
  analysis %like% "SqErrLoss",
    cmd := makecmd(sh = "RUN_ENSEMBLE_CV_SqErrLoss.sh", holdout, weeks, target)
]

## submit jobs to SLURM in order of analysis priority
lapply(rerun[analysis %like% "LambdaMin", cmd], system)
lapply(rerun[analysis %like% "ElastNetRF", cmd], system)
lapply(rerun[analysis %like% "SqErrLoss", cmd], system)
lapply(rerun[analysis %like% "LambdaSE", cmd], system)
