pacman::p_load(data.table, stringr, magrittr)

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

## look for bad weeks or incomplete prediction sets
filetab <- lapply(setNames(jobs$folder, jobs$folder), function(.x) {
  lf <- list.files(here::here("results", .x, "EnsembleCV"), pattern = "Rds")
  data.table(fn = lf)
}) %>% rbindlist(idcol = "analysis")

filetab[, ":=" (
  holdout = as.numeric(str_extract(fn, "(?<=holdout-)[0-9]{2}")),
  week = as.numeric(str_extract(fn, "(?<=week-)[0-9]{2}"))
)]

filetab

sqeweeks <- c(1, seq(5, 30, by = 5))
checkref <- lapply(unique(filetab$analysis), function(.x) {
  acurr <- .x
  if (acurr %like% "SqErr") {
    out <- expand.grid(analysis = acurr, holdout = 1:15, week = sqeweeks)
  } else {
    out <- expand.grid(analysis = acurr, holdout = 1:15, week = 1:30)
  }
  as.data.table(out)
}) %>% rbindlist()

if (nrow(checkref) == nrow(filetab)) stop("All jobs present.")

# If the value for the filename variable is missing, we do not have a file
# corresponding to the job spec in checkref. We need to rerun these jobs.
reruns_long <- merge(
  checkref,
  filetab,
  by = c("analysis", "holdout", "week"),
  all.x = TRUE
)[is.na(fn), -c("fn")]

reruns_long[, ":=" (
  target = fcase(
    analysis %like% "PeakRate", "pkrate",
    analysis %like% "PeakWeek", "pkweek",
    analysis %like% "CumHosp", "cumhosp"
  ),
  sh = fcase(
    analysis %like% "LambdaMin", "RUN_ENSEMBLE_CV.sh",
    analysis %like% "LambdaSE", "RUN_ENSEMBLE_CV_1se.sh",
    analysis %like% "ElastNetRF", "RUN_ENSEMBLE_CV_ElastNetRF.sh",
    analysis %like% "SqErrLoss", "RUN_ENSEMBLE_CV_SqErrLoss.sh"
  )
)]

reruns <- reruns_long[, .(week = paste(week, collapse = ",")),
                      by = .(analysis, target, holdout, sh)]

# build up a bash command that submits the missing week jobs
# for corresponding analyses
makecmd <- function(sh, holdout, week, target, time = "4:30:00") {
  goto <- "cd ~/fhp/scripts &&"
  basecmd <- paste0(
    "cat ", sh, " | egrep '", target, ".*LATE=", holdout, " ' | ",
    "sed 's/GB/GB --array=", week,
    ifelse(!is.null(time), paste(" -t", time), ""),
    "/'"
  )
  paste(goto, basecmd, "| sh")
}

# cobble together the bash commands for each job
reruns[, cmd := makecmd(sh, holdout, week, target, time = "5:30:00")][]

## submit jobs to SLURM in order of analysis priority
sapply(reruns[analysis %like% "LambdaMin", cmd], system)
sapply(reruns[analysis %like% "ElastNetRF", cmd], system)
sapply(reruns[analysis %like% "SqErrLoss", cmd], system)
sapply(reruns[analysis %like% "LambdaSE", cmd], system)
