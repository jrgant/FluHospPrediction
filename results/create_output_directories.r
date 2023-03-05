library(data.table)
library(magrittr)

resdir <- here::here("results")

targets <- c("pkrate" = "PeakRate",
             "pkweek" = "PeakWeek",
             "cumhosp" = "CumHosp")

analysis <- c("LambdaMin",
              "LambdaSE",
              "ElastNetRF",
              "SqErrLoss",
              "ProspObs",
              "ObsAllSeasons")

# CREATE FOLDERS IN RESULTS DIRECTORY IF THEY DON'T EXIST ----------------------

folder_names <- sapply(targets, function(x) paste(x, analysis, sep = "-")) %>%
  unname() %>%
  as.vector()

for (i in seq_len(length(folder_names))) {
  flpath <- file.path(resdir, folder_names[i])
  if (!dir.exists(flpath)) {
    dir.create(flpath)
    if (flpath %like% "LambdaMin|LambdaSE|Elast|SqErrLoss") {
      dir.create(file.path(flpath, "EnsembleCV"))
    } else if (flpath %like% "ProspObs") {
        dir.create(file.path(flpath, "Observed"))
        dir.create(file.path(flpath, "ProspSim"))
    }
  } else if (dir.exists(flpath)) {
      cat("Directory exists: ", flpath, "\n")
  }
}
