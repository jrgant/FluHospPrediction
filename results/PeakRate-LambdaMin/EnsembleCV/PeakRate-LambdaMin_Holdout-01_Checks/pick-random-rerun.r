library(data.table)

prlmin_ensdir <- here::here(
  "results",
  "PeakRate-LambdaMin",
  "EnsembleCV"
)

lf <- list.files(
  here::here(prlmin_ensdir, "PeakRate-LambdaMin_Holdout-01_Checks/"),
  pattern = "Array",
  full.names = TRUE
)

set.seed(1044)
pick <- sample(seq_along(lf), 1)
file.copy(
  from = file.path(lf[pick], list.files(lf[pick])),
  to = prlmin_ensdir
)
