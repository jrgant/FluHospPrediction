## In Week 16 of the Peak Rate (Lambda Min outcome), the cross-valided ensemble
## risk suddenly jumped. After investigating the issue, it appears that the
## algorithm was not able to fit the HOLDOUT_01 validation set (correponding to
## the 2003-04 flu season), resulting in extreme prediction risks for those
## simulated outcomes.

## We re-ran the super learner on that validation set 20 additional times to
## investigate whether this issue was an anomaly or whether this was in fact the
## characteristic behavior of the ensemble against this validation set.

library(data.table)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(cowplot)
library(FluHospPrediction)

source("inst/06.1_tables-figures-setup.R")


################################################################################
## READ DATA ##
################################################################################

h01w16_dir <- here::here(
  "results",
  "PeakRate-LambdaMin",
  "EnsembleCV",
  "PeakRate-LambdaMin_Holdout-01_Checks"
)

## original extreme risk
orig_file <- c(ORIG = list.files(h01w16_dir, "Rds", full.names = TRUE))

## replications runs
new_files <- sapply(
  list.files(h01w16_dir, "Array", full.names = TRUE),
  list.files,
  full.names = TRUE
)

nfids <- seq_along(new_files)

reslist <- lapply(c(orig_file, new_files), readRDS)
names(reslist)[-1] <- nfids

preds <- lapply(reslist, function(.x) {
  data.table(abserr = .x$sl_pred_abserr)[, rowid := 1:.N][]
}) %>% rbindlist(idcol = "runid")

setcolorder(preds, c("runid", "rowid", "abserr"))

preds[, runid := factor(runid, levels = unique(runid))]
preds

preds_means <- preds[, .(mean_abserr = mean(abserr)), runid]
preds_means


################################################################################
## PLOTS ##
################################################################################

ploterrs <- function(pat = "ORIG|[0-9]+", data = preds, datamn = preds_means) {
  data[runid %like% pat] %>%
    ggplot(aes(x = runid, y = abserr)) +
    geom_point(size = 0.1, color = "gray", alpha = 0.6, position = "jitter") +
    geom_boxplot(outlier.alpha = 0, fill = NA) +
    geom_point(data = datamn[runid %like% pat],
               aes(y = mean_abserr),
               shape = 21,
               size = 2.5,
               color = "black",
               fill = "steelblue") +
    labs(x = "Run ID", y = "Absolute Prediction Error") +
    theme_few(base_size = 16)
}

h01w16_risks <- plot_grid(ploterrs(), ploterrs("[0-9]+"))

plotsave(
  name = "Boxplot_H01-W16-Risks_PeakRate_LambdaMin",
  plot = h01w16_risks,
  width = 15,
  height = 7.5
)
