#' Functions to summarize, visualize, and analyze data
#'
#' @param dir Directory where desired super learner results are stored.
#' @param slug Slug that identifies the prediction target. One of `sl_pkrate`, `sl_pkweek`, or `sl_cumhosp`.
#'
#'
#' @return The function explicitly returns the risk table described in the function's description. In the process of executing, the function also assigns a list containing data.tables containing mean learner risks for each week of the flu season.
#'
#' @describeIn summary_functions Formats risk tables that compare average risks of the ensemble super learner, discrete super learner, and mean prediction.
#'
#' @rawNamespace import(data.table, except = first)
#' @import magrittr
#' @import stringr
#' @importFrom dplyr first
#'
#' @export fmt_risk_table

fmt_risk_table <- function(dir,
                           slug = c("sl_pkrate", "sl_pkweek", "sl_cumhosp")) {

  if (length(slug) != 1) {
    stop("You've either got too many slugs or not enough. Pick one.")
  }

  if (!slug %in% c("sl_pkrate", "sl_pkweek", "sl_cumhosp")) {
    stop("'Fraid we ain't got none of those slugs. Go on choose another.")
  }

  files <- list.files(dir, slug, full.names = TRUE)

  # pull CV risk tables from output
  risks <- lapply(files, function(x) {
    d <- readRDS(x)
    r <- d$cv_risk_abserr
    r$Week <- stringr::str_extract(x, "[0-9]{2}(?=\\.Rds)")
    return(r)
  })

  # will use this exported object in subsequent tables
  assign(paste0(slug, "_risktables"), risks, envir = .GlobalEnv)

  # format risks for output
  keep_always <- c("Lrnr_mean", "SuperLearner")

  rt <- lapply(risks, function(x) {
    disc_sl_name <- x[!learner %in% keep_always & mean_risk == min(mean_risk), learner]
    print(length(disc_sl_name))
    curr <- x[
      learner %in% keep_always |
          (!learner %in% keep_always & mean_risk == min(mean_risk))
        ][, learner := ifelse(!learner %in% keep_always, "BestComponent", learner)
          ][, risksum :=
                paste0(format(round(mean_risk, 2), digits = 3),
                       " (", format(round(SE_risk, 3), digits = 3), ")")
            ][, .(learner, risksum)
              ][learner != "Lrnr_glm_TRUE"]

    # using dplyr first here accounts for cases where two learners had
    # exactly the same mean_risk (SE)
    rtlong <- dcast(
      curr,
      . ~ learner,
      value.var = "risksum",
      fun.aggregate = dplyr::first
    )

    # if more than one learner had the same best mean_risk (SE),
    # they'll be concatenated and named in the BestComponent Model column
    rtlong[, BestComponentModel :=
               ifelse(
                 length(disc_sl_name) > 1,
                 paste(disc_sl_name, collapse = ", "),
                 disc_sl_name
               )
           ][, .(SuperLearner, BestComponent, Mean = Lrnr_mean, BestComponentModel)]
  })

  risksout <- lapply(1:length(files), function(x) {
    currwk <- str_extract(files[x], "[0-9]{2}(?=\\.Rds)")
    rt[[x]][, Week := currwk
            ][, .(Week, SuperLearner, BestComponent, Mean, BestComponentModel)]
  }) %>% rbindlist

  return(risksout)
}



#' @param outcome Prediction target outcome. One of `pkrate`, `pkweek`, or `cumhosp`
#'
#' @describeIn summary_functions Outputs a summary of the distribution of cross-validated risks across all learners, by week of the flu season.
#'
#' @import data.table
#' 
#' @export get_risk_dist

get_risk_dist <- function(outcome = c("pkrate", "pkweek", "cumhosp")) {

  curr <- get(paste0(outcome, "_risktables"))

  rtbl <- lapply(1:length(curr), function (x) {
    curr[[x]][learner != "SuperLearner"][]
  }) %>% rbindlist

  distbyweek <-
    rtbl[, .(
      Mean = mean(mean_risk),
      SD = sd(mean_risk),
      Median = median(mean_risk),
      Minimum = min(mean_risk),
      Maximum = max(mean_risk)),
      Week][, Week := as.character(Week)][]

  return(distbyweek)
}


#' @param dir Directory where desired super learner results are stored.
#' @param slug Slug that identifies the prediction target. One of `sl_pkrate`, `sl_pkweek`, or `sl_cumhosp`.
#'
#' @return A list of length 30, corresponding to the number of weeks of the flu sesaon, containing data.tables storing the mean risks for all learners.
#' 
#' @describeIn summary_functions Pulls the weights assigned to each learner by the metalearner.xm
#'
#' @import data.table
#'
#' @export get_learner_weights

get_learner_weights <- function(dir,
                                slug = c("sl_pkrate", "sl_pkweek", "sl_cumhosp"),
                                metalearner_is = c("nnls", "solnp")) {

  file <- list.files(dir, slug, full.names = TRUE)

  if (metalearner_is == "nnls") {
    out <- lapply(file, function(x) {
      curr <- readRDS(x)
      df <- data.table(
        learner = curr$sl_pruned$metalearner_fit$lrnrs,
        weight = curr$sl_pruned$metalearner_fit$x
      )
      df
    })
  } else {
    out <- lapply(file, function(x) {
      curr <- readRDS(x)
      df <- data.table(
        learner = names(curr$sl_pruned$metalearner_fit$coefficients),
        weight = curr$sl_pruned$metalearner_fit$coefficients
      )
      df
    })
  }

  out
}



#' @param risktables A list object containing risk tables, generated by \code{fmt_risk_table}.
#' 
#'
#' @param weights A list object containing weight tables, generated by \code{get_learner_weights}
#'
#' @return Returns a data.table containing all learners and , their mean risks, and the weights assigned to them.
#'
#' @describeIn summary_functions Generates a data.table that links each learner's mean cross-validated risk and the weight assigned to that learner based on the metalearner fit, for each week of the flu season.
#'
#' @import data.table
#'
#' @export join_learner_stats

join_learner_stats <- function(risktables, weights) {

  if (length(risktables) != length(weights)) {
    stop("Risktables and weights need to be the same length.")
  }

  out <- lapply(1:length(risktables), function(x) {
    risktables[[x]][
    weights[[x]], on = "learner"
    ][, .(Week, learner, mean_risk, SE_risk, weight)
      ][learner != "SuperLearner"]
  }) %>% rbindlist

  return(out)
}


#' @param learner_stats The data.table produced by `join_learner_stats`.
#'
#' @return Returns a data.table containing the number and percentage of weeks during which each learner is assigned a non-zero weight by the metalearner.
#'
#' @describeIn summary_functions Calculates the number of weeks during which a learner was assigned non-zero weight.
#'
#' @import data.table
#' @export summarize_learner_selection 

summarize_learner_selection <- function(learner_stats) {

  learner_stats[, .N, .(learner, weight > 0)
    ][, P := N / sum(N), .(learner)
    ][weight == TRUE, .(learner, N, pct = round(P * 100, 1))
    ][order(pct, decreasing = TRUE)]

}
