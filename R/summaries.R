#' Functions to summarize, visualize, and analyze data


#' @param dir Directory where desired super learner results are stored.
#' @param slug Slug that identifies the prediction target. One of `sl_pkrate`, `sl_pkweek`, or `sl_cumhosp`.
#' @param altslug Additional label to identify sensitivity analyses.
#'
#'
#' @return The function explicitly returns the risk table described in the function's description. In the process of executing, the function also assigns a list containing data.tables containing mean learner risks for each week of the flu season.
#'
#' @describeIn summary_functions Formats risk tables that compare average risks of the ensemble super learner, discrete super learner, and mean prediction.
#'
#' @import magrittr
#' @import stringr
#' @import data.table
#'
#' @export fmt_risk_table

fmt_risk_table <- function(dir,
                           slug = c("sl_pkrate", "sl_pkweek", "sl_cumhosp"),
                           altslug = NULL) {

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

  # pull CV ensemble risk
  cvfiles <- list.files(here::here(dir, "EnsembleCV"), "Rds", full.names = TRUE)

  cvens_dat <- rbindlist(
    lapply(cvfiles, function(.x) {
      tmp <- readRDS(.x)

      data.table(
        hseason     = tmp$holdout_season$template,
        hseason_num = tmp$holdout_season$template_numeric,
        Week        = stringr::str_extract(.x, "(?<=week\\-)[0-9]{2}"),
        outcome     = tmp$holdout_outcome,
        enspred     = tmp$sl_pred,
        prederr     = tmp$sl_pred_abserr
      )
    })
  )

  fold_risks <- cvens_dat[, .(risk = mean(prederr)), .(hseason, Week)]

  cv_SE_risk <- cvens_dat[, .(SE_risk = sd(prederr) / sqrt(.N)), Week]

  fold_sum <- fold_risks[, .(
    learner = "SuperLearnerCV",
    coefficients = NA,
    mean_risk = mean(risk),
    fold_SD = sd(risk),
    fold_min_risk = min(risk),
    fold_max_risk = max(risk)
  ), .(Week)]

  cvrisk <- merge(fold_sum, cv_SE_risk, by = "Week")
  risktab_names <- names(risks[[1]])

  if (all(risktab_names %in% names(cvrisk))) {
    setcolorder(cvrisk, risktab_names)
  } else {
    stop("Mismatch between risk table and ensemble CV data formats.")
  }

  ## add cross-validated ensemble risks to each week's risk table
  risks <- lapply(risks, function(.x) {
    rbind(.x, cvrisk[Week == unique(.x$Week)])
  })

  # will use this exported object in subsequent tables
  if (!is.null(altslug)) {
    assign(paste0(slug, "_risktables_", altslug), risks, envir = .GlobalEnv)
  } else {
    assign(paste0(slug, "_risktables"), risks, envir = .GlobalEnv)
  }

  # format risks for output
  slslug <- "SuperLearner$|SuperLearnerCV"

  rt <- lapply(risks, function(x) {

    disc_sl_name <- x[
      !(learner %like% slslug)
    ][mean_risk == min(mean_risk), learner]

    # get risk information;
    esl <- x[learner %like% slslug]

    dsl <- x[
      !(learner %like% slslug)
    ][mean_risk == min(mean_risk)
    ][1][, learner := "DiscreteSL"]

    lrnr_sum <- rbind(esl, dsl)[,
      risksum :=
        paste0(format(round(mean_risk, 2), digits = 4),
               " (", format(round(SE_risk, 3), digits = 4), ")")
      ][, .(learner, risksum)]

    # if more than one learner had the same best mean_risk (SE),
    # they'll be concatenated and named in the BestComponent Model column
    rtlong <- dcast(lrnr_sum, . ~ learner, value.var = "risksum")
  })

  risksout <- lapply(1:length(files), function(x) {
    currwk <- str_extract(files[x], "[0-9]{2}(?=\\.Rds)")
    rt[[x]][, Week := currwk
            ][, .(Week, SuperLearnerCV, DiscreteSL)]
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
    curr[[x]][!(learner %like% "SuperLearner$|SuperLearnerCV")][]
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
      ][!(learner %like% "SuperLearner$|SuperLearnerCV")]
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
  dt <- copy(learner_stats)
  wk_cnt <-  dt[, length(unique(Week))]

  dt <- dt[, selected := ifelse(!is.na(weight) & weight > 0, 1, 0)
    ][, .(cnt_selected = sum(selected)), lid
    ][, P := cnt_selected / wk_cnt
    ][, .(lid, cnt_selected, P)
    ][order(-P)]

  dt[lid %like% "NNet", lclass := "Neural network"]
  dt[lid %like% "LOESS", lclass := "LOESS"]
  dt[lid == "ScreenGLM", lclass := "GLM"]
  dt[lid %like% "SVM", lclass := "Support vector regression"]
  dt[lid %like% "RF", lclass := "Random forest"]
  dt[lid %like% "PMARS", lclass := "PolyMARS"]
  dt[
    lid %in% c("LASSO", "Ridge", paste0("EN", 1:3)),
    lclass := "Penalized regression"
  ]
  dt[lid == "Mean", lclass := "Mean of prediction target"]

}


#' @param data A _rwsum type data set.
#' @describeIn summary_functions Helper function for cleaning random forest learner names
#' @export relabel_rf

relabel_rf <- function(data) {
  slugs <- data[
    grepl("randomForest", lname),
    str_extract_all(lname, "[0-9]{2}$")
  ]

  prefixes <- data[
    grepl("randomForest", lname),
    str_extract(lname, ".*(?=[0-9]{2}$)")
  ]

  snum <- unique(as.numeric(slugs))

  newslugs <- paste0("mtry", match(slugs, snum))

  newlabs <- paste0(prefixes, newslugs)
  newlabs
}


#' @param data A _rwsum type data set.
#' @describeIn summary_functions Helper function to format results for plotting.
#'
#' @export apply_rf_relabel

apply_rf_relabel <- function(data) {

  if (!exists("lchar", envir = .GlobalEnv)) {
    stop("Create the learner ID lookup table. (Run code at top of script file.)")
  }

  data[grepl("randomForest", learner), .N, Week
    ][, check := N == length(lchar[grepl("randomForest", lname), lname])]

  data[, lname := learner
   ][grepl("randomForest", learner),
     lname := lchar$lname[grepl("RF", lchar$lid)], Week
   ]

  data[, lid := lchar$lid[match(lname, lchar$lname)]]
}


#' @param data A _rwsum type data set.
#' @param titlestring Add a title to the plot.
#' @param font Control font in plot output. Defaults to "serif".
#' @describeIn summary_functions Produces a tile plot summarizing component risks and metalearner weights.
#'
#' @export plot_risktiles

plot_risktiles <- function(data, titlestring = "", font = "serif") {

  data %>%
    ggplot(aes(
      x = lid,
      y = Week
    )) +
    geom_tile(
      aes(fill = log(mean_risk)),
      color = "white"
    ) +
    geom_point(
      aes(size = weight, alpha = weight > 0),
      shape = 22,
      color = "black",
      fill = "white"
    ) +
    scale_fill_viridis(
      name = "Log(mean_risk)",
      option = "magma",
      direction = -1
    ) +
    scale_alpha_manual(values = c(0, 0.7)) +
    scale_size_continuous(
      name = "Weight",
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("> 0.00", "0.25", "0.50", "0.75", "1")) +
    expand_limits(y = c(0, 1)) +
    labs(
      x = "Learner ID",
      y = "Week",
      title = titlestring
    ) +
    theme_minimal(base_family = font) +
    theme(
      plot.background = element_blank(),
      axis.text = element_text(angle = 90, hjust = 1, vjust = -1)
    )
}


#' @param data A _rwsum type data set.
#' @param titlestring Add a title to the plot.
#' @param font Control font in plot output. Defaults to "serif".
#' @describeIn summary_functions Produces a plot of the ensemble super learner's mean risk across all 30 weeks of the flu season against the mean prediction reference, as well as the distribution of cross-validated risks and ensemble weights assigned to each component learner.
#'
#' @importFrom magrittr `%>%`
#' @import data.table
#'
#' @export plot_ensemble_performance

plot_ensemble_performance <- function(data, risktables, titlestring = "", font = "serif") {

  ## Calculate 95% confidence intervals for mean risk.
  cntens <- lapply(risktables, function(x) {
    x[learner == "SuperLearnerCV", .(
      learner, ens_mean_risk = mean_risk, SE = SE_risk, Week
    )][, ":="(
      ll95 = ens_mean_risk - qnorm(0.975) * SE,
      ul95 = ens_mean_risk + qnorm(0.975) * SE
    )]
  }) %>% rbindlist

  ## Plot.
  data[weight > 0] %>%
    ggplot(aes(x = Week)) +
    geom_point(
      aes(size = weight, y = log(mean_risk)),
      alpha = 0.4,
      shape = 21
    ) +
    geom_pointrange(
      data = cntens,
      aes(
        y = log(ens_mean_risk),
        ymin = log(ll95),
        ymax = log(ul95),
        color = "Super Learner"
    ), shape = "square") +
    geom_line(
      data = cntens,
      aes(y = log(ens_mean_risk), group = 1, color = "Super Learner"),
      size = 1,
      alpha = 0.3
    ) +
    ggtitle(titlestring) +
    guides(name = "Mean cross-validated risk") +
    theme_base(base_family = font)
}
