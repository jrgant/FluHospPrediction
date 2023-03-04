################################################################################
## SETUP ##
################################################################################

source(here::here("inst", "06.1_tables-figures-setup.R"))

library(pacman)
p_load(
  data.table,
  ggplot2,
  ggthemes,
  magrittr,
  FluHospPrediction,
  e1071, # loading required to predict using SVM component learners
  foreach,
  parallel,
  doParallel,
  scico
)

pobs <- list.files(here::here("results"), "ProspObs", full.names = TRUE)


################################################################################
## ENSEMBLE SUPER LEARNER FILES ##
################################################################################

## This function pulls a data file.
pullpreds <- function(fp) {
  d <- as.data.table(readRDS(fp))
  d[, week := stringr::str_extract(fp, "(?<=w)[0-9]{2}")]
  return(d)
}

fitl <- lapply(pobs, function(.x) {

  # pull super learner fits from simulated training data
  simfiles <- list.files(
    file.path(.x, "ProspSim"),
    pattern = "pred",
    full.names = TRUE
  )

  psim <- rbindlist(lapply(simfiles, pullpreds))

  ## pull super learner fits from observed training data
  obsfiles <- list.files(
    file.path(.x, "Observed"),
    pattern = "pred",
    full.names = TRUE
  )

  pobs <- rbindlist(lapply(obsfiles, pullpreds))

  fc <- c(
    "pkrate"  = "PeakRate",
    "pkweek"  = "PeakWeek",
    "cumhosp" = "CumHosp"
  )

  psim[, outcome := names(
           match.arg(stringr::str_extract(.x, "[A-Za-z]+(?=-)"), fc)
         )]

  pobs[, outcome := names(
           match.arg(stringr::str_extract(.x, "[A-Za-z]+(?=-)"), fc)
         )]

  out <- list(sim = psim, obs = pobs)
  out
})

pdat <- rbind(
  rbindlist(fitl[[1]], idcol = "traindat"),
  rbindlist(fitl[[2]], idcol = "traindat"),
  rbindlist(fitl[[3]], idcol = "traindat")
)

pdatl <- melt(
  pdat,
  id.vars = c("traindat", "obs_season", "outcome", "week"),
  variable.name = "measure"
)

pdatl[, outcome := factor(outcome, levels = c("pkrate", "pkweek", "cumhosp"))]
pdatl

targlabs <- c(
  pkrate = "Peak rate",
  pkweek = "Peak week",
  cumhosp = "Cumulative\nhospitalizations"
)


################################################################################
## DISCRETE SUPER LEARNERS (EMPIRICAL TRAINING DATA) ##
################################################################################

obsdslpaths <- list.files(
  file.path(pobs, "Observed"),
  pattern = "slfit",
  full.names = T
)

process_odsl <- function(fp) {

  ## find correct place to store DSL fit
  target <- str_extract(fp, "(?<=results\\/).*(?=\\-P)")
  ## sublist <- obsdslpaths[obsdslpaths %like% target]
  ## sublist_indei <- which(sublist == i)

  sl1 <- readRDS(fp)
  cvrisk <- sl1$cv_risk(loss_absolute_error)[learner != "SuperLearner"]
  dslname <- cvrisk[mean_risk == min(mean_risk), learner]

  if (length(dslname) > 1) {

    dslname <- cvrisk[learner %in% dslname
                      ][fold_max_risk == min(fold_max_risk), learner]

    if (length(dslname) > 1) dslname <- sample(dslname, size = 1)
  }

  out <- list(
    target = target,
    season = str_extract(fp, "(?<=\\/s)[0-9]{4}\\-[0-9]{2}"),
    week = str_extract(fp, "(?<=_w)[0-9]{2}"),
    dsl = sl1$learner_fits[[dslname]]
  )

  out
}

getdsl <- function(string, paths = obsdslpaths) {
  active_paths <- paths[paths %like% string]

  ncores <- detectCores() - 4
  registerDoParallel(ncores)

  dsllist <- foreach(
    iter = seq_along(active_paths),
    .packages = c(
      "sl3", "stringr", "data.table", "FluHospPrediction", "e1071"
    )
  )  %dopar% {
    process_odsl(active_paths[iter])
  }

  dsllist
}

## loop and retrieve DSLs trained on observed data

obsdsl <- list()

obsdsl[["PeakRate"]] <- getdsl("PeakRate")
length(obsdsl[[1]])
sapply(obsdsl[[1]], length)
gc()

obsdsl[["PeakWeek"]] <- getdsl("PeakWeek")
length(obsdsl[[2]])
sapply(obsdsl[[2]], length)
gc()

obsdsl[["CumHosp"]] <- getdsl("CumHosp")
length(obsdsl[[3]])
sapply(obsdsl[[3]], length)
gc()


################################################################################
## DISCRETE SUPER LEARNERS (SIMULATED TRAINING DATA) ##
################################################################################

simdslpaths <- file.path(pobs, "ProspSim", "dslfits.Rds")
simdslnames <- str_extract(simdslpaths, "(?<=results\\/).*(?=\\-)")

simdsl <- lapply(setNames(simdslpaths, simdslnames), readRDS)


################################################################################
## FORMAT EMPIRICAL DATA ##
################################################################################

## NOTE
## In this section, we format empirical data to predict the outcome(s) using
## the DSL for later comparison with the ESL and naive predictions.

## function to format empirical season for prediction
source(here::here("inst", "fmt_emp_season.R"))

settings <- as.data.table(
  expand.grid(
    target = c("pkrate", "pkweek", "cumhosp"),
    week = seq(5, 25, 5),
    season = c("2016-17", "2017-18", "2018-19"),
    stringsAsFactors = F
  )
)

sapply(settings, class)

tasks <- lapply(
  seq_len(settings[, .N]),
  function(.x) {
    tmp <- settings[.x]

    tt <- suppressWarnings(
      fhp_make_task(
        target = tmp$target,
        current_week = tmp$week,
        lambda_type = "lambda-min",
        prosp = tmp$season
      )
    )

    out <- list(
      task   = tt,
      target = tmp$target,
      week   = tmp$week,
      season = tmp$season
    )

    out
  }
)

fmtd_emp <- lapply(tasks, function(.x) {
  ## target_select needed by format_empirical_season
  ## let it read the object from the global environment
  target_select <<- .x$target
  fe <- format_empirical_season(
    week = .x$week,
    predict_on = .x$season,
    origtask = .x$task
  )

  list(
    target = target_select,
    week = .x$week,
    season = .x$season,
    task = fe$task
  )
})

target_select <- NULL


################################################################################
## GET DSL PREDICTIONS ##
################################################################################

getindex <- function(index, outcome) {

  tobs <- obsdsl[[outcome]][[index]]
  tsim <- simdsl[[outcome]][[index]]

  data.table(
    otarget = tobs$target,
    oweek   = as.numeric(tobs$week),
    oseason = tobs$season,
    oindex  = index,
    starget = tsim$target,
    sweek   = as.numeric(tsim$week),
    sseason = tsim$season,
    sindex  = index
  )
}

fitinds <- rbindlist(
  list(
    pkrate = rbindlist(lapply(1:15, getindex, outcome = "PeakRate")),
    pkweek = rbindlist(lapply(1:15, getindex, outcome = "PeakWeek")),
    cumhosp = rbindlist(lapply(1:15, getindex, outcome = "CumHosp"))
  ),
  idcol = "target"
)

## check to make sure all DSL objects are aligned by week, season, and target
fitinds[, .N, .(weekeq = oweek == sweek,
                seaseq = oseason == sseason,
                targeq = otarget == starget)]

## looks good, create by variables for merge with empirical data indices
fitinds[, ":=" (week = oweek, season = oseason, fitindex = oindex)]

## empirical data indices
fmtinds <- rbindlist(
  lapply(
    seq_along(fmtd_emp),
    function(.x) {
      data.table(
        target = fmtd_emp[[.x]]$target,
        week   = fmtd_emp[[.x]]$week,
        season = fmtd_emp[[.x]]$season,
        fmtindex  = .x
      )
    }
  )
)

sapply(fitinds, class)
sapply(fmtinds, class)

idtable <- merge(
  fitinds,
  fmtinds,
  by = c("target", "week", "season")
)[, .(target, week, season, fitindex, fmtindex)]

idtable[target == "pkrate", dsltarg := "PeakRate"]
idtable[target == "pkweek", dsltarg := "PeakWeek"]
idtable[target == "cumhosp", dsltarg := "CumHosp"]

dslpred <- lapply(
  seq_len(nrow(idtable)),
  function(.x) {
    tmp    <- idtable[.x]
    fitat  <- tmp[, fitindex]
    empat  <- tmp[, fmtindex]
    dslsub <- tmp[, dsltarg]
    ptask  <- fmtd_emp[[empat]]$task

    preds <- data.table(
      outcome     = tmp[, target],
      week        = sprintf("%02d", tmp[, week]),
      obs_season  = tmp[, season],
      obs_outcome = ptask$Y,
      ## DSL preds, observed training data
      odsl_name   = obsdsl[[dslsub]][[fitat]]$dsl$name,
      odsl_pred   = obsdsl[[dslsub]][[fitat]]$dsl$predict(ptask),
      ## DSL preds, simulated training data
      sdsl_name   = simdsl[[dslsub]][[fitat]]$dsl$fit$name,
      sdsl_pred   = simdsl[[dslsub]][[fitat]]$dsl$fit$predict(ptask)
    )

    preds
  }
) %>% rbindlist


################################################################################
## CHECK DSL/ESL MATCHES ##
################################################################################

## making sure the DSL, ESL, and median predictions are matched properly
## by week, season, and prediction target

dsl_esl_match <- merge(
  pdatl[measure == "obs_outcome"],
  dslpred,
  by = c("outcome", "week", "obs_season")
)

dsl_esl_match[, .N, .(outcomeval_eq = value == obs_outcome)]


################################################################################
## INCORPORATE DSL DATA INTO PLOTTING DATASET ##
################################################################################

dslpred[, .N, odsl_name]
dslpred[, .N, sdsl_name]

ddatl <- melt(
  dslpred[, -c("odsl_name", "sdsl_name")],
  id.vars = c("outcome", "week", "obs_season"),
  variable.name = "measure"
)[measure != "obs_outcome"]

ddatl[, traindat := ifelse(measure %like% "odsl", "obs", "sim")]

setcolorder(
  ddatl,
  c("traindat", "obs_season", "outcome", "week", "measure", "value")
)

ddatl


################################################################################
## PLOT ENSEMBLE AND DISCRETE SL PREDICTIONS ##
################################################################################

plotdat <- rbind(pdatl, ddatl)

plotdat[measure %in% c("sl_pred", "odsl_pred", "sdsl_pred"),
        sltype := ifelse(measure %like% "dsl", "Discrete", "Ensemble")]

## plotting datasets
sll <- plotdat[sltype %in% c("Discrete", "Ensemble")]

sll[measure == "sl_pred", measure := "pred"]
sll[measure %like% "dsl", measure := "pred"]

sll[traindat == "sim", traindat := "Simulated"]
sll[traindat == "obs", traindat := "Observed"]

obsout <- plotdat[
  measure == "obs_outcome" & traindat == "sim",
  -c("traindat", "sltype", "measure")
]

setnames(obsout, "value", "obs_outcome")

sll_segments <- merge(sll, obsout, by = c("obs_season", "outcome", "week"))

bg <- data.table(
  week = c("10", "20"),
  x1 = c(1.5, 3.5),
  x2 = c(2.5, 4.5)
)

make_pl_pobs_panel <- function(targ = c("pkrate", "pkweek", "cumhosp"),
                               season = c("2016-17", "2017-18", "2018-19"),
                               base_font_size = 14,
                               data = sll,
                               legend = FALSE) {

  plot <- data[outcome == targ & obs_season == season] %>%
    ggplot(aes(x = as.numeric(factor(week)))) +
    geom_linerange(
      data = sll_segments[outcome == targ & obs_season == season],
      aes(
        x = as.numeric(factor(week)),
        ymin = obs_outcome, ymax = value,
        group = interaction(sltype, traindat)
      ),
      position = position_dodge(width = 0.75),
      linetype = "dashed",
      color = "gray50"
    ) +
    geom_hline(
      data = obsout[outcome == targ & obs_season == season],
      aes(yintercept = obs_outcome, linetype = "Target")
    ) +
    geom_point(
      data = data.table(
        x = c(0.5, 1.5, 2.5, 3.5, 4.5),
        y = obsout[outcome == targ & obs_season == season, obs_outcome]
      ),
      aes(x = x, y = y),
      shape = 15,
      size = 2,
      color = "white",
      fill = "white"
    ) +
    geom_point(
      aes(
        fill = traindat, shape = sltype, y = value,
        group = interaction(sltype, traindat)
      ),
      position = position_dodge(width = 0.75),
      size = 4
    ) +
    labs(x = "Week") +
    scale_x_continuous(
      breaks = 1:5,
      labels = c("05", "10", "15", "20", "25")
    ) +
    scale_linetype_manual(name = "", values = "solid") +
    scale_shape_manual(
      name = expression(underline(SuperLearner)),
      values = c(22, 21)
    ) +
    scale_fill_scico_d(
      name = expression(underline(Training~Data)),
      palette = "berlin"
    )

  if (legend == T) {
    plot <- plot +
      guides(
        fill = guide_legend(override.aes = list(shape = 21, size = 7)),
        shape = guide_legend(override.aes = list(size = 7)),
        linetype = FALSE
      )
  } else {
    plot <- plot +
          guides(fill = FALSE, shape = FALSE, linetype = FALSE)
  }

  plot +
    theme_tufte(
      base_family = global_plot_font,
      base_size = base_font_size
    ) +
    theme(
      legend.box = "vertical",
      legend.box.background = element_rect(),
      legend.box.spacing = ggplot2::margin(0.1, unit = "in"),
      legend.position = c(0.5, 0.65),
      legend.title = element_text(size = base_font_size, hjust = 0.5),
      legend.text = element_text(size = base_font_size),
      axis.text = element_text(size = base_font_size),
      axis.line = element_line(),
      axis.title.x = element_text(
        margin = ggplot2::margin(t = 0.125, unit = "in")
      ),
      axis.title.y = element_text(
        margin = ggplot2::margin(r = 0.1, unit = "in")
      )
    )
}

plot_targets <- c("pkrate", "pkweek", "cumhosp")
plot_seasons <- c("2016-17", "2017-18", "2018-19")

pgrid <- as.data.table(expand.grid(ptarg = plot_targets, pseas = plot_seasons))
setkey(pgrid, ptarg)
pgrid

pgrid[, ":="(
  add_legend = fcase(
    ptarg == "cumhosp" & pseas == "2018-19", TRUE,
    default = FALSE
  ),
  yll = fcase(
    ptarg == "pkrate", 0,
    ptarg == "pkweek", -25,
    ptarg == "cumhosp", 0
  ),
  yul = fcase(
    ptarg == "pkrate", 50,
    ptarg == "pkweek", 25,
    ptarg == "cumhosp", 350
  )
)]

pgrid

pl_pobs_list <- lapply(seq_len(nrow(pgrid)), function(x) {
  tmp <- pgrid[x]
  tlim <- tmp[, unlist(.(yll, yul))]
  tp <- make_pl_pobs_panel(
    targ = tmp[, ptarg],
    season = tmp[, pseas],
    legend = tmp[, add_legend]
  )
  if (tmp[, ptarg] == "pkrate") {
    tp <- tp +
      scale_y_continuous(
        breaks = seq(tlim[1], tlim[2], 10),
        limits = tlim
      ) +
      labs(y = "Peak Rate")
  } else if (tmp[, ptarg] == "pkweek") {
    tp <- tp +
      scale_y_continuous(
        breaks = seq(tlim[1], tlim[2], 5),
        limits = tlim
      ) +
      labs(y = "Peak Week")
  } else if (tmp[, ptarg] == "cumhosp") {
    tp <- tp +
      scale_y_continuous(
        breaks = seq(tlim[1], tlim[2], 50),
        limits = tlim
      ) +
      labs(y = "Cumulative Hospitalizations")
  }
  tp
})

relheights_cp <- c(peakrate = 1, peakweek = 1.4, cumhosp = 1.8)
pl_pobs <- cowplot::plot_grid(
  plotlist = pl_pobs_list,
  labels = paste0(LETTERS[seq_len(length(pl_pobs_list))], ")"),
  label_size = pl_pobs_list[[1]]$theme$text$size,
  label_fontface = "plain",
  hjust = 0.5,
  vjust = 0,
  rel_heights = relheights_cp
) + theme(plot.margin = unit(c(0.2, 0, 0, 0.2), "in"))

pl_pobs

pl_pobs_width <- 12
pl_pobs_height <- 12

plotsave(
  plot = pl_pobs,
  width = pl_pobs_width,
  height = pl_pobs_height,
  name = "Prospective-Observed-Application"
)

lapply(seq_along(pl_pobs_list), function(x) {
  cumht <- sum(relheights_cp)
  plotsave(
    plot = pl_pobs_list[[x]],
    name = paste0("Prospective-Observed-Application-Panel-", LETTERS[x]),
    width = pl_pobs_width / 3,
    height = switch(
      pl_pobs_list[[x]]$labels$y,
      "Peak Rate" = pl_pobs_height * (relheights_cp["peakrate"] / cumht),
      "Peak Week" = pl_pobs_height * (relheights_cp["peakweek"] / cumht),
      "Cumulative Hospitalizations" =
        pl_pobs_height * (relheights_cp["cumhosp"] / cumht)
    )
  )
})


################################################################################
## COMPARE ENSEMBLE ERRORS ##
################################################################################

abserr <- merge(
  sll[sltype == "Ensemble"],
  obsout,
  by = c("obs_season", "week", "outcome")
)

abserr[, abserr := abs(value - obs_outcome)]

abserrw <- dcast(
  outcome + obs_season + week ~ traindat,
  value.var = "abserr",
  data = abserr
)

## data to place labels in each facet of the plot below
seclabs <- paste0(
  "Favors SL trained on\n",
  c("observed", "simulated"),
  " data"
)

## plotlabs <- data.table(
##   outcome = c(rep("pkrate", 2), rep("pkweek", 2), rep("cumhosp", 2)),
##   Observed = c(12, 20,
##                8.25, 14.75,
##                85, 150), # x-axis
##   Simulated = c(18.75, 12,
##                 13, 9,
##                 130, 95), # y-axis
##   label = rep(seclabs, 3)
## )

plotlabs <- data.table(
  outcome = c(rep("pkrate", 2), rep("pkweek", 2), rep("cumhosp", 2)),
  Observed = c(
    mean(c(0, 0, 32)),   # pkrate, label 1, x
    mean(c(0, 32, 32)),  # pkrate, label 2, x
    mean(c(0, 0, 21)),   # pkweek, label 1, x
    mean(c(0, 21, 21)),  # pkweek, label 2, x
    mean(c(0, 0, 205)),  # cumhosp, label 1, x
    mean(c(0, 205, 205)) # cumhosp, label 2, x
  ),
  Simulated = c(
    mean(c(0, 32, 32)),   # pkrate, label 1, y
    mean(c(0, 0, 32)),    # pkrate, label 2, y
    mean(c(0, 21, 21)),   # pkweek, label 1, y
    mean(c(0, 0, 21)),    # pkweek, label 2, y
    mean(c(0, 205, 205)), # cumhosp, label 1, y
    mean(c(0, 0, 205))    # cumhosp, label 2, y
  ),
  label = rep(seclabs, 3)
)

outorder <- c("pkrate", "pkweek", "cumhosp")
plotlabs[, outcome := factor(outcome, outorder)]
abserrw[, outcome := factor(outcome, outorder)]

p_abserr <- abserrw %>%
  ggplot(aes(x = Observed, y = Simulated)) +
  stat_function(fun = function(.x) 1 * .x + 0, color = "gray40") +
  geom_text(
    data = plotlabs,
    aes(x = Observed, y = Simulated, label = label),
    size = 7,
    color = "gray40",
    family = global_plot_font,
    lineheight = 1
  ) +
  geom_point(aes(fill = week, shape = obs_season), size = 7) +
  facet_wrap(
    vars(outcome),
    scales = "free",
    labeller = labeller(outcome = targlabs)
  ) +
  labs(
    x = "\nObserved Training Data\n(Absolute Prediction Error)",
    y = "Simulated Training Data\n(Absolute Prediction Error)\n"
  ) +
  scale_fill_scico_d(
    name = "Week",
    palette = "hawaii",
    direction = -1
  ) +
  scale_shape_manual(
    name = "Season",
    values = c(25, 22, 21)
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = 21, size = 9)),
    shape = guide_legend(override.aes = list(size = 9))
  ) +
  theme_minimal(base_size = 25, base_family = global_plot_font) +
  theme(
    strip.text = element_text(size = 25, face = "bold"),
    legend.key.height = unit(1.5, "cm"),
    panel.grid.major = element_line(color = "whitesmoke"),
    panel.grid.minor = element_line(color = "whitesmoke")
  )

p_abserr

plotsave(
  plot = p_abserr,
  width = 27,
  height = 8.4,
  name = "Prospective-Observed-Error"
)
