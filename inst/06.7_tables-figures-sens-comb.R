source(here::here("inst", "06.1_tables-figures-setup.R"))

# load risktables into environment
fmt_risk_table(dir = respr, slug = "sl_pkrate")
fmt_risk_table(dir = respw, slug = "sl_pkweek")
fmt_risk_table(dir = resch, slug = "sl_cumhosp")

fmt_risk_table(dir = respr_1se, slug = "sl_pkrate", altslug = "1se")
fmt_risk_table(dir = respw_1se, slug = "sl_pkweek", altslug = "1se")
fmt_risk_table(dir = resch_1se, slug = "sl_cumhosp", altslug = "1se")

fmt_risk_table(dir = respr_erf, slug = "sl_pkrate", altslug = "erf")
fmt_risk_table(dir = respw_erf, slug = "sl_pkweek", altslug = "erf")
fmt_risk_table(dir = resch_erf, slug = "sl_cumhosp", altslug = "erf")


################################################################################
## SENSITIVITY ANALYSIS: COMBINED FIGURE ##
################################################################################

written_tabs_date <- "2022-01-24"

## Need to run prior 06.X files first to create the CSV files referenced here.
risktabs <- list.files(
  here::here("results", "00_paper_output"),
  pattern = paste0("Risks.*", written_tabs_date),
  full.names = TRUE
)

lu_file <- data.table(
  fn_slug = c(
    "PeakRate_20",  "PeakWeek_20",  "CumHosp_20",
    "PeakRate-L1SE",  "PeakWeek-L1SE",  "CumHosp-L1SE",
    "PeakRate-ERF",   "PeakWeek-ERF",   "CumHosp-ERF"
  ),
  objname = c(
    "pkrate_rwsum",     "pkweek_rwsum",     "cumhosp_rwsum",
    "pkrate_rwsum_1se", "pkweek_rwsum_1se", "cumhosp_rwsum_1se",
    "pkrate_rwsum_erf", "pkweek_rwsum_erf", "cumhosp_rwsum_erf"
  )
)

lapply(seq_len(nrow(lu_file)), function(.x) {
  s <- lu_file[.x, objname]
  f <- lu_file[.x, fn_slug]
  currfile <- risktabs[grep(f, risktabs)]
  if (length(currfile) == 0) {
    invisible(NULL)
  } else {
    assign(s, fread(currfile), envir = .GlobalEnv)
  }
})

## Main analysis objects did not have a suffix.
analysis <- c("main" = "", "1se" = "_1se", "erf" = "_erf")

sens_compare <- lapply(setNames(analysis, analysis), function(x) {

  rwsum <- rbindlist(
    idcol = "target",
    list(
      pkrate = get(paste0("pkrate_rwsum", x)),
      pkweek = get(paste0("pkweek_rwsum", x)),
      cumhosp = get(paste0("cumhosp_rwsum", x))
    )
  )

  risktab <- rbindlist(
    idcol = "target",
    list(
      pkrate = rbindlist(get(paste0("sl_pkrate_risktables", x))),
      pkweek = rbindlist(get(paste0("sl_pkweek_risktables", x))),
      cumhosp = rbindlist(get(paste0("sl_cumhosp_risktables", x)))
    )
  )

  if (x == "") {
    rwsum[, analysis := "main"]
    risktab[, analysis := "main"]
  } else {
    slug <- names(analysis)[grep(x, analysis)]
    rwsum[, analysis := slug]
    risktab[, analysis := slug]
  }

  list(
    rwsum = rwsum,
    risktab = risktab[learner == "SuperLearnerCV"]
  )
})

## Bind up component learner weight summaries.
sens_rwsum <- rbindlist(
  list(
    sens_compare[[1]]$rwsum,
    sens_compare[[2]]$rwsum,
    sens_compare[[3]]$rwsum
  )
)

## Bind up risk tables.
sens_risktab <- rbindlist(
  list(
    sens_compare[[1]]$risktab,
    sens_compare[[2]]$risktab,
    sens_compare[[3]]$risktab
  )
)

sens_risktab[, ":=" (
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

## Match up naive (median predictor) risk estimates.
sens_naive <- data.table(
  analysis = c(rep("main", 3), rep("lse", 3), rep("erf", 3)),
  target = rep(c("pkrate", "pkweek", "cumhosp"), 3),
  naiverisk = c(
    pr_medrisk$log_mean_risk,
    pw_medrisk$log_mean_risk,
    ch_medrisk$log_mean_risk,
    pr_medrisk_1se$log_mean_risk,
    pw_medrisk_1se$log_mean_risk,
    ch_medrisk_1se$log_mean_risk,
    pr_medrisk$log_mean_risk,
    pw_medrisk$log_mean_risk,
    ch_medrisk$log_mean_risk
  )
)

## Make nice facet labels.
anlys_labs <- c(
  main = "Main analysis",
  lse = "Alternate trend filter penalty",
  erf = "Component learner subset"
)

targ_labs <- c(
  pkrate = "Peak rate",
  pkweek = "Peak week",
  cumhosp = "Cumulative hospitalizations"
)

## Control display order for prediction targets and analysis types.
forder2 <- c("pkrate", "pkweek", "cumhosp")
sens_rwsum[, target := factor(target, levels = forder2, ordered = TRUE)]
sens_risktab[, target := factor(target, levels = forder2, ordered = TRUE)]
sens_naive[, target := factor(target, levels = forder2, ordered = TRUE)]

forder3 <- c("main", "lse", "erf")
sens_rwsum[, analysis := factor(analysis, levels = forder3, ordered = TRUE)]
sens_risktab[, analysis := factor(analysis, levels = forder3, ordered = TRUE)]
sens_naive[, analysis := factor(analysis, levels = forder3, ordered = TRUE)]

## Select x-axis tick mark labels to hide
hide_xticks <- c(paste0("0", c(2:4, 6:9)), 11:14, 16:19, 21:24, 26:29)
invislab <- rep("", length(hide_xticks))
names(invislab) <- hide_xticks

## NOTE Log scale figure is not in the paper.
## Plot analysis comparisons.
pep_sens_compare <-
  ggplot(sens_risktab) +
  geom_point(
    aes(
      x = Week, y = log(mean_risk),
      size = coefficients, color = "Component learner"
    ),
    shape = 21
  ) +
  geom_pointrange(
    data = sens_risktab,
    aes(
      x = as.numeric(Week),
      y = log(mean_risk),
      ymin = log(ll95),
      ymax = log(ul95),
      color = "Ensemble"
    ),
    shape = 21,
    size = 0.3,
    fill = "white"
  ) +
  geom_hline(
    data = sens_naive,
    aes(
      yintercept = naiverisk,
      color = "Median predictor"
    ),
    linetype = "dashed"
  ) +
  facet_grid(
    target ~ analysis,
    scales = "free_y",
    switch = "y",
    labeller = labeller(
      target = targ_labs,
      analysis = anlys_labs
    )
  ) +
  scale_color_manual(
    name = "Prediction",
    values = c("#dddddd", "#990000", "black")
  ) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  scale_x_discrete(breaks = week_breaks) +
  xlab("Week") +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(size = 8)
  )

pep_sens_compare

plotsave(
  name = "Ensemble-Summary_All-Targets_Main-1SE-ERF",
  plot = pep_sens_compare,
  width = 10,
  height = 8
)


################################################################################
## DIFFERENCE PLOTS ACROSS SENSITIVITY ANALYSIS ##
################################################################################

sens_normscale <- sens_risktab %>%
  ggplot(
    aes(x = Week, y = mean_risk, group = analysis, color = analysis)
  ) +
  geom_hline(
    data = sens_naive[analysis != "erf"],
    aes(
      yintercept = exp(naiverisk),
      linetype = analysis
    )
  ) +
  geom_line(aes(group = analysis), alpha = 0.5) +
  geom_pointrange(
    aes(ymin = ll95, ymax = ul95),
    fill = "white",
    shape = 21
  ) +
  facet_wrap(
    ~ target,
    scales = "free",
    labeller = labeller(
      target = targ_labs
    )
  ) +
  ylab("Mean risk") +
  xlab("Week") +
  scale_linetype_manual(
    name = "Median prediction",
    values = c("dashed", "dotted"),
    labels = c("Main analysis", "Alternate trend filter")
  ) +
  scale_color_viridis_d(
    name = "Ensemble risks",
    option = "magma",
    end = 0.1,
    begin = 0.7,
    labels = c("Main analysis", "Alternative trend filter", "Component subset")
  ) +
  guides(
    linetype = guide_legend(order = 1),
    color = guide_legend(
      order = 2
    )
  ) +
  scale_x_discrete(breaks = week_breaks) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 8),
    legend.margin = ggplot2::margin(0, 0, 0, 0, "cm")
  )

sens_normscale

plotsave(
  name = "Ensemble-Summary_All-Targets_Regular-Scale",
  plot = sens_normscale,
  width = 11,
  height = 4
)


################################################################################
## COMPONENT LEARNER INSPECTION ##
################################################################################

## This function extracts the validation set predictions and weights from
## the SL output objects.
bind_components <- function(sl_files) {
    sl <- readRDS(sl_files)

    w <- as.data.table(
      sl$sl_pruned$metalearner_fit$coefficients,
      keep.rownames = TRUE
    )[, .(learner = V1, weight = V2)]

    dt <- sl$sl_pruned$cv_meta_task$X
    dtm <- suppressWarnings(
      melt(dt, value.name = "pred", variable.name = "learner")
    )

    wk <- stringr::str_extract(sl_files, "[0-9]{2}")
    dtm[, Week := wk]

    out <- rbind(
      merge(dtm, w, by = "learner"),
      data.table(
        learner = "SuperLearnerCV",
        pred = sl$full_preds,
        Week = wk,
        weight = NA
      )
    )

    out[, truth := rep(sl$task$Y, .N / length(sl$task$Y))]
    out
}

pkrate_cp <- rbindlist(lapply(
  list.files(respr, pattern = "Rds", full.names = TRUE),
  bind_components
))

pkweek_cp <- rbindlist(lapply(
  list.files(respw, pattern = "Rds", full.names = TRUE),
  bind_components
))

cumhosp_cp <- rbindlist(lapply(
  list.files(resch, pattern = "Rds", full.names = TRUE),
  bind_components
))

theme_set(theme_tufte(ticks = F, base_size = 25))

ggplot(
  pkrate_cp[learner == "SuperLearnerCV"],
  aes(x = pred, y = truth)
  ) +
  geom_point(aes(color = weight), alpha = 0.3) +
  facet_wrap(~ Week) +
  scale_color_viridis()

pkrate_cp[
  weight > 0,
  .(weight = round(data.table::first(weight), 4)),
  .(learner, Week)
]

pkweek_cp[
  weight > 0,
  .(weight = round(data.table::first(weight), 4)),
  .(learner, Week)
]

cumhosp_cp[
  weight > 0,
  .(weight = round(data.table::first(weight), 4)),
  .(learner, Week)
]
