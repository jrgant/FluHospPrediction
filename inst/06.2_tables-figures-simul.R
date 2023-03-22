source(here::here("inst", "06.1_tables-figures-setup.R"))

################################################################################
## TABLE: PARAMETER TARGETS ##
################################################################################

emp <- fread(file.path(clndir, "empdat.csv"))

pkht <- emp[, .(pkht = max(weekrate)), by = season]
pkwk <- emp[, .(pkwk = mean(weekint[weekrate == max(weekrate)])), by = season]
cumhosp <- emp[, .(cumrate = cumrates[weekint == max(weekint)]), by = season]

targets <- pkht[pkwk[cumhosp, on = "season"], on = "season"] %>%
  .[, .(Season = season,
        `Peak rate` = pkht,
        `Peak week` = pkwk,
        `Cumulative hospitalizations` = cumrate)]

targets

fwrite(targets, nicefile(tabslug, "Prediction-Targets", "csv"))


################################################################################
## TABLES: CURVES BY SEASON TEMPLATE ##
################################################################################

## Check how many observations affected by curve transformation
pretrans_sims <- lapply(1:length(sim_lm$hc), function(x) {
  cid <- rep(x, 30)
  original_sim <- unname(unlist(sim_lm$hc[[x]]$eq$fi_pluserr))
  data.table(cid = cid, weekint = 1:30, original_sim)
}) %>% rbindlist

setnames(pretrans_sims, "V1", "origsim")
pretrans_sims

sims <- sim_lm$outhc

tmpct <- sims[weekint == 1, .N, keyby = .(`Template season` = template)
              ][, N := format(N, big.mark = ",")]


fwrite(tmpct, nicefile(tabslug, "Simulation-Template-Counts", "csv"))


## Reported in methods: proportion of weeks negative and set to 0.

transcomp <- sims[pretrans_sims, on = c("cid", "weekint")]

proptrans <-
  transcomp[, .N, .(origsim < 0)][, P := N / sum(N)][origsim == TRUE, P]

saveRDS(proptrans, nicefile(valslug, "Proportion-Weeks-Transformed", "Rds"))


################################################################################
## FIGURE: CURVES BY SEASON TEMPLATE AND LAMBDA VALUE ##
################################################################################

# compare curves by lambda
simlist <- list(
  # lambda.rand = sim_lr$outhc,
  lambda.min = sim_lm$outhc,
  lambda.1se = sim_ls$outhc
)

simdt <- rbindlist(simlist, idcol = "lambda")

simdt

# get summaries of arg_f week sliding
sapply(unique(simdt$lambda), function(x) simdt[lambda == x, summary(week)])

simdt[, .(pkrate = max(prediction)), .(lambda, cid)] %>%
  ggplot(aes(x = lambda, y = pkrate)) +
  geom_boxplot() +
  theme_base()

sim_crvs <- sims[, .(
  cid,
  weekint,
  weekrate = prediction,
  template
)][, crvtype := "Simulated"]


emp_crvs <- emp[season != "2009-10", .(
  cid = paste(season),
  weekint,
  weekrate,
  template = season)
  ][, crvtype := "Empirical"]

crv <- rbind(sim_crvs, emp_crvs)

make_tempsim_panel <- function(season = NULL,
                               sim_geom = c("line", "boxplot"),
                               base_font_size = 14,
                               yint = 2,
                               data = crv) {

  tmp <- data[cid == season | template == season]

  # spec desired geom()
  curr_sim_geom <- switch(
    sim_geom,
    line = call(
      "geom_line",
      data = tmp[crvtype == "Simulated"],
      alpha = 0.03,
      size = 0.5,
      color = "gray"
    ),
    boxplot = call(
      "geom_boxplot",
      data = tmp[crvtype == "Simulated"],
      mapping = aes(group = weekint),
      color = "gray80",
      outlier.size = 0.5
    )
  )

  plot <- tmp %>%
    ggplot(aes(
      x = weekint,
      y = weekrate,
      group = cid
    )) +
    eval(curr_sim_geom) +
    geom_line(
      data = tmp[crvtype == "Empirical"],
      alpha = 1,
      size = 0.7,
      aes(color = "Empirical")
    ) +
    scale_color_manual(name = "", values = "black") +
    scale_y_continuous(
      limits = c(0, ceiling(max(data$weekrate)) + 1),
      breaks = seq(0, ceiling(max(data$weekrate)) + 1, by = yint)
    ) +
    labs(
      y = "Rate",
      x = "Week"
    ) +
    theme_tufte(
      base_family = global_plot_font,
      base_size = base_font_size
    ) +
    theme(legend.position = "none") +
    theme(
      plot.background = element_rect(color = "white"),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = base_font_size)
    )
  plot
}

season_labs <- season_levels()[season_levels() != "2009-10"]
season_labs_alpha <- paste0(LETTERS[seq_along(season_labs)], ")")

tempsim_list <- lapply(
  setNames(season_labs, season_labs),
  make_tempsim_panel,
  sim_geom = "line",
  yint = 6
)

tempsim_plot <- cowplot::plot_grid(
  plotlist = tempsim_list,
  ncol = 3,
  labels = season_labs_alpha,
  label_fontface = "plain",
  label_size = tempsim_list[[1]]$theme$text$size,
  hjust = 0.5,
  vjust = 0
  ) +
  theme(
    plot.margin = unit(c(0.25, 0, 0, 0.25), "in"),
    plot.background = element_blank()
  )

tempsim_plot

plotsave(
  plot = tempsim_plot,
  width = 7,
  height = 11,
  name = "Simulation-Curves-by-Template"
)


tempsim_bp_list <- lapply(
  setNames(season_labs, season_labs),
  make_tempsim_panel,
  sim_geom = "boxplot",
  yint = 6
)

tempsim_boxplot <- cowplot::plot_grid(
  plotlist = tempsim_bp_list,
  ncol = 3,
  labels = season_labs_alpha,
  label_fontface = "plain",
  label_size = tempsim_bp_list[[1]]$theme$text$size,
  hjust = 0.5,
  vjust = 0
  ) +
  theme(
    plot.margin = unit(c(0.25, 0.1, 0, 0.25), "in"),
    plot.background = element_blank()
  )

tempsim_boxplot

plotsave(
  plot = tempsim_boxplot,
  width = 7,
  height = 11,
  name = "Simulation-Curves-by-Template-Boxplot"
)

## Save individual panels.
lapply(seq_along(tempsim_bp_list), function(x) {
  plotsave(
    plot = tempsim_bp_list[[x]],
    width = 5,
    height = 5,
    png = FALSE,
    name = paste0(
      "Simulation-Curves-by-Template-Boxplot-Panel-",
      substring(season_labs_alpha[[x]], 1, 1)
    )
  )
})



################################################################################
## FIGURE: SELECT RANDOM CURVES BY SHAPE TEMPLATE ##
################################################################################

set.seed(197211)

select_curves <-
  crv[, .(selcid = sample(unique(cid), 10)), template][, as.numeric(selcid)]

crvsub <- crv[crvtype == "Empirical" | cid %in% select_curves]

tempsim_randcrv <- crvsub %>%
  ggplot(aes(x = weekint, y = weekrate)) +
  geom_line(
    data = crvsub[crvtype == "Simulated"],
    aes(color = crvtype, group = cid)
  ) +
  geom_line(
    data = crvsub[crvtype == "Empirical"],
    aes(color = crvtype, group = cid)
  ) +
  facet_wrap(~ template) +
  scale_color_manual(
    name = "Curve type",
    values = c("black", "gray90")
  ) +
  labs(x = "Week", y = "Hospitalizations (per 100,000 population)") +
  theme_base(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    plot.background = element_blank()
  )

plotsave(
  plot = tempsim_randcrv,
  width = 8,
  height = 10,
  name = "Simulation-Curves-by-Template-Rand10"
)


################################################################################
## COMPARE SIMULATED CURVES TO THEIR EMPIRICAL TEMPLATES ##
################################################################################

simvemp <- list(
  empirical = emp[, .(template = season, weekrate, weekint, cid = season)],
  sim.lmin = sim_lm$outhc[, .(template, weekrate = prediction, weekint, cid)],
  sim.l1se = sim_ls$outhc[, .(template, weekrate = prediction, weekint, cid)]
)

simvempl <- rbindlist(simvemp, idcol = "type")

simsum <- copy(simvempl[type != "empirical"])

simsum <- simsum[, .(
  peakrate = max(weekrate),
  peakweek = weekint[weekrate == max(weekrate)]
), .(cid, type, template)]

# summarize peak rate by lambda sim and season shape template
simsum[, .(
  mean = mean(peakrate),
  median = median(peakrate),
  sd = sd(peakrate),
  min = min(peakrate),
  max = max(peakrate)
), keyby = .(type, template)]

# summarize peak week by lambda sim and season shape template
simsum[, .(
  mean = mean(peakweek),
  median = median(peakweek),
  sd = sd(peakweek),
  min = min(peakweek),
  max = max(peakweek)
), keyby = .(type, template)]

simsum[, type := as.factor(type)][, type := relevel(type, ref = "sim.lmin")]

simtemp_bp_theme <-
  theme_base() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))

bp_peakrate <- ggplot(simsum, aes(x = template, y = peakrate)) +
  geom_boxplot(fill = "aliceblue") +
  facet_wrap(~ type) +
  labs(
    x = "Empirical template season",
    y = "Peak hospitalization rate (per 100,000 population)"
  ) +
  simtemp_bp_theme

bp_peakweek <- ggplot(simsum, aes(x = template, y = peakweek)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.5) +
  facet_wrap(~ type) +
  labs(x = "Empirical template season",
       y = "Week of peak hospitalization rate"
  ) +
  simtemp_bp_theme

chsum <- simvempl[
  type != "empirical",
  .(cumhosp = sum(weekrate)), .(cid, type, template)
]

bp_cumhosp <- ggplot(chsum, aes(x = template, y = cumhosp)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.5) +
  facet_wrap(~ factor(type, levels = c("sim.lmin", "sim.l1se"))) +
  labs(
    x = "Empirical template season",
    y = "Cumulative hospitalization rate (per 100,000 population)"
  ) +
  simtemp_bp_theme

plotsave(
  name = "Sim-Dists_Peak-Rate",
  plot = bp_peakrate,
  width = 10,
  height = 8
)

plotsave(
  name = "Sim-Dists_Peak-Week",
  plot = bp_peakweek,
  width = 10,
  height = 8
)

plotsave(
  name = "Sim-Dists_Cum-Hosp",
  plot = bp_cumhosp,
  width = 10,
  height = 8
)


################################################################################
## FIGURE: EMPIRICAL VS. SIMULATED PREDICTION TARGETS ##
################################################################################

pr_dist <- simvempl[, .(value = max(weekrate)), .(type, cid)]

fwrite(
  pr_dist,
  here::here("results", "00_paper_output", "TAB_Simulated-PeakRate.csv")
)

pw_dist <- simvempl[, .(
  value = weekint[weekrate == max(weekrate)]
), .(type, cid)]

fwrite(
  pw_dist,
  here::here("results", "00_paper_output", "TAB_Simulated-PeakWeek.csv")
)

ch_dist <- simvempl[, .(value = sum(weekrate)), .(type, cid)]

fwrite(
  ch_dist,
  here::here("results", "00_paper_output", "TAB_Simulated-CumHosp.csv")
)

## Plot distributions.
distl <- list(peakrate = pr_dist, peakweek = pw_dist, cumhosp = ch_dist)

dl <- rbindlist(distl, idcol = "target")

facetlabs <- c(
  peakrate = "Peak rate\n(per 100,000 pop.)",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations\n(per 100,000 pop.)"
)

simtype <- c("empirical", "sim.lmin", "sim.l1se")


## Before plotting, we add a small jitter to the peak week values so we can
## get a sense of the density of simulated peak weeks.
set.seed(1066)
scdl <- dl[, target := factor(target, levels = names(facetlabs))
   ][, value_jitter := ifelse(
     target == "peakweek" & type %in% c("sim.lmin", "sim.l1se"),
     value + rnorm(.N, 0, 0.1),
     value
     )]

scdl[, type := factor(type, levels = c("sim.l1se", "sim.lmin", "empirical"))]

typef_levels <- c("Simulated(lambda[min])",
                  "Simulated(lambda[SE])",
                  "Empirical")

## typef_labels <- c(expression(atop("Simulated", "(lambda[min])")),
##                   expression(atop("Simulated", "(lambda[SE])")),
##                   expression("Empirical"))

typef_labels <- c("Simulated<br>&lambda;<sub>min</sub>",
                  "Simulated<br>&lambda;<sub>SE</sub>",
                  "Empirical")

scdl[, typef := fcase(
         type == "sim.lmin", typef_levels[1],
         type == "sim.l1se", typef_levels[2],
         type == "empirical", typef_levels[3]
       )][, typef := factor(typef, levels = typef_levels)]

## Plot with additional horizontal jitter to view point density across all
## targets and simulation/empirical categories.
## Jitter applied only to peak week for readability.

make_simcompare_panel <- function(targ, base_font_size = 14, data = scdl) {
  data[target == targ] %>%
  ggplot(
    aes(x = value_jitter, y = typef)
  ) +
  geom_point(
    aes(alpha = typef, size = typef, color = typef),
    position = position_nudge(
      y = -0.11 + rnorm(nrow(data[target == targ]), 0, 0.03)
    )
  ) +
  geom_boxplot(
    width = 0.2,
    fill = rgb(0, 0, 0, 0),
    outlier.alpha = 0,
    position = position_nudge(y = -0.11)
  ) +
  geom_density_ridges(
    aes(fill = typef),
    alpha = 0.5,
    scale = 0.5
  ) +
  scale_alpha_manual(values = c(0.2, 0.2, 1)) +
  scale_size_manual(values = c(0.1, 0.1, 1)) +
  scale_color_viridis_d(option = "magma", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(option = "magma", begin = 0.8, end = 0.1) +
  scale_y_discrete(labels = typef_labels) +
  labs(
    y = "Data",
    x = c(
      "Peak Rate",
      "Peak Week",
      "Cumulative Hospitalizations"
    )[which(c("peakrate", "peakweek", "cumhosp") == targ)]
  ) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = FALSE,
    fill = FALSE
  ) +
  theme_tufte(
    base_family = global_plot_font,
    base_size = base_font_size
  ) +
  theme(
    axis.line = element_line(),
    panel.border = element_blank(),
    axis.title.x = element_text(margin = ggplot2::margin(t = 0.2, unit = "in")),
    axis.title.y = element_text(margin = ggplot2::margin(r = 0.2, unit = "in")),
    axis.text = element_text(size = base_font_size),
    axis.text.y = ggtext::element_markdown(vjust = 0.5, hjust = 0.5),
    plot.background = element_blank()
  )
}

set.seed(1900)
simcompare_list <- list(
  peakrate = make_simcompare_panel("peakrate"),
  peakweek = make_simcompare_panel("peakweek"),
  cumhosp = make_simcompare_panel("cumhosp")
)

simcompare <- cowplot::plot_grid(
  plotlist = simcompare_list,
  nrow = 1,
  labels = paste0(LETTERS[1:3], ")"),
  label_fontface = "plain",
  label_size = simcompare_list[[1]]$theme$text$size,
  hjust = 0
) + theme(plot.margin = unit(c(0, 0.05, 0, 0), "in"))

simcompare

plotsave(
  name = "TargetDists-Emp-vs-Sim",
  plot = simcompare,
  width = 11,
  height = 4.8
)

lapply(seq_along(simcompare_list), function(x) {
  plotsave(
    plot = simcompare_list[[x]],
    width = 11 / 3,
    height = 4.8,
    png = FALSE,
    name = paste0("TargetDists-Emp-vs-Sim-Panel-", LETTERS[[x]]))
})
