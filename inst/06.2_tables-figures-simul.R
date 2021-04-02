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

tempsim_plot <- crv %>%
  ggplot(aes(x = weekint, y = weekrate, group = cid)) +
  geom_line(
    data = crv[crvtype == "Simulated"],
    alpha = 0.03,
    size = 0.5,
    color = "gray"
  ) +
  geom_line(
    data = crv[crvtype == "Empirical"],
    alpha = 1,
    size = 1,
    aes(color = "Empirical")
  ) +
  labs(
    y = "Hospitalization rate (per 100,000 population)",
    x = "Week"
  ) +
  facet_wrap(~ template, ncol = 5) +
  scale_color_manual(name = "", values = "black") +
  theme_base(base_family = global_plot_font) +
  theme(legend.position = "bottom")

tempsim_plot

plotsave(
  plot = tempsim_plot,
  width = 6,
  height = 6,
  name = "Simulation-Curves-by-Template"
)

tempsim_boxplot <- crv[crvtype == "Simulated"] %>%
  ggplot(aes(x = factor(weekint), y = weekrate)) +
  geom_boxplot(color = "gray80", outlier.size = 0.5) +
  geom_line(
    data = crv[crvtype == "Empirical"],
    aes(x = weekint, y = weekrate, color = "Empirical"),
    size = 1
  ) +
  facet_wrap(~ template, ncol = 5) +
  labs(
    y = "Hospitalization rate (per 100,000 population)",
    x = "Week"
  ) +
  scale_color_manual(name = "Curve type", values = "#990000") +
  scale_x_discrete(
    breaks = c(1, 5, 10, 15, 20, 25, 30),
    labels = c("1", "5", "10", "15", "20", "25", "30")
  ) +
  theme_base(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

tempsim_boxplot

plotsave(
  plot = tempsim_boxplot,
  width = 8,
  height = 7,
  name = "Simulation-Curves-by-Template-Boxplot"
)


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
  theme_base(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

plotsave(
  plot = tempsim_randcrv,
  width = 8,
  height = 7,
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

scdl[, typef := fcase(
         type == "empirical", "Empirical",
         type == "sim.lmin", "Simulated(lambda[min])",
         type == "sim.l1se", "Simulated(lambda[SE])"
       )]

scdl[, typef := factor(
         typef,
         levels = c(
           "Simulated(lambda[SE])",
           "Simulated(lambda[min])",
           "Empirical"
         )
       )]

## Make labellers for plot facets and axes.
target_labeller <- c(
  peakrate = "Peak rate\n(per 100,000 population)",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations\n(per 100,000 population)"
)

## Plot with additional vertical jitter to view point density across all
## targets and simulation/empirical categories.
simcompare <- scdl %>%
  ggplot(
    aes(x = value_jitter, y = typef)
  ) +
  geom_boxplot(
    width = 0.2,
    outlier.alpha = 0,
    position = position_nudge(y = -0.11)
  ) +
  geom_point(
    aes(alpha = typef, size = typef, color = typef),
    position = position_nudge(y = -0.11 + rnorm(nrow(scdl), 0, 0.03))
  ) +
  geom_density_ridges(
    aes(fill = typef),
    alpha = 0.5,
    scale = 0.5
  ) +
  scale_alpha_manual(values = c(0.01, 0.01, 1)) +
  scale_size_manual(values = c(0.1, 0.1, 1)) +
  scale_color_viridis_d(option = "magma", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(option = "magma", begin = 0.8, end = 0.1) +
  facet_wrap(
    ~ target,
    labeller = labeller(target = target_labeller),
    scales = "free_x"
  ) +
  scale_y_discrete(labels = scales::parse_format()) +
  labs(y = "Data", x = "Value") +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = FALSE,
    fill = FALSE
  ) +
  theme_base(
    base_family = global_plot_font
  ) +
  theme(strip.text = element_text(face = "bold"))

simcompare

plotsave(
  name = "TargetDists-Emp-vs-Sim",
  plot = simcompare,
  width = 10,
  height = 4.8
)
