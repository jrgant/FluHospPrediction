################################################################################
## LOAD ##
################################################################################

pacman::p_load(
  FluHospPrediction,
  data.table,
  extrafont
)

## Data directories.
clndir <- here::here("data", "cleaned")
paper_output <- here::here("results", "00_paper_output")

## Slugs for table, figure, and value filenames.
figslug <- "FIG"
tabslug <- "TAB"
valslug <- "VAL"

## This function formats filenames during save operations, using both the
## slugs above and the current date. If one file is updated, make sure to
## update all files, as the manuscript Rmd selects files to use based on
## a global date string.
nicefile <- function(slug, description, ext, date = Sys.Date(),
                     dir = paper_output) {
  file.path(dir, paste0(slug, "_", description, "_", date, ".", ext))
}

## Load font database.
loadfonts(device = "win")
global_plot_font <- "CMU Serif"

## Universal breaks for week labeling in plots.
week_breaks <- c("01", "05", "10", "15", "20", "25", "30")

################################################################################
## TABLE ONE: PARAMETER TARGETS ##
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
## RESULTS DIRECTORIES ##
################################################################################

resdir <- "results"

## Main analysis files
respr <- file.path(resdir, "PeakRate-LambdaMin")
respw <- file.path(resdir, "PeakWeek-LambdaMin")
resch <- file.path(resdir, "CumHosp-LambdaMin")

## Sensitivity analysis files (alternative trend filter lambda)
respr_1se <- file.path(resdir, "PeakRate-LambdaSE")
respw_1se <- file.path(resdir, "PeakWeek-LambdaSE")
resch_1se <- file.path(resdir, "CumHosp-LambdaSE")

## Sensitivity analysis files (elastic net, random forest subset)
respr_erf <- file.path(resdir, "PeakRate-ElastNetRF")
respw_erf <- file.path(resdir, "PeakWeek-ElastNetRF")
resch_erf <- file.path(resdir, "CumHosp-ElastNetRF")

## Sensitivity analysis files (squared error loss)
respr_sqe <- file.path(resdir, "PeakRate-SqErrLoss")
respw_sqe <- file.path(resdir, "PeakWeek-SqErrLoss")
resch_sqe <- file.path(resdir, "CumHosp-SqErrLoss")

## Simulated curves
# sim_lr <- readRDS(file.path(clndir, "hypothetical-curves.Rds"))
sim_lm <- readRDS(file.path(clndir, "hypothetical-curves_lambda-min.Rds"))
sim_ls <- readRDS(file.path(clndir, "hypothetical-curves_lambda-1se.Rds"))


################################################################################
## LEARNER NAME LOOKUP TABLE ##
################################################################################

# Learner lookup table (get submitted learners from a task)
lid07 <- readRDS(file.path(respr, "sl_pkrate_07.Rds"))

lchar <- data.table(
  lname = names(lid07$sl_pruned$metalearner_fit$coefficients)
)

# Relabel the random forests
relabel_rf(lchar)

# Assign IDs to the component learners and create lookup table
lchar[
  grepl("glmnet.*(0\\.25|0\\.5|0\\.75)", lname),
  lid := paste0("EN", 1:max(.I))
]

lchar[
  grepl("glmnet.*1_100", lname),
  lid := "LASSO"
]

lchar[
  grepl("loess", lname),
  lid := paste0("LOESS", 1:max(.I))
]

lchar[
  grepl("nnet", lname),
  lid := paste0("NNet", str_pad(1:max(.I), 2, "left", "0"))
]

lchar[
  grepl("polspline", lname),
  lid := paste0("PMARS", 1:max(.I))
]

lchar[
  grepl("randomForest", lname),
  lname := relabel_rf(lchar)
]

lchar[
  grepl("randomForest", lname),
  lid := paste0("RF", str_pad(1:max(.I), 2, "left", "0"))
]

lchar[
  grepl("glmnet.*0_100", lname),
  lid := "Ridge"
]

lchar[
  grepl("Pipeline", lname),
  lid := "ScreenGLM"
]

lchar[
  grepl("svm", lname),
  lid := paste0("SVM", 1:max(.I))
][]

lchar


################################################################################
## RISK TABLES ##
################################################################################

## TABLE: Peak Rate Risks ------------------------------------------------------

pkrate_risks <- fmt_risk_table(dir = respr, slug = "sl_pkrate")
pkrate_risks

fwrite(
  pkrate_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate", "csv")
)


## TABLE: Peak Week Risks ----------------------------------------------------

pkweek_risks <- fmt_risk_table(dir = respw, slug = "sl_pkweek")
pkweek_risks

fwrite(
  pkweek_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakWeek", "csv")
)


## TABLE: Cumulative Hospitalizations Risks ----------------------------------

cumhosp_risks <- fmt_risk_table(dir = resch, slug = "sl_cumhosp")
cumhosp_risks

fwrite(
  cumhosp_risks,
  nicefile(tabslug, "Risks-EDSL-Mean_CumHosp", "csv")
)


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

ggsave(
  plot = tempsim_plot,
  width = 6,
  height = 6,
  units = "in",
  file = nicefile(figslug, "Simulation-Curves-by-Template", "png"),
  dev = "png"
)

tempsim_boxplot <- crv[crvtype == "Simulated"] %>%
  ggplot(aes(x = factor(weekint), y = weekrate)) +
  geom_line(
    aes(group = cid),
    alpha = 0.03,
    size = 0.5,
    color = "gray"
  ) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.5) +
  geom_line(
    data = crv[crvtype == "Empirical"],
    aes(x = weekint, y = weekrate, color = "Empirical"),
    size = 1
  ) +
  facet_wrap(~ template, ncol = 5) +
  xlab("Week") +
  scale_color_manual(name = "Curve type", values = "#990000") +
  scale_x_discrete(
    labels = c(
      "1", rep("", 3),
      "5", rep("", 4),
      "10", rep("", 4),
      "15", rep("", 4),
      "20", rep("", 4),
      "25", rep("", 4), "30"
    )
  ) +
  theme_base(base_family = global_plot_font) +
  theme(axis.text.x = element_text(size = 8))

tempsim_boxplot

ggsave(
  plot = tempsim_boxplot,
  width = 8,
  height = 7,
  units = "in",
  file = nicefile(figslug, "Simulation-Curves-by-Template-Boxplot", "png"),
  dev = "png"
)


################################################################################
## COMPARE SIMULATED CURVE TO THEIR EMPIRICAL TEMPLATES ##
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

ggsave(
  nicefile(figslug, "Sim-Dists_Peak-Rate", "png"),
  bp_peakrate,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)

ggsave(
  nicefile(figslug, "Sim-Dists_Peak-Week", "png"),
  bp_peakweek,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)

ggsave(
  nicefile(figslug, "Sim-Dists_Cum-Hosp", "png"),
  bp_cumhosp,
  width = 10,
  height = 8,
  units = "in",
  device = "png"
)


################################################################################
## FIGURE: EMPIRICAL VS. SIMULATED PREDICTION TARGETS ##
################################################################################

pr_dist <- simvempl[, .(value = max(weekrate)), .(type, cid)]

pw_dist <- simvempl[, .(
  value = weekint[weekrate == max(weekrate)]
), .(type, cid)]

ch_dist <- simvempl[, .(value = sum(weekrate)), .(type, cid)]

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

ggsave(
  nicefile(figslug, "TargetDists-Emp-vs-Sim", "png"),
  simcompare,
  width = 10,
  height = 4.8,
  unit = "in"
)


################################################################################
## TABLES: AVERAGE RISK BY WEEK, ACROSS COMPONENT MODELS ##
################################################################################

## Peak rate
riskdist_pkrate <- get_risk_dist("sl_pkrate")
riskdist_pkrate

fwrite(riskdist_pkrate, nicefile(tabslug, "Risk-Week_Peak-Rate", "csv"))

## Peak week
riskdist_pkweek <- get_risk_dist("sl_pkweek")
riskdist_pkweek

fwrite(riskdist_pkweek, nicefile(tabslug, "Risk-Week_Peak-Week", "csv"))


## Cum. hosp.
riskdist_cumhosp <- get_risk_dist("sl_cumhosp")
riskdist_cumhosp

fwrite(riskdist_cumhosp, nicefile(tabslug, "Risk-Week_Cum-Hosp", "csv"))


################################################################################
## SETUP: INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

pkrate_weights <- get_learner_weights(
  dir = respr,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

pkweek_weights <- get_learner_weights(
  dir = respw,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

cumhosp_weights <- get_learner_weights(
  dir = resch,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

# check that we have all 30 weeks for each prediction target
sapply(
  list(pkrate_weights, pkweek_weights, cumhosp_weights),
  function(x) length(x) == 30
)


################################################################################
## FIGURES: PEAK KRATE, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

### Data summary
pkrate_rwsum <- join_learner_stats(
  risktables = sl_pkrate_risktables,
  weights = pkrate_weights
)

pkrate_rwsum

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum)

### Risk tile plot
prt_pr <- plot_risktiles(pkrate_rwsum, titlestring = "Peak rate")

prt_pr_out <- prt_pr +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.0", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate", "png"),
  prt_pr_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pr <- plot_ensemble_performance(
  pkrate_rwsum,
  sl_pkrate_risktables,
  titlestring = "Peak rate"
)

mn_pkrate_lmin <- simsum[type == "sim.lmin", mean(peakrate)]

pkr_mnrisks <- simsum[
  type == "sim.lmin",
  .(risks = mean(abs(mn_pkrate_lmin - peakrate))),
  template
]

pkr_mnrisks[, mean(risks)]

## Median (naive) prediction risk.
pr_medrisk <- pr_dist[type == "sim.lmin", log(median(abs(mean(value) - value)))]

pep_pr_out <- pep_pr +
  geom_hline(aes(yintercept = pr_medrisk, linetype = "Median benchmark risk"), color = "red") +
  scale_linetype_manual(values = c("dashed"), name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.4,
    end = 0.4,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  guides(shape = FALSE) + 
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate", "png"),
  pep_pr_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkrate_lrnr_sel <- summarize_learner_selection(pkrate_rwsum)[order(-P)]
pkrate_lrnr_sel

fwrite(
  pkrate_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Peak-Rate", "csv")
)

pkrate_lrnr_sel[, lid := as.factor(lid)
                ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, shape = lclass)) +
#  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(size = 2, color = "maroon") +
  scale_shape_manual(values = c(0:5, 7, 12, 9, 13)) +
  # scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (peak rate)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "right"
  )


################################################################################
## FIGURES: PEAK WEEK, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

### Data summary
pkweek_rwsum <- join_learner_stats(
  sl_pkweek_risktables,
  pkweek_weights
)

pkweek_rwsum

### Relabel randomForest learners
apply_rf_relabel(pkweek_rwsum)

### Risk tile plot
prt_pw <- plot_risktiles(pkweek_rwsum, titlestring = "Peak week")
prt_pw

prt_pw_out <- prt_pw +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week", "png"),
  prt_pw_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

# sparkline dat
slpw_spark <- lapply(sl_pkweek_risktables, function(x) {
  x[learner == "SuperLearner", .(learner, mean_risk)]
}) %>% rbindlist(, idcol = "Week") %>%
  .[, Week := stringr::str_pad(Week, 2, "left", "0")]

setnames(slpw_spark, "learner", "ensemble_sl")

pkweek_rwsum %>%
  ggplot(aes(x = Week, y = log(mean_risk), group = lid)) +
  geom_line(size = 1) +
  facet_wrap(~lid) +
  geom_line(
    data = slpw_spark,
    aes(x = Week, y = log(mean_risk), group = ensemble_sl),
    linetype = "dashed",
    color = "red"
  ) +
  theme_tufte()


### Super Learner performance
pep_pw <- plot_ensemble_performance(
  pkweek_rwsum,
  sl_pkweek_risktables,
  titlestring = "Peak week"
)

pep_pw

## Median (naive) risk.
pw_medrisk <- pw_dist[type == "sim.lmin", log(mean(abs(median(value) - value)))]

pep_pw_out <- pep_pw +
  geom_hline(
    aes(yintercept = pw_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pw_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week", "png"),
  pep_pw_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel <- summarize_learner_selection(pkweek_rwsum)
pkweek_lrnr_sel[, .N, lclass]

fwrite(
  pkweek_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Peak-Week", "csv")
)

pkweek_lrnr_sel[, lid := as.factor(lid)
  ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, fill = lclass)) +
  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (peak week)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "bottom"
  )


################################################################################
## FIGURES: CUMULATIVE HOSPITALIZATIONS, INSPECT WEIGHT ASSIGNMENTS ##
################################################################################

### Data summary
cumhosp_rwsum <- join_learner_stats(
  sl_cumhosp_risktables,
  cumhosp_weights
)

cumhosp_rwsum

### Relabel randomForest learners
apply_rf_relabel(cumhosp_rwsum)

### Risk tile plot
prt_ch <- plot_risktiles(
  cumhosp_rwsum,
  titlestring = "Cumulative hospitalizations"
)

prt_ch_out <- prt_ch +
  guides(alpha = FALSE) +
  scale_size_continuous(
   name = "Weight",
   labels = c("> 0.00", "0.2", "0.4", "0.6")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp", "png"),
  prt_ch_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch <- plot_ensemble_performance(
  cumhosp_rwsum,
  sl_cumhosp_risktables,
  titlestring = "Cumulative hospitalizations"
)

pep_ch

## Median prediction risk.
ch_medrisk <- ch_dist[type == "sim.lmin", log(mean(abs(median(value) - value)))]

pep_ch_out <- pep_ch +
  geom_hline(
    aes(yintercept = ch_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction",
    labels = c("Mean", "Ensemble")
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Cum-Hosp", "png"),
  pep_ch_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel <- summarize_learner_selection(cumhosp_rwsum)

fwrite(
  cumhosp_lrnr_sel,
  nicefile(tabslug, "Learner-Select_Cum-Hosp", "csv")
)


cumhosp_lrnr_sel[, lid := as.factor(lid)
                ][, lid := forcats::fct_reorder(lid, cnt_selected)] %>%
  ggplot(aes(x = lid, y = cnt_selected, fill = lclass)) +
  geom_segment(aes(xend = lid, yend = 0), color = "slategray") +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis_d(name = "Learner class", option = "magma") +
  labs(
    x = "Component learner",
    y = "Number of weeks selected (out of 30)",
    title = "Component learner selection (cumulative hospitalizations)"
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    legend.position = "bottom"
  )


################################################################################
## COMBINED ENSEMBLE PERFORMANCE ##
################################################################################

## Bind component learner data.
pep_main_weights <- rbindlist(
  idcol = "target",
  list(
    peakrate = pep_pr_out$data,
    peakweek = pep_pw_out$data,
    cumhosp = pep_ch_out$data
  )
)

pep_main_weights

## Bind ensemble risk data and calculate 95% confidence limits.
pep_main_ens <- rbindlist(
  idcol = "target",
  list(
    peakrate = rbindlist(sl_pkrate_risktables),
    peakweek = rbindlist(sl_pkweek_risktables),
    cumhosp = rbindlist(sl_cumhosp_risktables)
  )
)[learner == "SuperLearner"]

pep_main_ens[, ":="(
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

pep_main_ens

## Bind together the risks of the naive predictions
pep_main_naive <- data.table(
  target = c("peakrate", "peakweek", "cumhosp"),
  naiverisk = c(pr_medrisk, pw_medrisk, ch_medrisk)
)

## Make nice lables for each facet and set facet order.
facetlabs <- c(
  peakrate = "Peak rate",
  peakweek = "Peak week",
  cumhosp = "Cumulative hospitalizations"
)

forder <- names(facetlabs)

pep_main_weights[, target := factor(target, levels = forder, ordered = TRUE)]
pep_main_ens[, target := factor(target, levels = forder, ordered = TRUE)]
pep_main_naive[, target := factor(target, levels = forder, ordered = TRUE)]

## Plot ensemble performance across all prediction targets.
pep_main_all <- pep_main_weights %>%
  ggplot(aes(x = Week, y = log(mean_risk))) +
  geom_point(
    aes(size = weight, color = "Component"),
    shape = 21
  ) +
  geom_pointrange(
    data = pep_main_ens,
    aes(x = Week, y = log(mean_risk),
        ymin = log(ll95), ymax = log(ul95),
        color = "Ensemble"
        ),
    size = 0.3,
    shape = 21,
    fill = "white"
  ) +
  geom_hline(
    data = pep_main_naive,
    aes(
      yintercept = naiverisk,
      color = "Median predictor"
    ),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ target,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(target = facetlabs)
  ) +
  scale_color_manual(
    name = "Prediction source",
    values = c("#dddddd", "#990000", "black")
  ) +
  scale_size(name = "Weight") +
  guides(linetype = FALSE) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 8)
  )

ggsave(
  nicefile(figslug, "Ensemble-Summary_All-Targets", "png"),
  pep_main_all,
  width = 6,
  height = 10,
  unit = "in",
  device = "png"
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK RATE, LAMBDA 1SE ##
################################################################################

l1se_slug <- "Alternate trend filter penalty"

pkrate_risks_1se <- fmt_risk_table(
  dir = respr_1se,
  slug = "sl_pkrate",
  altslug = "1se"
)

pkrate_risks_1se

fwrite(
  pkrate_risks_1se,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-L1SE", "csv")
)

pkrate_weights_1se <- get_learner_weights(
  dir = respr_1se,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_1se <- join_learner_stats(
  risktables = sl_pkrate_risktables_1se,
  weights = pkrate_weights_1se
)

pkrate_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_1se)

### Risk tile plot
prt_pr_1se <- plot_risktiles(
  pkrate_rwsum_1se,
  titlestring = paste("Peak rate:", l1se_slug)
)

prt_pr_1se_out <- prt_pr_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate-L1SE", "png"),
  prt_pr_1se_out,
  width = 12,
  height = 6,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pr_1se <- plot_ensemble_performance(
  pkrate_rwsum_1se,
  sl_pkrate_risktables_1se,
  titlestring = paste("Peak rate:", l1se_slug)
)

## Median prediction risk.
pr_1se_medrisk <- pr_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_pr_1se_out <- pep_pr_1se +
  geom_hline(
    aes(yintercept = pr_1se_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_1se_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate-L1SE", "png"),
  pep_pr_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkrate_lrnr_sel_1se <- summarize_learner_selection(pkrate_rwsum_1se)
pkrate_lrnr_sel_1se

fwrite(
  pkrate_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_1se.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, LAMBDA 1SE ##
################################################################################

pkweek_risks_1se <- fmt_risk_table(
  dir = respw_1se,
  slug = "sl_pkweek",
  altslug = "1se"
)

pkweek_risks_1se

fwrite(
  pkweek_risks_1se,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-L1SE", "csv")
  )
)

pkweek_weights_1se <- get_learner_weights(
  dir = respw_1se,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_1se <- join_learner_stats(
  risktables = sl_pkweek_risktables_1se,
  weights = pkweek_weights_1se
)

pkweek_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_1se)

### Risk tile plot
prt_pw_1se <- plot_risktiles(
  pkweek_rwsum_1se,
  titlestring = paste("Peak week:", l1se_slug)
)

prt_pw_1se_out <- prt_pw_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.0")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week-L1SE", "png"),
  prt_pw_1se_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pw_1se <- plot_ensemble_performance(
  pkweek_rwsum_1se,
  sl_pkweek_risktables_1se,
  titlestring = paste("Peak week:", l1se_slug)
)

## Median prediction risk.
pw_1se_medrisk <- pw_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_pw_1se_out <- pep_pw_1se +
  geom_hline(
    aes(yintercept = pw_1se_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week-L1SE", "png"),
  pep_pw_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel_1se <- summarize_learner_selection(pkweek_rwsum_1se)
pkweek_lrnr_sel_1se

fwrite(
  pkweek_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_1se.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUMULATIVE HOSPITALIZATIONS, LAMBDA 1SE ##
################################################################################

cumhosp_risks_1se <- fmt_risk_table(
  dir = resch_1se,
  slug = "sl_cumhosp",
  altslug = "1se"
)

cumhosp_risks_1se

fwrite(
  cumhosp_risks_1se,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-L1SE", "csv"))
)

cumhosp_weights_1se <- get_learner_weights(
  dir = resch_1se,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_1se <- join_learner_stats(
  risktables = sl_cumhosp_risktables_1se,
  weights = cumhosp_weights_1se
)

cumhosp_rwsum_1se

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_1se)

### Risk tile plot
prt_ch_1se <- plot_risktiles(
  cumhosp_rwsum_1se,
  titlestring = paste("Cumulative hospitalizations:", l1se_slug)
)

prt_ch_1se_out <- prt_ch_1se +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_1se_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp-L1SE", "png"),
  prt_ch_1se_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch_1se <- plot_ensemble_performance(
  cumhosp_rwsum_1se,
  sl_cumhosp_risktables_1se,
  titlestring = paste("Cumulative hospitalizations:", l1se_slug)
)

## Median prediction risk.
ch_1se_medrisk <- ch_dist[
  type == "sim.l1se",
  log(mean(abs(median(value) - value)))
]

pep_ch_1se_out <- pep_ch_1se +
  geom_hline(
    aes(yintercept = ch_1se_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_1se_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_CumHosp-L1SE", "png"),
  pep_ch_1se_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel_1se <- summarize_learner_selection(cumhosp_rwsum_1se)
cumhosp_lrnr_sel_1se

fwrite(
  cumhosp_lrnr_sel_1se,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_1se.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK RATE, ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

erfslug <- "Elastic Net, Random Forest Learner Subset"

pkrate_risks_erf <- fmt_risk_table(
  dir = respr_erf,
  slug = "sl_pkrate",
  altslug = "erf"
)

pkrate_risks_erf

fwrite(
  pkrate_risks_erf,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-ERF", "csv")
)

pkrate_weights_erf <- get_learner_weights(
  dir = respr_erf,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_erf <- join_learner_stats(
  risktables = sl_pkrate_risktables_erf,
  weights = pkrate_weights_erf
)

pkrate_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_erf)

### Risk tile plot
prt_pr_erf <- plot_risktiles(
  pkrate_rwsum_erf,
  titlestring = paste("Peak rate:", erfslug)
)

prt_pr_erf_out <- prt_pr_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_erf_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate-ERF", "png"),
  prt_pr_erf_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_pr_erf <- plot_ensemble_performance(
  pkrate_rwsum_erf,
  sl_pkrate_risktables_erf,
  titlestring = paste("Peak rate:", erfslug)
)

## Median prediction risk.
pr_erf_medrisk <- pr_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_pr_erf_out <- pep_pr_erf +
  geom_hline(
    aes(yintercept = pr_erf_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_erf_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate-ERF", "png"),
  pep_pr_erf_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkrate_lrnr_sel_erf <- summarize_learner_selection(pkrate_rwsum_erf)
pkrate_lrnr_sel_erf

fwrite(
  pkrate_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_erf.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

pkweek_risks_erf <- fmt_risk_table(
  dir = respw_erf,
  slug = "sl_pkweek",
  altslug = "erf"
)

pkweek_risks_erf

fwrite(
  pkweek_risks_erf,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-ERF", "csv")
  )
)

pkweek_weights_erf <- get_learner_weights(
  dir = respw_erf,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_erf <- join_learner_stats(
  risktables = sl_pkweek_risktables_erf,
  weights = pkweek_weights_erf
)

pkweek_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_erf)

### Risk tile plot
prt_pw_erf <- plot_risktiles(
  pkweek_rwsum_erf,
  titlestring = paste("Peak week:", erfslug)
)

prt_pw_erf_out <- prt_pw_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1.0")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_erf_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week-ERF", "png"),
  prt_pw_erf_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pw_erf <- plot_ensemble_performance(
  pkweek_rwsum_erf,
  sl_pkweek_risktables_erf,
  titlestring = paste("Peak week:", erfslug)
)

## Median prediction risk.
pw_erf_medrisk <- pw_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_pw_erf_out <- pep_pw_erf +
  geom_hline(
    aes(yintercept = pw_erf_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pw_erf_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week-ERF", "png"),
  pep_pw_erf_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel_erf <- summarize_learner_selection(pkweek_rwsum_erf)
pkweek_lrnr_sel_erf

fwrite(
  pkweek_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_erf.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUM. HOSP., ELASTNET + RANDFOREST COMPONENT SUBSET ##
################################################################################

cumhosp_risks_erf <- fmt_risk_table(
  dir = resch_erf,
  slug = "sl_cumhosp",
  altslug = "erf"
)

cumhosp_risks_erf

fwrite(
  cumhosp_risks_erf,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-ERF", "csv"))
)

cumhosp_weights_erf <- get_learner_weights(
  dir = resch_erf,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_erf <- join_learner_stats(
  risktables = sl_cumhosp_risktables_erf,
  weights = cumhosp_weights_erf
)

cumhosp_rwsum_erf

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_erf)

### Risk tile plot
prt_ch_erf <- plot_risktiles(
  cumhosp_rwsum_erf,
  titlestring = paste("Cumulative hospitalizations:", erfslug)
)

prt_ch_erf_out <- prt_ch_erf +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_erf_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp-ERF", "png"),
  prt_ch_erf_out,
  width = 12,
  height = 6.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch_erf <- plot_ensemble_performance(
  cumhosp_rwsum_erf,
  sl_cumhosp_risktables_erf,
  titlestring = paste("Cumulative hospitalizations:", erfslug)
)

## Median prediction risk.
ch_erf_medrisk <- ch_dist[
  type == "sim.lmin",
  log(mean(abs(median(value) - value)))
]

pep_ch_erf_out <- pep_ch_erf +
  geom_hline(
    aes(yintercept = ch_erf_medrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_erf_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_CumHosp-ERF", "png"),
  pep_ch_erf_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel_erf <- summarize_learner_selection(cumhosp_rwsum_erf)
cumhosp_lrnr_sel_erf

fwrite(
  cumhosp_lrnr_sel_erf,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_erf.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK RATE, SQUARED ERROR LOSS ##
################################################################################

sqeslug <- "Squared Error Loss"

pkrate_risks_sqe <- fmt_risk_table(
  dir = respr_sqe,
  slug = "sl_pkrate",
  altslug = "sqe"
)

pkrate_risks_sqe

fwrite(
  pkrate_risks_sqe,
  nicefile(tabslug, "Risks-EDSL-Mean_PeakRate-SQE", "csv")
)

pkrate_weights_sqe <- get_learner_weights(
  dir = respr_sqe,
  slug = "sl_pkrate",
  metalearner_is = "solnp"
)

### Data summary
pkrate_rwsum_sqe <- join_learner_stats(
  risktables = sl_pkrate_risktables_sqe,
  weights = pkrate_weights_sqe
)

pkrate_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(pkrate_rwsum_sqe)

### Risk tile plot
prt_pr_sqe <- plot_risktiles(
  pkrate_rwsum_sqe,
  titlestring = paste("Peak rate:", sqeslug)
)

prt_pr_sqe_out <- prt_pr_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.25", "0.50", "0.75", "1")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pr_sqe_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Rate-SQE", "png"),
  prt_pr_sqe_out,
  width = 12,
  height = 4.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_pr_sqe <- plot_ensemble_performance(
  pkrate_rwsum_sqe,
  sl_pkrate_risktables_sqe,
  titlestring = paste("Peak rate:", sqeslug)
)

## Median prediction risk.
pr_sqe_mnrisk <- pr_dist[
  type == "sim.lmin",
  log(mean((mean(value) - value)^2))
]

pep_pr_sqe_out <- pep_pr_sqe +
  geom_hline(
    aes(yintercept = pr_sqe_mnrisk, linetype = "Mean prediction risk"),
    color = "red"
  ) +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pr_sqe_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Rate-SQE", "png"),
  pep_pr_sqe_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)



### Summarize learner selection
pkrate_lrnr_sel_sqe <- summarize_learner_selection(pkrate_rwsum_sqe)
pkrate_lrnr_sel_sqe

fwrite(
  pkrate_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_pkrate_lrnr_selection_sqe.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: PEAK WEEK, SQUARED ERROR LOSS ##
################################################################################

sqe_slug <- "Squared error loss"

pkweek_risks_sqe <- fmt_risk_table(
  dir = respw_sqe,
  slug = "sl_pkweek",
  altslug = "sqe"
)

pkweek_risks_sqe

fwrite(
  pkweek_risks_sqe,
  file.path(
    nicefile(tabslug, description = "Risks-EDSL-Mean_PeakWeek-SQE", "csv")
  )
)

pkweek_weights_sqe <- get_learner_weights(
  dir = respw_sqe,
  slug = "sl_pkweek",
  metalearner_is = "solnp"
)

### Data summary
pkweek_rwsum_sqe <- join_learner_stats(
  risktables = sl_pkweek_risktables_sqe,
  weights = pkweek_weights_sqe
)

pkweek_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(pkweek_rwsum_sqe)

### Risk tile plot
prt_pw_sqe <- plot_risktiles(
  pkweek_rwsum_sqe,
  titlestring = paste("Peak week:", sqeslug)
)

prt_pw_sqe_out <- prt_pw_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_pw_sqe_out

ggsave(
  nicefile(figslug, "Risktiles_Peak-Week-SQE", "png"),
  prt_pw_sqe_out,
  width = 12,
  height = 4.5,
  unit = "in",
  device = "png"
)

### Super Learner performance
pep_pw_sqe <- plot_ensemble_performance(
  pkweek_rwsum_sqe,
  sl_pkweek_risktables_sqe,
  titlestring = paste("Peak week:", sqe_slug)
)

## Median prediction risk.
pw_sqe_mnrisk <- pw_dist[
  type == "sim.lmin",
  log(mean((mean(value) - value)^2))
]

pep_pw_sqe_out <- pep_pw_sqe +
  geom_hline(
    aes(yintercept = pw_sqe_mnrisk, linetype = "Median prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_pw_sqe_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_Peak-Week-SQE", "png"),
  pep_pw_sqe_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
pkweek_lrnr_sel_sqe <- summarize_learner_selection(pkweek_rwsum_sqe)
pkweek_lrnr_sel_sqe

fwrite(
  pkweek_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_pkweek_lrnr_selection_sqe.csv"))
)


################################################################################
## SENSITIVITY ANALYSIS: CUMULATIVE HOSPITALIZATIONS, SQUARED ERROR LOSS ##
################################################################################

cumhosp_risks_sqe <- fmt_risk_table(
  dir = resch_sqe,
  slug = "sl_cumhosp",
  altslug = "sqe"
)

cumhosp_risks_sqe

fwrite(
  cumhosp_risks_sqe,
  file.path(nicefile("TAB", "Risks-EDSL-Mean_CumHosp-SQE", "csv"))
)

cumhosp_weights_sqe <- get_learner_weights(
  dir = resch_sqe,
  slug = "sl_cumhosp",
  metalearner_is = "solnp"
)

### Data summary
cumhosp_rwsum_sqe <- join_learner_stats(
  risktables = sl_cumhosp_risktables_sqe,
  weights = cumhosp_weights_sqe
)

cumhosp_rwsum_sqe

### Relabel randomForest learners in results
apply_rf_relabel(cumhosp_rwsum_sqe)

### Risk tile plot
prt_ch_sqe <- plot_risktiles(
  cumhosp_rwsum_sqe,
  titlestring = paste("Cumulative hospitalizations:", sqeslug)
)

prt_ch_sqe_out <- prt_ch_sqe +
  guides(alpha = FALSE) +
  scale_size_continuous(
    name = "Weight",
    labels = c("> 0.00", "0.2", "0.4", "0.6", "0.8")
  ) +
  theme(axis.text.y = element_text(hjust = 0.5))

prt_ch_sqe_out

ggsave(
  nicefile(figslug, "Risktiles_Cum-Hosp-SQE", "png"),
  prt_ch_sqe_out,
  width = 12,
  height = 4.5,
  unit = "in",
  device = "png"
)


### Super Learner performance
pep_ch_sqe <- plot_ensemble_performance(
  cumhosp_rwsum_sqe,
  sl_cumhosp_risktables_sqe,
  titlestring = paste("Cumulative hospitalizations:", sqeslug)
)

## Median prediction risk.
ch_sqe_mnrisk <- ch_dist[
  type == "sim.lmin",
  log(mean((mean(value) - value)^2))
]

pep_ch_sqe_out <- pep_ch_sqe +
  geom_hline(
    aes(yintercept = ch_sqe_mnrisk, linetype = "Mean prediction risk"),
    color = "red"
  ) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_color_viridis_d(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "Prediction"
  ) +
  scale_size_continuous(name = "Component Learner Weight")

pep_ch_sqe_out

ggsave(
  nicefile(figslug, "Ensemble-Summary_CumHosp-SQE", "png"),
  pep_ch_sqe_out,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)


### Summarize learner selection
cumhosp_lrnr_sel_sqe <- summarize_learner_selection(cumhosp_rwsum_sqe)
cumhosp_lrnr_sel_sqe

fwrite(
  cumhosp_lrnr_sel_sqe,
  file.path(paper_output, paste0(Sys.Date(), "_cumhosp_lrnr_selection_sqe.csv"))
)


################################################################################
## COMBINED ENSEMBLE SUMMARY PLOT: SQUARED ERROR LOSS ##
################################################################################

## Bind component learner data.
pep_sqe_weights <- rbindlist(
  idcol = "target",
  list(
    peakrate = pep_pr_sqe_out$data,
    peakweek = pep_pw_sqe_out$data,
    cumhosp = pep_ch_sqe_out$data
  )
)

pep_sqe_weights

## Bind ensemble risk data and calculate 95% confidence limits.
pep_sqe_ens <- rbindlist(
  idcol = "target",
  list(
    peakrate = rbindlist(sl_pkrate_risktables_sqe),
    peakweek = rbindlist(sl_pkweek_risktables_sqe),
    cumhosp = rbindlist(sl_cumhosp_risktables_sqe)
  )
)[learner == "SuperLearner"]

pep_sqe_ens[, ":="(
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

pep_sqe_ens

## Bind together the risks of the naive predictions
pep_sqe_naive <- data.table(
  target = c("peakrate", "peakweek", "cumhosp"),
  naiverisk = c(pr_sqe_mnrisk, pw_sqe_mnrisk, ch_sqe_mnrisk)
)

pep_sqe_weights[, target := factor(target, levels = forder, ordered = TRUE)]
pep_sqe_ens[, target := factor(target, levels = forder, ordered = TRUE)]
pep_sqe_naive[, target := factor(target, levels = forder, ordered = TRUE)]

pep_sqe_all <- pep_sqe_weights %>%
  ggplot(aes(x = Week, y = log(mean_risk))) +
  geom_point(
    aes(size = weight, color = "Component"),
    shape = 21
  ) +
  geom_pointrange(
    data = pep_main_ens,
    aes(x = Week, y = log(mean_risk),
        ymin = log(ll95), ymax = log(ul95),
        color = "Ensemble"
        ),
    size = 0.3,
    shape = 21,
    fill = "white"
  ) +
  geom_hline(
    data = pep_sqe_naive,
    aes(
      yintercept = naiverisk,
      color = "Mean predictor"
    ),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ target,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(target = facetlabs)
  ) +
  scale_color_manual(
    name = "Prediction source",
    values = c("#dddddd", "#990000", "black")
  ) +
  scale_x_discrete(breaks = week_breaks) +
  scale_size(name = "Weight") +
  guides(linetype = FALSE) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 8)
  )

pep_sqe_all

ggsave(
  nicefile(figslug, "Ensemble-Summary_All-Targets-SQE", "png"),
  pep_sqe_all,
  width = 6,
  height = 10,
  unit = "in",
  device = "png"
)


################################################################################
## SENSITIVITY ANALYSIS: COMBINED FIGURE ##
################################################################################

## Main analysis objects did not have a suffix.
analysis <- c("", "_1se", "_erf")

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

  list(rwsum = rwsum, risktab = risktab[learner == "SuperLearner"])
})

names(sens_compare) <- c("main", "lse", "erf")
sens_compare

## Bind up component learner weight summaries.
sens_rwsum <- rbindlist(
  idcol = "analysis",
  list(
    main = sens_compare$main$rwsum,
    lse = sens_compare$lse$rwsum,
    erf = sens_compare$erf$rwsum
  )
)

## Bind up risk tables.
sens_risktab <- rbindlist(
  idcol = "analysis",
  list(
    main = sens_compare$main$risktab,
    lse = sens_compare$lse$risktab,
    erf = sens_compare$erf$risktab
  )
)

sens_risktab[, ":="(
  ll95 = mean_risk - qnorm(0.975) * SE_risk,
  ul95 = mean_risk + qnorm(0.975) * SE_risk
)]

## Match up naive (median predictor) risk estimates.
sens_naive <- data.table(
  analysis = c(rep("main", 3), rep("lse", 3), rep("erf", 3)),
  target = rep(c("pkrate", "pkweek", "cumhosp"), 3),
  naiverisk = c(
    pr_medrisk, pw_medrisk, ch_medrisk,
    pr_1se_medrisk, pw_1se_medrisk, ch_1se_medrisk,
    pr_medrisk, pw_medrisk, ch_medrisk
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

## Plot analysis comparisons.
pep_sens_compare <- ggplot(sens_rwsum) +
  geom_point(
    aes(
      x = Week, y = log(mean_risk),
      size = weight, color = "Component learner"
    ),
    shape = 21
  ) +
  geom_segment(
    ## This geom_segment adds an arrow where log(ll95) is undefined because the
    ## ll95 on the original scale was a negative number.
    data = sens_risktab[target == "pkweek" & Week == "20" & analysis == "erf"],
    aes(
      x = as.numeric(Week), xend = as.numeric(Week),
      y = log(ul95), yend = -3.308303,
      color = "Ensemble"
    ),
    lineend = "butt",
    linejoin = "mitre",
    size = 0.3,
    arrow = arrow(length = unit(0.02, "npc"))
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

ggsave(
  nicefile(figslug, "Ensemble-Summary_All-Targets_Main-1SE-ERF", "png"),
  pep_sens_compare,
  width = 10,
  height = 8,
  unit = "in",
  device = "png"
)


################################################################################
## DIFFERENCE PLOTS ACROSS SENSITIVITY ANALYSIS ##
################################################################################

sens_normscale <- sens_risktab %>%
  ggplot(
    aes(x = Week, y = mean_risk, group = analysis)
  ) +
  geom_hline(
    data = sens_naive,
    aes(yintercept = exp(naiverisk), color = "Median predictor"),
    linetype = "dashed"
  ) +
  geom_linerange(
    aes(ymin = ll95, ymax = ul95, color = "Ensemble (95% CL)"),
    alpha = 0.3
  ) +
  geom_point(
    aes(color = "Ensemble (95% CL)"),
    fill = "white",
    shape = 21
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
  ylab("Mean risk") +
  xlab("Week") +
  scale_color_manual(name = "Prediction", values = c("#990000", "black")) +
  scale_x_discrete(breaks = week_breaks) +
  theme_base(base_family = global_plot_font) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 8)
  )

sens_normscale

ggsave(
  nicefile(figslug, "Ensemble-Summary_All-Targets_Regular-Scale", "png"),
  sens_normscale,
  width = 11,
  height = 8,
  unit = "in",
  device = "png"
)
