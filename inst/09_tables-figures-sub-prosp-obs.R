################################################################################
## SETUP ##
################################################################################

source(here::here("inst", "06.1_tables-figures-setup.R"))

library(pacman)
p_load(data.table, ggplot2, ggthemes, magrittr)


################################################################################
## FILES ##
################################################################################

pobs <- list.files(here::here("results"), "ProspObs", full.names = TRUE)

fitl <- lapply(pobs, function(.x) {
  psim <- rbindlist(lapply(
    list.files(file.path(.x, "ProspSim"), full.names = TRUE),
    function(.y) {
      d <- as.data.table(readRDS(.y))
      d[, week := stringr::str_extract(.y, "(?<=w)[0-9]{2}")]
      return(d)
    }
  ))
  pobs <- rbindlist(lapply(
    list.files(file.path(.x, "Observed"), full.names = TRUE),
    function(.y) {
      d <- as.data.table(readRDS(.y))
      d[, week := stringr::str_extract(.y, "(?<=w)[0-9]{2}")]
      return(d)
    }
  ))
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
  list(
    sim = psim,
    obs = pobs
  )
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

pl_pobs <- pdatl[!measure %like% "err|outcome|md"] %>%
  ggplot(aes(x = factor(week), y = value)) +
  geom_segment(
    data = dcast(
      obs_season + outcome + week ~ measure + traindat,
      data = pdatl[!measure %like% "err|outcome|md"]
    ),
    aes(x = week, xend = week, y = sl_pred_sim, yend = sl_pred_obs),
    color = "gray80"
  ) +
  geom_hline(
    data = pdatl[measure == "obs_outcome"],
    aes(yintercept = value, linetype = "Target")
  ) +
  geom_hline(
    data = pdatl[measure == "md_pred"],
    aes(yintercept = value, linetype = "Median prediction")
  )+
  geom_point(
    aes(fill = traindat),
    size = 5,
    color = "black",
    shape = 21
  ) +
  facet_grid(
    outcome ~ obs_season,
    scales = "free_y",
    labeller = labeller(outcome = targlabs)
  ) +
  labs(y = "Value", x = "Week") +
  scale_linetype_manual(
    name = "Observed measures",
    values = c("dashed", "solid")
  ) +
  scale_y_continuous(n.breaks = 5) +
  scale_fill_grey(
    name = "Training data",
    labels = c("Observed", "Simulated")
  ) +
  scale_shape_manual(values = c(21, 24)) +
  theme_few(
    base_size = 16,
    base_family = "serif"
  ) +
  theme(
    panel.spacing = unit(0.25, "cm"),
    legend.position = "bottom",
    strip.text = element_text(size = 20, face = "bold")
  )

pl_pobs

plotsave(
  plot = pl_pobs,
  width = 10,
  height = 10,
  name = "Prospective-Observed-Application"
)
