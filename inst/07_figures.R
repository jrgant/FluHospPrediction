# LOAD -------------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
          ggplot2,
          ggthemes,
          gridExtra,
          data.table
        )

resdir <- "results/paper_output"

# FIGURE SAVING FUNCTION -------------------------------------------------------

save_plot <- function(..., file) {
  ggsave(..., filename = paste0(file, ".pdf"), dev = "pdf")
  ggsave(..., filename = paste0(file, ".png"), dev = "png")
}

# FIGURE 1 ---------------------------------------------------------------------

emp <- fread("data/cleaned//empdat.csv")
sim <- readRDS("data/cleaned/hypothetical-curves.Rds")

theme_tweak <-
  theme_base(
    base_size = 16,
    base_family = "serif"
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 10, face = "italic"),
    plot.background = element_blank()
  )

emp_hosp <- ggplot(emp, aes(x = weekint, y = weekrate)) +
  geom_line(
    aes(group = season),
    alpha = 0.4
  ) +
  labs(
    x = "Week",
    y = "Observed hospitalization rate\n(per 100,000)"
  ) +
  theme_tweak

set.seed(9834784)
simsub <- 15

hyp_hosp_p <-
  ggplot(sim$outhc[cid %in% sample(cid, size = simsub)],
         aes(x = weekint,
             y = prediction)
         ) +
  geom_line(aes(group = cid),
            alpha = 0.4
            ) +
  coord_cartesian(y = c(0, 10)) +
  labs(x = "Week",
       y = "Simulated hospitalizations\n(per 100,000)"
       ) +
  theme_tweak +
  theme(legend.position = "none")

fig01 <- grid.arrange(emp_hosp, hyp_hosp_p)

# write the plot to files
save_plot(
  plot = fig01,
  width = 5.5,
  height = 9.5,
  file = file.path(resdir, "fig01")
)


# FIGURE 2 ---------------------------------------------------------

targets <- fread(here::here(resdir, "table-01_prediction-targets.csv"))

theme_set(
  theme_base(
    base_size = 14,
    base_family = "serif"
  ) +
  theme(
    axis.title = element_text(size = 8),
    plot.background = element_blank())
  )

sim_pkht_dist <- sim$outhc[, .(pkht = max(prediction)), cid]

sim_pkwk_dist <- sim$outhc[,
  .(pkwk = weekint[prediction == max(prediction)]), cid]

sim_cumhosp_dist <- sim$outhc[, cumhosp := cumsum(prediction), by = "cid"] %>%
  .[weekint == max(weekint)]

targets$type <- "Observed"

sim_pkht_dist$type <- "Simulated"
sim_pkwk_dist$type <- "Simulated"
sim_cumhosp_dist$type <- "Simulated"

xlab <- "Curve type"

pkht_compare <- rbind(targets[, .(pkht = `Peak rate`, type)],
      sim_pkht_dist[, .(pkht, type)]) %>%
  ggplot(aes(x = type, y = pkht)) +
  geom_boxplot(fill = "aliceblue") +
  labs(
    x = xlab,
    y = "Peak hospitalization rates (per 100,000)"
  )


pkwk_compare <- rbind(targets[, .(pkwk = `Peak week`, type)],
      sim_pkwk_dist[, .(pkwk, type)]) %>%
  ggplot(aes(x = type, y = pkwk)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.7) +
  labs(
    x = xlab,
    y = "Weeks in which peak hospitalizations occured"
  )

ch_compare <- rbind(targets[, .(cumhosp = `Cumulative rate`, type)],
      sim_cumhosp_dist[weekint == max(weekint), .(cumhosp, type)]) %>%
  ggplot(aes(x = type, y = cumhosp)) +
  geom_boxplot(fill = "aliceblue", outlier.size = 0.7) +
  labs(
    x = xlab,
    y = "Cumulative hospitalization rates (per 100,000)"
  )

fig02 <- grid.arrange(pkht_compare, pkwk_compare, ch_compare, nrow = 1)

save_plot(
  plot = fig02,
  width = 6.5,
  height = 3,
  file = file.path(resdir, "fig02")
)


# S1 FIGURE --------------------------------------------------------------------

## See 02_simulate_hospcurves.R


