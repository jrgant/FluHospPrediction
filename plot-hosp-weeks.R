
# Packages ------------------------------------------------------------------

# install.packages("pacman")
# install.packages("devtools")
# devtools::install_github("kmcconeghy/flumodelr)
pacman::p_load(flumodelr, magrittr, stringr, viridis, ggthemes, broom, gridExtra)

# Load Data -----------------------------------------------------------------

# empirical data
empdat <- readRDS("empdat.Rds")

# View Hospitalization Curves -----------------------------------------------

# drop pandemic influenza year
# drop 2018-19 due to lack of severity designation
wct_p <- empdat$whsp_ct %>% filter(!seas %in% c("2009-10", "2018-19"))

# plot theme tweaks
theme_tweak <- 
  theme_clean(base_size = 12) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "right",
        legend.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "in"))


seas_p <- wct_p %>%
  ggplot(aes(x = factor(epiweek),
             y = inf.tot,
             group = seas,
             color = severity)) +
  geom_line(size = 1, alpha = 0.7) +
  labs(x = "Epiweek", 
       y = "Influenza hospitalizations (A and B)") +
  scale_color_viridis_d("Severity", 
                        option = "inferno", 
                        end = 0.8) +
  scale_x_discrete(breaks = c(45, 50, 2, 7, 12)) +
  facet_grid(~severity) +
  theme_tweak +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

seas_p

# Summarize peak weeks
epiweek_levels <- levels(empdat$whsp_ct$epiweek)
epiweek_subset <- epiweek_levels[!epiweek_levels %in% 35:39]

peaks <- wct_p %>%
  group_by(seas) %>%
  filter(inf.tot == max(inf.tot)) %>%
  select(seas, 
         wk = epiweek,
         inf.tot,
         severity, 
         sev2) %>%
  mutate(wk2 = match(wk, epiweek_subset))

pkwk_p <- peaks %>%
  ggplot(aes(x = sev2, y = wk2)) +
  geom_point(size = 2, alpha = 0.5) +
  scale_y_continuous(labels = c(0, epiweek_subset[15], epiweek_subset[20], epiweek_subset[25])) +
  labs(x = "Severity", y = "Peak week (epiweek)") +
  theme_tweak + 
  theme(axis.line = element_blank())

pkcs_p <- peaks %>%
  ggplot(aes(x = sev2, y = inf.tot, label = seas)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(x = "Severity", y = "Peak cases (#)") +
  theme_tweak +
  theme(axis.line = element_blank())

hosp_grid <- grid.arrange(
  seas_p, pkwk_p, pkcs_p,
  layout_matrix = matrix(c(1, 1, 2, 1, 1, 3), nrow = 2, byrow = T)
  )
grid::grid.draw(hosp_grid)

# Save Grid in Analysis Plan Directory --------------------------------------
ggsave(filename = "analysis-plan/hospital-curve-empirical.pdf", plot = hosp_grid)
