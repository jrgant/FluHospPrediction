# %% LOAD ----------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

pacman::p_load(
  ggthemes,
  ggridges,
  cowplot
)

datl <- fread(here::here("data", "cleaned", "sim_dataset_long.csv"))
datw <- fread(here::here("data", "cleaned", "sim_dataset_wide.csv"))

head(datl)
head(datw)

# %% SCATTERPLOTS --------------------------------------------------------------

theme_set(theme_base())

# Predictor: hospitalization rate
hosprate_pkrate <- ggplot(
  datl, aes(x = hosprate_100k, y = pkrate)
  ) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

hosprate_pkweek <- ggplot(
  datl, aes(x = hosprate_100k, y = pkweek)
  ) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

hosprate_cumhosp <- ggplot(
  datl, aes(x = hosprate_100k, y = cumhosp)
  ) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

hosprate_pkrate
hosprate_pkweek
hosprate_cumhosp

# Predictor: cumulative hospitalization rate
cumhosp_pkrate <- ggplot(datl, aes(x = cumhosp_100k, y = pkrate)) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

cumhosp_pkweek <- ggplot(datl, aes(x = cumhosp_100k, y = pkweek)) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

cumhosp_cumhosp <- ggplot(datl, aes(x = cumhosp_100k, y = cumhosp)) +
  geom_hex(bins = 70) +
  facet_wrap(~ weekint) +
  scale_fill_viridis_c()

cumhosp_pkrate
cumhosp_pkweek
cumhosp_cumhosp


# %% COMPARE SIMULATED SEASONS -------------------------------------------------

theme_tweak <- theme(
  panel.border = element_blank(),
  panel.background = element_blank(),
)

pkrates <- ggplot(datl, aes(x = pkrate, y = template)) +
  geom_boxplot(alpha = 0)

pkrates

pkrate_hist <- ggplot(datl, aes(x = pkrate)) +
  geom_histogram(color = "white", fill = "lightgray") +
  facet_wrap(~ template)

pkrate_hist

pkweeks <- ggplot(datl, aes(x = pkweek, y = template)) +
  geom_boxplot(alpha = 0) +
  coord_flip()

pkweeks

curveplot <- ggplot(datl, aes(x = weekint, y = hosprate_100k)) +
  geom_line(aes(group = cid, color = template), alpha = 0.3) +
  guides(color = "none") +
  facet_wrap(~template, ncol = 3)

curveplot
