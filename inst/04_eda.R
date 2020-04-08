# %% LOAD ----------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))
pacman::p_load(ggthemes)

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
