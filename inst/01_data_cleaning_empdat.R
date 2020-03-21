# %% Set Global Options -------------------------------------------------------

suppressMessages(library(FluHospPrediction))

## set data.table print options
options(datatable.print.topn = 10)
options(datatable.print.class = TRUE)


# %% Empirical Hospitalization Rates (FluSurv-NET) -----------------------------

hsp_rate_cols <- c(
  "catchment",
  "network",
  "season",
  "year",
  "mmwr_week",
  "agecat",
  "cumrates",
  "weekrate"
)

hsp_fsn_rates <- here::here(
  "data", "raw", "flu", "FluSurveillance_FluSurv-NET_Entire Network_Data.csv"
)

# @NOTE:
# - A warning is thrown when fread() gets to the CDC disclaimer text contained
#   in the .csv file.
# - The warning is BENIGN.

whsp_fsn_rt <- fread(hsp_fsn_rates,
                     col.names = hsp_rate_cols,
                     quote = "") %>%
  # drop mmwr weeks out of sample
  .[mmwr_week != 53 & !mmwr_week %in% 35:39] %>%
  .[, ":="(network = "EIP",
           weekint = assign_weekint(mmwr_week))]


print(whsp_fsn_rt, topn = 50)

dfSummary(whsp_fsn_rt, graph.col = F)

# %% Empirical Hospitalization Rates (EIP) -------------------------------------

hsp_eip_rates <- here::here("data", "raw", "flu",
                            "FluSurveillance_EIP_Entire Network_Data.csv")

# @NOTE:
# - A warning is thrown when fread() gets to the CDC disclaimer text contained
#   in the .csv file.
# - The warning is BENIGN.

whsp_eip_rt <- fread(hsp_eip_rates,
                     col.names = hsp_rate_cols,
                     quote = "") %>%
  # drop age-specific rates and two variables
  .[agecat == "Overall", -c("catchment", "network")] %>%
  # drop mmwr weeks out of sample
  .[mmwr_week != 53 & !mmwr_week %in% 35:39] %>%
  .[, ":="(network = "EIP",
           weekint = assign_weekint(mmwr_week))]

print(whsp_eip_rt, topn = 50)


# %% Compare FluSurv-NET to EIP (2009-2019) ------------------------------------

grabcols <- c("season",
              "mmwr_week",
              "weekint",
              "weekrate",
              "cumrates")

hsp_rate_compare <- whsp_fsn_rt[
    whsp_eip_rt,
    on = c("season", "mmwr_week", "weekint")] %>%
  .[, ratediff := weekrate - i.weekrate] %>%
  .[!is.na(weekrate)]

summary(hsp_rate_compare$ratediff)


# @NOTE
# - FluSurv-NET and EIP provide very similar hospitalization rates
# - Stick with EIP
# - Consider including the plot below in a supplement to demonstrate the
#   similarity

hsp_rate_compare[!is.na(weekint)] %>%
  ggplot(aes(x = ratediff,
             y = factor(weekint))) +
  geom_density_ridges() +
  geom_vline(aes(xintercept = 0), col = "red") +
  labs(x = "Rate difference (per 100,000)") +
  theme_ridges()

# Merge FluSurv-NET and EIP data
flumerge_full <-
  whsp_eip_rt[!is.na(weekint)] %>%
  merge(., whsp_fsn_rt[!is.na(weekint)],
           by = c("season", "weekint", "mmwr_week"),
           suffixes = c("_eip", "_fsn")
           ) %>%
  .[!season == "2009-10"]

str(flumerge_full)
print(flumerge_full, topn = 20)
unique(flumerge_full$agecat_eip) == "Overall"
unique(flumerge_full$agecat_fsn) == "Overall"

sumvars <- c(
  "cumrates_eip", "weekrate_eip",
  "cumrates_fsn", "weekrate_fsn"
)

fluweek_sum <- flumerge_full[, lapply(.SD, function(x) round(mean(x), 3)),
                               by = weekint,
                               .SDcols = sumvars]

str(fluweek_sum)
print(fluweek_sum, topn = 50)

# Plots -

theme_set(theme_ridges())

rate_lab <- "rates per 100,000"
eip_lab <- "Emerging Infections Program"
fsn_lab <- "FluSurv Network"

# WEEKLY HOSPITALIZATION RATES, AVERAGE

ggplot(fluweek_sum, aes(x = weekint)) +
  geom_line(aes(y = weekrate_eip, color = eip_lab), size = 1, alpha = 0.7) +
  geom_line(aes(y = weekrate_fsn, color = fsn_lab), size = 1, alpha = 0.7) +
  scale_color_manual(name = "Source", values = c("black", "slategray")) +
  labs(
    "Average weekly hospitalization rates",
    y = rate_lab
    ) +
  theme(legend.position = "top")

# WEEKLY CUMULATIVE HOSPITALIZATION RATES, AVERAGE

ggplot(fluweek_sum, aes(x = weekint)) +
  geom_line(aes(y = cumrates_eip, color = eip_lab), size = 1, alpha = 0.7) +
  geom_line(aes(y = cumrates_fsn, color = fsn_lab), size = 1, alpha = 0.7) +
  scale_color_manual(name = "Source", values = c("black", "slategray")) +
  labs(
    "Average weekly cumulative hospitalization rates",
    y = rate_lab
    ) +
  theme(legend.position = "top")


# %% Write Data to Files -------------------------------------------------------

# Merged data
fwrite(whsp_eip_rt, here::here("data", "cleaned", "empdat.csv"))

# Weekly averages
fwrite(fluweek_sum, here::here("data", "cleaned", "empdat_weeksum.csv"))
