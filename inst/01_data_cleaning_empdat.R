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

hsp_fsn_rates <- here::here("data", "raw", "flu",
                            "FluSurveillance_FluSurv-NET_Entire Network_Data.csv")

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


# %% Merge Empirical Data ------------------------------------------------------

# select columns from datasets to merge
sel_ctcols <- c(
  "season",
  "weekint",
  "mmwr_week",
  paste0("flu.", c("a", "b", "ab", "unk", "tot"))
)

# Merge Hospitalizations and ILI Data
flumerge_full <- whsp_eip_rt[!is.na(weekint)] %>%
    merge(., whsp_fsn_rt[!is.na(weekint)], by = c("season", "weekint")) %>%
    .[!season == "2009-10"]

str(flumerge_full)
print(flumerge_full, topn = 50)

sumvars <- c("cumrates",
             "weekrate", "weekrate_lag1", "weekrate_lag2",
             "pctw_ili", "pctw_ili_lag1", "pctw_ili_lag2",
             "viral_flupct", "viral_flupct_lag1", "viral_flupct_lag2")

fluweek_sum <- flumerge_full[, lapply(.SD, function(x) round(mean(x), 3)),
                               by = .(weekint, mmwr_week),
                               .SDcols = sumvars]

str(fluweek_sum)
print(fluweek_sum, topn = 50)

# FULL DATA BY SEASON AND EPIWEEK

out_full <- flumerge_full %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

str(out_full)
print(out_full)


# WEEKLY SUMMARIES

outweek_sum <- fluweek_sum %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

str(outweek_sum)
print(outweek_sum)


# %% Plots ------------------------------------------------------------------

theme_set(theme_ridges())
rate_lab <- "rates per 100,000"

# HOSPITALIZATION RATES, WEEKLY

ggplot(outweek_sum, aes(x = weekint, y = weekrate)) +
  geom_line(data = out_full,
            aes(x = weekint,
                y = weekrate,
                group = season),
            col = "gray") +
  geom_line(size = 1) +
  labs(title = "Average weekly hospitalization rates",
       y = rate_lab)

# HOSPITALIZATION RATES, CUMULATIVE

ggplot(outweek_sum, aes(x = weekint, y = cumrates)) +
  geom_line(data = out_full,
            aes(x = weekint,
                y = cumrates,
                group = season),
            col = "gray") +
  geom_line(size = 1) +
  labs(title = "Average cumulative hospitalization rates",
       y = rate_lab)

# HOSPITALIZATION RATES AND LAGS, WEEKLY

ggplot(melt(outweek_sum,
            id.vars = "weekint",
            measure.vars = c("weekrate", "weekrate_lag1", "weekrate_lag2"),
            variable.name = "lagtype")) +
  geom_line(aes(x = weekint,
                y = value,
                linetype = lagtype)) +
  labs(title = "Hospitalization rates",
       y = rate_lab)

# WEIGHTED ILI AND LAGS, WEEKLY

ggplot(melt(outweek_sum,
            id.vars = "weekint",
            measure.vars = c("pctw_ili", "pctw_ili_lag1", "pctw_ili_lag2"),
            variable.name = "lagtype")) +
  geom_line(aes(x = weekint,
                y = value,
                linetype = lagtype)) +
  labs(title = "Weighted ILI %",
       y = "%")

# VIRAL ACTIVITY AND LAGS, WEEKLY

ggplot(melt(outweek_sum,
            id.vars = "weekint",
            measure.vars = c("viral_flupct", "viral_flupct_lag1", "viral_flupct_lag2"),
            variable.name = "lagtype")) +
  geom_line(aes(x = weekint,
                y = value,
                linetype = lagtype)) +
  labs(title = "Viral Activity %",
       y = "%")


# %% Write Data to Files -------------------------------------------------------

# Merged data
fwrite(out_full, here::here("data", "cleaned", "empdat.csv"))

# Weekly averages
fwrite(outweek_sum, here::here("data", "cleaned", "empdat_weeksum.csv"))
