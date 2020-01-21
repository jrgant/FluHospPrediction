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

whsp_fsn_rt <- fread(hsp_fsn_rates,
                     col.names = hsp_rate_cols,
                     quote = "") %>%
  .[, weekint := assign_weekint(mmwr_week)]

print(whsp_fsn_rt)

dfSummary(whsp_fsn_rt) %>% view


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
  .[, ":="(network = "EIP",
           weekint = assign_weekint(mmwr_week))]

print(whsp_eip_rt)


# %% Compare FluSurv-NET to EIP (2009-2019) ------------------------------------

grabcols <- c("season", "mmwr_week", "weekint", "weekrate", "cumrates")

hsp_rate_compare <- whsp_fsn_rt[whsp_eip_rt, 
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


# %% ILINet Data --------------------------------------------------------------

ili_file <- here::here("data", "raw", "flu", "ILINET.csv")
ili_dat  <- fread(ili_file)

# clean up column names

ili_colnames <-
  names(ili_dat) %>%
  tolower %>%
  str_replace_all("\\s", "\\_") %>%
  str_replace_all("^\\%\\_|\\%", "pct_") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\-", "to")

print(ili_colnames)

# select columns

ili_col_select <-
  ili_colnames %>%
  .[!grepl("region|age", .) & grepl("year|week|^pct", .)]

names(ili_dat) <- ili_colnames
ili_dat <- ili_dat[, ..ili_col_select]

setnames(ili_dat, "week", "mmwr_week")
setnames(ili_dat, "pct_weighted_ili", "pctw_ili")
setnames(ili_dat, "pct_unweighted_ili", "pctunw_ili")
print(ili_dat)

# add variables and sort

ili_dat <-
  ili_dat %>%
    .[, weekint := assign_weekint(mmwr_week)] %>%
    .[year %in% 2003:2019 & weekint %in% -2:30] %>%
    setDT

print(ili_dat)

# check ranges

ili_dat[, .(range_weekint = range(weekint), 
            range_epiweek = range(mmwr_week))]

ili_dat[, season :=
  ifelse(weekint %in% -2:13,
    paste0(year, "-", str_extract(year + 1, "[0-9]{2}$")),
    paste0(year - 1, "-", str_extract(year, "[0-9]{2}$")))]

ili_dat <- ili_dat[!season %in% c("2002-03", "2009-10")]

# create lag variables
ili_dat[, ":="(pctw_ili_lag1 = shift(pctw_ili, type = "lag"),
               pctw_ili_lag2 = shift(pctw_ili, n = 2, type = "lag"))]


ili_dat <- ili_dat[
  weekint %in% 0:30,
  .(season, mmwr_week, weekint, pctunw_ili, pctw_ili, pctw_ili_lag1, pctw_ili_lag2)
  ]
print(ili_dat)


# %% Check ILI weeks vs. Hospitalization Weeks ------------------------------

# Check number of weeks for each season

rt_seas_n <- whsp_eip_rt[, .(hosp_rt = .N), season]
il_seas_n <- ili_dat[, .(ili = .N), season]

# @NOTE 
# - season 2009-10: extra epiweeks in FluSurv-NET, but pandemic influenza 
#   season, to be dropped anyway

merge(rt_seas_n, il_seas_n, by = "season") %>%
  .[, check := (hosp_rt + ili) / hosp_rt == 2] %>%
  print

# max(N) -- should be 1
ili_dat[, .N, c("season", "mmwr_week", "weekint")][, max(N)]

# %% Plot to Check Proper weekint labeling

ili_dat[, .N, .(weekint, mmwr_week)]


# %% Viral Activity -----------------------------------------------------------

# @SOURCE:

# National Center for Immunization and Respiratory Diseases (NCIRD). U.S.
# Influenza Surveillance System: Purpose and Methods [Internet]. Centers for
# Disease Control and Prevention. 2019 [cited 2020 Jan 9]. Available from:
# https://www.cdc.gov/flu/weekly/overview.htm

# @NOTE:

# - For seasons prior to 2015-2016, clinical laboratory data were combined
#   with data from public health laboratories (comb dataset)
# - CDC notes that public health labs often receive reports from clinical labs,
#   so the combined stats contain some overlap
# - Clinical labs are preferred (see Source above)
# - For 2015-16 onward, use clinical labs only
# - Use combined estimates for prior seasons due to lack of breakouts by
#   clinical vs. public labs

vrl <- list.files(
  here::here("data", "raw", "flu"), pattern = "^WHO",
  full.names = T
  )

print(vrl)

clin <- fread(vrl[[1]])   # clinical labs, 2015-16 onward
comb <- fread(vrl[[2]])   # combined clinical + public health, before 2015-16

names(clin) <- tolower(names(clin))
names(comb) <- tolower(names(comb))

str(clin)
str(comb)

st_options(dfSummary.graph.col = F)
dfSummary(clin)
dfSummary(comb)


## check for time overlap in clinical and combined datasets
clco_sharedyear <- unique(clin$year)[unique(clin$year) %in% unique(comb$year)]
clco_sharedyear

table(clin[year == clco_sharedyear, week] %in% comb[year == clco_sharedyear, week])

select_vrlcols <- c("year", "week", "percent positive")

viral_dat <- rbind(comb[, ..select_vrlcols], 
               clin[, ..select_vrlcols]) %>%
  .[, weekint := assign_weekint(week)] %>%
  .[, season := case_when(
          weekint <= 13 ~ paste(year, 
                                str_extract(year + 1, "[0-9]{2}$"), 
                                sep = "-"),
          weekint >= 14 ~ paste(year - 1, 
                                str_extract(year, "[0-9]{2}$"), 
                                sep = "-")
    )] %>%
  # filter to desired seasons and weeks
  .[season %in% season_levels() & 
      season != "2009-10" & 
      weekint %in% -2:30] %>%
  .[, .(season, 
        mmwr_week = week, 
        weekint, 
        viral_flupct = `percent positive`)] %>%
  .[, ":="(viral_flupct_lag1 = shift(viral_flupct, type = "lag"),
           viral_flupct_lag2 = shift(viral_flupct, n = 2, type = "lag"))] %>%
  # drop negative weeks
  .[weekint >= 0]

print(viral_dat)

ggplot(viral_dat,
       aes(x = weekint,
           y = viral_flupct,
           col = factor(season))) +
  geom_line() +
  labs(title = "Percent positive for flu") +
  theme_ridges()

# view flupct by week

ggplot(viral_dat, aes(x = viral_flupct,
                  y = factor(weekint),
                  fill = ..x..)) +
  geom_density_ridges_gradient(
    scale = 0.95,
    jittered_points = T,
    point_shape = "|",
    position = position_points_jitter(height = 0)) +
  scale_fill_viridis_c(name = "Percent positive") +
  labs(x = "Percent of specimens positive for influenza",
       y = "Week (integer)") +
  theme_ridges(center = T)


# %% Holiday Epiweeks ---------------------------------------------------------

# @CITATION:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling
# of Epidemics with an Empirical Bayes Framework. PLoS Comput Biol
# 2015;11:e1004382. doi:10.1371/journal.pcbi.1004382.

# holiday years
# drops pandemic flu (2009-2010)
holyrs <- seq(2003, 2018, 1) %>% .[. != 2009]

# get epiweeks for Christmas
xmas <- sapply(paste(holyrs, "12", "25", sep = "-"), lubridate::epiweek)
xmas

xmas_epiweeks <- range(xmas)
xmas_epiweeks

# get epiweeks for Thanksgiving
# Date range for Thanksgiving: Nov. 22-28
tg_url <- "https://en.wikipedia.org/wiki/Thanksgiving_(United_States)"

tg_wiki <- read_html(tg_url) %>%
  html_nodes(".wikitable") %>%
  html_table() %>%
  .[[1]]

tg_wiki

tg_dates <- lapply(1:length(tg_wiki), function(x) {
  paste(na.omit(tg_wiki[[x]]), 11,
        str_extract(names(tg_wiki), "[0-9]{2}")[x],
        sep = "-")
  }) %>% unlist

print(tg_dates)

tg_epiweeks <- range(lubridate::epiweek(tg_dates))
print(tg_epiweeks)


# %% Merge Empirical Data ------------------------------------------------------

# select columns from datasets to merge
sel_ctcols <- c(
  "season",
  "weekint",
  "mmwr_week",
  paste0("flu.", c("a", "b", "ab", "unk", "tot"))
)

# Merge Hospitalizations and ILI Data
flumerge_full <- 
  whsp_eip_rt[!is.na(weekint)] %>%
    merge(., ili_dat,
          by = c("season", "weekint", "mmwr_week"),
          all.x = TRUE) %>%
    merge(., viral,
          by = c("season", "weekint", "mmwr_week"),
          all.x = TRUE) %>%
    .[!season == "2009-10"]

names(flumerge_full)
print(flumerge_full)

sumvars <- c("cumrates", "weekrate",
             "pctw_ili", "pctw_ili_lag1", "pctw_ili_lag2",
             "viral_flupct", "viral_flupct_lag1", "viral_flupct_lag2")

fluweek_sum <- flumerge_full[, lapply(.SD, function(x) round(mean(x), 3)),
                               by = .(weekint, mmwr_week),
                               .SDcols = sumvars]

print(fluweek_sum)


# FULL DATA BY SEASON AND EPIWEEK

out_full <- flumerge_full %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

print(out_full)


# WEEKLY SUMMARIES

outweek_sum <- fluweek_sum %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

print(outweek_sum)


# %% Write Data to Files -------------------------------------------------------

# Merged data
fwrite(out_full, here::here("data", "cleaned", "empdat.csv"))

# Weekly averages
fwrite(outweek_sum, here::here("data", "cleaned", "empdat_weeksum.csv"))
