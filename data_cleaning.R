# %% Setup -------------------------------------------------------------------

pacman::p_load(
  rvest,
  stringr,
  readr,
  dplyr,
  data.table,
  stringr,
  forcats,
  here,
  lubridate,
  rnoaa
)


# %% CDC Flu Season Severity -------------------------------------------------

# Method described in:

# Biggerstaff M et al. Systematic Assessment of Multiple Routine and Near
# Real-Time Indicators to Classify the Severity of Influenza Seasons and
# Pandemics in the United States, 2003-2004 Through 2015-2016. Am J Epidemiol
# 2018;187:1040–50. doi:10.1093/aje/kwx334.

# @NOTE:
# If simply interested in viewing cleaned data, skip to 'Set Global Options'

datfldr <- "data"

# Severity information is written to a file at the end of section
url <- "https://www.cdc.gov/flu/about/classifies-flu-severity.htm"

cdc_svr <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame %>%
  setDT

names(cdc_svr) <- tolower(names(cdc_svr))

cdc_svr[, ":="(
  sev1 = overall,
  sev2 = fct_collapse(overall,
    HiMod = c("High", "Moderate"),
    Low = "Low"
  )
)]

print(cdc_svr)

# frequency table
cdc_svr[, .N, by = c("sev1", "sev2")]

# Write the season severity designations to a file
fwrite(cdc_svr, file = here::here(datfldr, "cdc_svr.csv"), row.names = FALSE)

# %% Set Global Options -------------------------------------------------------

## set data.table print options
options(datatable.print.topn = 10)
options(datatable.print.class = TRUE)

# %% Empirical Hospitalization Counts -----------------------------------------

# CDC season severity data
cdcsvr_file <- here::here(datfldr, "cdc_svr.csv")

cdc_svr <- fread(cdcsvr_file, check.names = TRUE)
print(cdc_svr)

# %%
hsp_file <- here::here(datfldr, "Weekly_Data_Counts.csv")

hsp_names <- c(
  "season",
  "mmwr_yrweek",
  "flu.a",
  "flu.b",
  "flu.ab",
  "flu.unk"
)

# label epiweeks and seasons
epiweek_levels <- paste(c(40:53, 1:17))
epiweek_labels <- 1:31
seas_levels <- paste(2003:2018, str_extract(2004:2019, "[0-9]{2}$"), sep = "-")
seas_levels

# whsp_ct = weekly hospitalization counts
whsp_ct <- fread(hsp_file, col.names = hsp_names)
print(whsp_ct)

# %%
whsp_ct %>%
  .[, ":=" (year = str_extract(mmwr_yrweek, "^[0-9]{4}"),
            mmwr_yr = str_extract(season, "^[0-9]{4}"),
            mmwr_week = str_extract(mmwr_yrweek, "[0-9]{1,2}$"))] %>%
  .[, ":=" (weekint = match(mmwr_week, epiweek_levels))] %>%
  .[, flu.tot := rowSums(.SD, na.rm = TRUE),
                 .SDcols = paste0("flu.", c("a", "b", "ab", "unk"))]

print(whsp_ct)

# check recoding

# %%
# @NOTE 2019-08-21:
#   MMWR weeks < 40 are out of sample, so NAs are expected
whsp_ct[, .N, by = c("mmwr_yrweek", "mmwr_yr")]
whsp_ct[, .N, by = c("mmwr_yrweek", "mmwr_week")]
whsp_ct[, .N, by = c("mmwr_week", "weekint")]
whsp_ct[, .N, by = c("season", "year")]

# %% Empirical Hospitalization Rates -------------------------------------------

# %%
hsp_rates <- here::here(datfldr, "FluSurveillance_EIP_Entire Network_Data.csv")

whsp_rt_cols <- c(
  "catchment",
  "network",
  "season",
  "year",
  "mmwr_week",
  "agecat",
  "cumrates",
  "weekrate"
)

# NOTE 2019-08-21:
#   A warning is thrown when fread() gets to the CDC disclaimer text contained #   in the csv file. Benign.
whsp_rt <- fread(hsp_rates, col.names = whsp_rt_cols, quote = "") %>%
  # drop age-specific rates and two variables
  .[agecat == "Overall", -c("catchment", "network")] %>%
  .[, mmwr_week := as.character(mmwr_week)] %>%
  merge(., whsp_ct[, c("season", "mmwr_week", "weekint")],
        by = c("season", "mmwr_week"),
        all.x = TRUE) %>%
  # remove out-of-sample weeks
  tidyr::drop_na(.) %>%
  # order by season and factor-ordered mmwr_week
  .[order(season, factor(mmwr_week, epiweek_levels, epiweek_labels))]

whsp_rt

# check weekint matching with mmwr_week
whsp_rt[, .N, by = c("weekint", "mmwr_week")]


# %% ILINet Data --------------------------------------------------------------

ili_file <- here::here("data", "ILINET.csv")

ili_dat <- fread(ili_file)

ili_colnames <-
  names(ili_dat) %>%
  tolower %>%
  str_replace_all("\\s", "\\_") %>%
  str_replace_all("^\\%\\_|\\%", "pct_") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\-", "to")

ili_colnames

names(ili_dat) <- ili_colnames

ili_select <- ili_colnames %>% .[!grepl("region|age", .)]

ili_dat <- ili_dat[, ..ili_select]
setnames(ili_dat, "week", "mmwr_week")

# %%

# create a season variable that conforms to the hospitalization dataset format
ili_dat %>%
  .[, mmwr_week := as.character(mmwr_week)] %>%
  .[, weekint := match(mmwr_week, epiweek_levels)] %>%
  filter(year %in% 2003:2019 & mmwr_week %in% epiweek_levels) %>%
  setDT -> ili_dat

ili_dat

ili_dat[, season :=
  ifelse(weekint %in% 1:14,
    paste0(year, "-", str_extract(year + 1, "[0-9]{2}$")),
    paste0(year - 1, "-", str_extract(year, "[0-9]{2}$")))]

ili_dat <- ili_dat[season != "2002-03"]

ct_seas_n <- whsp_ct[, .N, season]
rt_seas_n <- whsp_rt[, .N, season]
il_seas_n <- ili_dat[, .N, season]

merge(ct_seas_n, rt_seas_n, by = "season") %>%
  merge(., il_seas_n, by = "season") %>%
  .[, check := all.equal(N.x, N.y, N)] %>%
  print

print(ili_dat)


# check weeks
ili_dat[, .N, mmwr_week] %>%
  .[order(factor(mmwr_week, epiweek_levels, epiweek_labels))]
ili_dat[, .N, year]

# max(N) should be 1
ili_dat[, .N, c("season", "mmwr_week", "weekint")][, max(N)]

# %% Plot to Check Proper weekint labeling
library(ggplot2)
ggplot(ili_dat, aes(x = weekint, y = as.numeric(mmwr_week))) +
  geom_point(size = 0.4) +
  geom_vline(aes(xintercept = 14.5), color = "red") +
  labs(caption = "Separation looks good") +
  facet_wrap(~season) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# %%
ilisum <- ili_dat[, .(mn_pwi = mean(pct_weighted_ili),
                      mn_pui = mean(pct_unweighted_ili)), weekint]
ilisum

# %% ILI Percent
ggplot(ilisum, aes(x = weekint)) +
  geom_line(aes(y = mn_pwi, linetype = "weighted")) +
  geom_line(aes(y = mn_pui, linetype = "unweighted")) +
  labs(title = "Weighted vs. Unweighted ILI %") +
  theme_minimal()


# %% Holiday Epiweeks ---------------------------------------------------------

# Sources:

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

tg_dates

tg_epiweeks <- range(lubridate::epiweek(tg_dates))
tg_epiweeks

# %% Save Empirical Data ------------------------------------------------------

# merge all weekly data
# dt1: whsp_ct
# dt2: whsp_rt
# dt2: cdc_svr

# select columns from datasets to merge
sel_ctcols <- c(
  "season",
  "weekint",
  paste0("flu.", c("a", "b", "ab", "unk", "tot"))
)

# %% Merge Hospitalizions and ILI Data
whsp_rt %>%
  merge(., whsp_ct[, ..sel_ctcols],
    by = c("season", "weekint"),
    all.x = TRUE
  ) %>%
  merge(., ili_dat[, -c("year", "mmwr_week")],
    by = c("season", "weekint"),
    all.x = TRUE) %>%
  merge(., cdc_svr, by = "season") -> flumerge

setnames(flumerge,
         c("child", "adults", "older.adults", "overall"),
         c("child_svr", "adults_svr", "older.adults_svr", "overall_svr"))

names(flumerge)
print(flumerge)

flumerge %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

flumerge

# %% Write Merged Data
saveRDS(flumerge, here::here(datfldr, "empdat.Rds"))
