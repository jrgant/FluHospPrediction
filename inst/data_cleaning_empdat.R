# %% Set Global Options -------------------------------------------------------

library(FluHospPrediction)

## set data.table print options
options(datatable.print.topn = 10)
options(datatable.print.class = TRUE)

## labeling of epiweeks and seasons
epiweek_levels <- paste(c(40:53, 1:17))
epiweek_labels <- 1:31
cbind(epiweek_levels, epiweek_labels)


seas_levels <- paste(2003:2018, str_extract(2004:2019, "[0-9]{2}$"), sep = "-")
print(seas_levels)


# %% Empirical Hospitalization Counts -----------------------------------------

# %%
hsp_file <- here::here("data", "raw", "flu", "Weekly_Data_Counts.csv")

hsp_names <- c(
  "season",
  "mmwr_yrweek",
  "flu.a",
  "flu.b",
  "flu.ab",
  "flu.unk"
)

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
hsp_rates <- here::here("data", "raw", "flu",
                        "FluSurveillance_EIP_Entire Network_Data.csv")

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

# @NOTE:
# - A warning is thrown when fread() gets to the CDC disclaimer text contained
#   in the .csv file: benign.

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

print(whsp_rt)

# check weekint matching with mmwr_week
whsp_rt[, .N, by = c("weekint", "mmwr_week")]


# %% ILINet Data --------------------------------------------------------------

ili_file <- here::here("data", "raw", "flu", "ILINET.csv")
ili_dat  <- fread(ili_file)

# %% Select columns

ili_colnames <-
  names(ili_dat) %>%
  tolower %>%
  str_replace_all("\\s", "\\_") %>%
  str_replace_all("^\\%\\_|\\%", "pct_") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\-", "to")

print(ili_colnames)
names(ili_dat) <- ili_colnames

ili_col_select <-
  ili_colnames %>%
  .[!grepl("region|age", .)]

ili_dat <- ili_dat[, ..ili_col_select]
setnames(ili_dat, "week", "mmwr_week")
print(ili_dat)

# %% Merge ILI

# create a season variable that conforms to the hospitalization dataset format
ili_dat <-
  ili_dat %>%
    .[, mmwr_week := as.character(mmwr_week)] %>%
    .[, weekint := match(mmwr_week, epiweek_levels)] %>%
    filter(year %in% 2003:2019 & mmwr_week %in% epiweek_levels) %>%
    setDT

print(ili_dat)

ili_dat[, season :=
  ifelse(weekint %in% 1:14,
    paste0(year, "-", str_extract(year + 1, "[0-9]{2}$")),
    paste0(year - 1, "-", str_extract(year, "[0-9]{2}$")))]

ili_dat <- ili_dat[season != "2002-03"]

# Check number of weeks for each season
ct_seas_n <- whsp_ct[, .(hosp_ct = .N), season]
rt_seas_n <- whsp_rt[, .(hosp_rt = .N), season]
il_seas_n <- ili_dat[, .(ili = .N), season]

# @BUG 2019-11-03
#   - 2008-09: number of weeks in season don't match across hosp and ILI dfs
#   - 2009-10: also mismatched, but pandemic influenza season (to be dropped)
merge(ct_seas_n, rt_seas_n, by = "season") %>%
  merge(., il_seas_n, by = "season") %>%
  .[, check := (hosp_ct + hosp_rt + ili) / hosp_ct == 3] %>%
  print

print(ili_dat)


# check weeks
ili_dat[, .N, mmwr_week] %>%
  .[order(factor(mmwr_week, epiweek_levels, epiweek_labels))]

ili_dat[, .N, year]

# max(N) -- should be 1
ili_dat[, .N, c("season", "mmwr_week", "weekint")][, max(N)]

# %% Plot to Check Proper weekint labeling

ggplot(ili_dat, aes(x = weekint, y = as.numeric(mmwr_week))) +
  geom_point(size = 0.4) +
  geom_vline(aes(xintercept = 14.5), color = "red") +
  labs(caption = "Separation looks good") +
  facet_wrap(~season) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ilisum <- ili_dat[, .(mn_pct_wgt_ili = mean(pct_weighted_ili),
                      mn_pct_unwgt_ili = mean(pct_unweighted_ili)), weekint]
ilisum

# ILI Percent
ggplot(ilisum, aes(x = weekint)) +
  geom_line(aes(y = mn_pct_wgt_ili, linetype = "weighted")) +
  geom_line(aes(y = mn_pct_unwgt_ili, linetype = "unweighted")) +
  labs(title = "Weighted vs. Unweighted ILI %") +
  theme_minimal()



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

vrl <- list.files(paste0(rawdir, "/flu"), "^WHO", full.names = T)

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

table(clin[year == clco_sharedyear, week] %in%
  comb[year == clco_sharedyear, week])

select_vrlcols <- c("year", "week", "percent positive")

viral <- rbind(comb[, ..select_vrlcols], clin[, ..select_vrlcols]) %>%

  # subset to desired epiweeks
  .[week %in% c(40:53, 1:17), ] %>%

  # drop pandemic flu season
  .[!((year == 2009 & week %in% 40:53) | (year == 2010 & week %in% 1:17))] %>%

  # drop seasons prior to 2003
  .[year >= 2003 & !(year == 2003 & week %in% 1:17)] %>%

  # assign integer week
  .[, weekint := epiweek_labels[match(week, epiweek_levels)]] %>%

  # label flu season
  .[, season := case_when(
        weekint %in% 1:14 ~ paste(
          year, str_extract(year + 1, "[0-9]{2}$"), sep = "-"
          ),
        weekint %in% 15:31 ~ paste(
          year - 1, str_extract(year, "[0-9]{2}$"),  sep = "-")
          )] %>%

  # rename variables
  .[, .(season,
        year,
        mmwr_week = week,
        weekint,
        viral_flupct = `percent positive`)
        ]



viral[year == 2009, range(mmwr_week)]
viral[year == 2010, range(mmwr_week)]

print(viral, topn = 50)

theme_set(theme_clean())

ggplot(viral,
       aes(x = weekint,
           y = viral_flupct,
           col = factor(season))) +
  geom_line() +
  labs(title = "Percent positive for flu")

# view flupct by week

ggplot(viral, aes(x = viral_flupct,
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

viral_dat <- viral[, .(mean_vflupct = mean(viral_flupct)), weekint]

viral_dat

ggplot(viral_dat, aes(x = weekint, y = mean_vflupct)) +
  geom_line()

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
flumerge <-
  whsp_rt %>%
    merge(., whsp_ct[, ..sel_ctcols],
      by = c("season", "weekint"),
      all.x = TRUE
    ) %>%
    merge(., ili_dat[, -c("year", "mmwr_week")],
      by = c("season", "weekint"),
      all.x = TRUE)

names(flumerge)
print(flumerge)


outmerge <- flumerge %>%
  # create holiday indicators
  .[, xmas := mmwr_week %in% xmas_epiweeks] %>%
  .[, thanksgiving := mmwr_week %in% tg_epiweeks] %>%
  # create squared weekint
  .[, weekint2 := weekint^2]

flumerge


# %% Write Data to Files -------------------------------------------------------

# Merged data
fwrite(outmerge, here::here("data", "cleaned", "empdat.csv"))

# Weekly averages
