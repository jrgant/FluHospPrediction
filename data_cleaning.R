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

# @CITATION:

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

# Label epiweeks and seasons
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
#   A warning is thrown when fread() gets to the CDC disclaimer text contained
#   in the csv file. Benign.
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

# @DEV 2019-11-03
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
library(ggplot2)
ggplot(ili_dat, aes(x = weekint, y = as.numeric(mmwr_week))) +
  geom_point(size = 0.4) +
  geom_vline(aes(xintercept = 14.5), color = "red") +
  labs(caption = "Separation looks good") +
  facet_wrap(~season) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# %%
ilisum <- ili_dat[, .(mn_pct_wgt_ili = mean(pct_weighted_ili),
                      mn_pct_unwgt_ili = mean(pct_unweighted_ili)), weekint]
ilisum

# %% ILI Percent
ggplot(ilisum, aes(x = weekint)) +
  geom_line(aes(y = mn_pct_wgt_ili, linetype = "weighted")) +
  geom_line(aes(y = mn_pct_unwgt_ili, linetype = "unweighted")) +
  labs(title = "Weighted vs. Unweighted ILI %") +
  theme_minimal()


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

tg_dates

tg_epiweeks <- range(lubridate::epiweek(tg_dates))
tg_epiweeks

# %% Temperature Data ----------------------------------------------------------

# @CITATION
# Variable glossaries from NOAA:
# https://www.ncdc.noaa.gov/cdo-web/webservices/v2

# ... States of interest

# eip = Emerging Infections Program
eip_states <- c(
  "California",
  "Colorado",
  "Connecticut",
  "Georgia",
  "Maryland",
  "Minnesota",
  "New Mexico",
  "New York",
  "Oregon",
  "Tennessee"
)

oth_states <- c("Michigan", "Ohio", "Utah")

all_states <- c(eip_states, oth_states)
print(all_states)


# ... Pull FluView states FIPS codes

# @NOTE: To pull data from NOAA's website, the user must request a token
#        from them and store it on their computer. The rnoaa package links to
#        the relevant instructions.

fips <- rnoaa::fipscodes
setDT(fips)
print(fips)

fipsdat <- as.data.table(
  ncdc_locs(locationcategoryid = "ST", limit = 52)$data
)
fipsdat

# State FIPS codes
fv_stfips <- fipsdat[name %in% all_states, c("name", "id")]
fv_stfips



# %% Function to pull station IDs in states represented in FLuView
# statefips = state's FIPS code
# stdt = start date
# enddt = end date
# limreq = limit request, defaults to maximum limit (1000)

get_stn_ids <- function(statefips,
                        stdt = "2003-10-01",
                        enddt = "2019-04-30",
                        limreq = 1000) {

    stndat <- ncdc_stations(locationid = statefips,
                            startdate = stdt,
                            enddate = enddt,
                            limit = limreq)

    totalrows <- stndat$meta$totalCount
    
    datlist <- list(
      as.data.table(stndat$data)
      )

    if (totalrows < limreq | limreq < 1000) {
      
      findat <- datlist[[1]][, .(stnid = id, name, mindate, maxdate)]
      findat[, stfips := statefips]

      list(
        rows_reported = totalrows,
        rows_in_data = nrow(findat),
        dat = findat,
        discrepancy = totalrows - nrow(findat)
      )

    } else {

      extra <- ceiling((totalrows - limreq) / limreq)
      pass_offset <- limreq + 1

      for (i in 1:extra) {
        
        stndatcurr <- ncdc_stations(
          locationid = statefips,
          limit = limreq,
          startdate = stdt,
          enddate = enddt,
          offset = pass_offset)

        datlist[[i + 1]] <- as.data.table(stndatcurr$data)
        pass_offset <- pass_offset + limreq

      }

      findat <- rbindlist(datlist)[, .(stnid = id, name, mindate, maxdate)]
      findat[, stfips := statefips]
      
      list(
        rows_reported = totalrows,
        rows_in_data = nrow(findat),
        dat = findat,
        discrepancy = totalrows - nrow(findat)
      )
    }
}

stations <- lapply(
  setNames(fv_stfips$id, fv_stfips$id), 
  function(x) get_stn_ids(statefips = x)
  )

# @DEV 2019-11-02:
#  - If time, put all these checks into a test folder

# check that stations from all states were pulled
unique(names(stations)) == unique(fv_stfips$id)
length(stations) == nrow(fv_stfips)

# check that there were no discrepancies in requested rows vs. returned
sapply(stations, function(x) x$rows_reported == x$rows_in_data)

# check that all states have the same columns
lapply(stations, function(x) names(x$dat))

stns <- lapply(stations, function(x) x$dat) %>% rbindlist
stns

# 33 duplicate station ids detected (72 rows)
length(unique(stns$stnid))
dups <- stns[, .N, .(stnid)][N > 1, .(stnid)]
dups

stns[stnid %in% dups$stnid][order(stnid)]   # grab rows for all dups
stns[grepl("NEXRAD", stnid)]                # view all NEXRAD stations

# @CITATION
# About the GHCDN: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

# @CONUNDRUM 2019-11-02

# @DESCRIPTION
# Search for state FIPS seems to return the stations that serve a given area.
# Example: FIPS:09 and FIPS:36 are CT and NY, respectively. Both return KBOX,
#          which is a NEXRAD station near Boston.

# @DECISION
# Keep these records. Presumably, they reflect the weather in areas of states
# included in the FluView data, even if the radards themselves are located in a
# neighboring state.



# %% Humidity Data ------------------------------------------------------------

# Absolute humidity - Weight of water vapor in air

# Relative humidity - Percentage of saturation point achieved for given air 
#                     temperature (where saturation = 100% water vapor 
#                     concentration)

# @CITATION

# Definition source:

# Spellman FR, editor. Precipitation and evapotranspiration. The Handbook of
# Meteorology, Lanham, MD: Scarecrow Press; 2013, p. 58–67.


# @CITATION

# Data source:

# National Climatic Data Center/NESDIS/NOAA/U.S. Department of Commerce,
# National Weather Service/NOAA/U.S. Department of Commerce, 557th Weather Wing/
# U.S. Air Force/U. S. Department of Defense, and Federal Aviation Agency/U.S. 
# Department of Transportation, 1987: NCDC TD3210 U.S. First Order Summary of
# Day. Research Data Archive at the National Center for Atmospheric Research,
# Computational and Information Systems Laboratory, Boulder, CO. [Available
# online at https://doi.org/10.5065/H0J8-V485.]




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
