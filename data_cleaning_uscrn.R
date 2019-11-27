# %% Setup ---------------------------------------------------------------------

pacman::p_load(
  crn,
  data.table,
  magrittr,
  stringr,
  summarytools,
  lubridate
)

# %% Climate Data -------------------------------------------------------------

## Definitions

# Absolute humidity - Weight of water vapor in air
# Relative humidity - Percentage of saturation point achieved for given air
#                     temperature (where saturation = 100% water vapor
#                     concentration)

# @CITATION
# Spellman FR, editor. Precipitation and evapotranspiration. The Handbook of
# Meteorology, Lanham, MD: Scarecrow Press; 2013, p. 58–67.

# @DATASOURCE
# Diamond HJ, Karl TR, Palecki MA, Baker CB, Bell JE, Leeper RD, et al. U.S.
# Climate Reference Network after One Decade of Operations: Status and
# Assessment. Bull Am Meteorol Soc 2013;94:485–98.
# doi:10.1175/BAMS-D-12-00170.1.

# Data location: ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/daily01

datadir <- here::here("data", "uscrn")

# %% Specify States of Interest ----------------------------------------------

# eip = Emerging Infections Program
eip_states <- c(
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Georgia" = "GA",
  "Maryland" = "MD",
  "Minnesota" = "MN",
  "New Mexico" = "NM",
  "New York" = "NY",
  "Oregon" = "OR",
  "Tennessee" = "TN"
)

oth_states <- c(
  "Michigan" = "MI",
  "Ohio" = "OH",
  "Utah" = "UT"
)

all_states <- c(eip_states, oth_states)
print(all_states)
length(all_states)


# %% Collate Data ------------------------------------------------------------

years <- 2003:2019

kept_paths <- list.files(paste(datadir, years, sep = "/"), full.names = T)

crn <- lapply(kept_paths, function(x) {
  fread(x)
  }) %>%
  rbindlist

varinfo <- fread(paste(datadir, "HEADERS.txt", sep = "/"), header = T)
varnames <- varinfo[1, ] %>% unlist
varnames

names(crn) <- tolower(varnames)
print(crn)

crnsub <- crn[, .(wbanno, lst_date, t_daily_avg, rh_daily_avg)]

crnsub

summary(crnsub)

# replace -9999 with NA
nasub <- names(crnsub)[grepl("daily", names(crnsub))]

crnsub[, c(nasub) := lapply(.SD, function(x) {
  ifelse(x == -9999, NA, x)
  }
  ), .SDcols = nasub]

# supplement date information
crnsub[, `:=`(year = str_extract(lst_date, "^20[0-9]{2}"),
              epiweek = epiweek(ymd(lst_date)))]
crnsub

# %% Missing Data Checks ------------------------------------------------------

# Missing Data
sapply(crnsub, function(x) sum(is.na(x)))
comp_crn <- crnsub[, .(wbanno, epiweek, t_daily_avg, rh_daily_avg)]

# Missingness by Climate Station
wbanno_t_miss <- comp_crn %>%
  .[, .(prop_missing = mean(is.na(t_daily_avg))), wbanno]

plot(density(wbanno_t_miss$prop_missing))
summary(wbanno_t_miss$prop_missing)

# Missingness by Epiweek

epiweek_t_miss <- comp_crn %>%
  .[, .(prop_missing = mean(is.na(t_daily_avg))), epiweek]

plot(density(epiweek_t_miss$prop_missing))
summary(epiweek_t_miss$prop_missing)

# @NOTE 2019-11-21, Re: Missing Temperature Data
#  - Very low missingness by both station and epiweek
#  - Proceed with "complete case" analysis

wbanno_rh_miss <- comp_crn[, .(pmiss = mean(is.na(rh_daily_avg))), wbanno]
wbanno_rh_miss

epiweek_rh_miss <- comp_crn[, .(pmiss = mean(is.na(rh_daily_avg))),
                              keyby = epiweek]
epiweek_rh_miss


# %% Temperatures -------------------------------------------------------------

# @CITATION
# Temperature conversion:
# https://www.weather.gov/media/epz/wxcalc/tempConvert.pdf
selweek <- c(37:53, 1:17)

temps <- crnsub[!is.na(t_daily_avg) & (epiweek %in% selweek),
                .(epiweek, t_daily_avg)] %>%
              .[, t_daily_avg_f := (9 / 5) * t_daily_avg + 32] %>%
              .[order(epiweek)]

temps

temps[, .N, epiweek]

temps_avg <-
  temps[, .(mn_t_daily_avg = mean(t_daily_avg_f),
            sd_t_daily_avg = sd(t_daily_avg_f)),
          keyby = epiweek] %>%
      .[, ll := mn_t_daily_avg - 1.96 * sd_t_daily_avg] %>%
      .[, ul := mn_t_daily_avg + 1.96 * sd_t_daily_avg]

print(temps_avg)


# %% Relative Humidity --------------------------------------------------------

rh <- crnsub[!is.na(rh_daily_avg) & (epiweek %in% selweek),
                .(epiweek, rh_daily_avg)] %>%
              .[order(epiweek)]

rh[, .N, epiweek]
