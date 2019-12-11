# %% Setup ---------------------------------------------------------------------

pacman::p_load(
  data.table,
  magrittr,
  stringr,
  summarytools,
  lubridate,
  ggplot2,
  ggthemes
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
  "California"  = "CA",
  "Colorado"    = "CO",
  "Connecticut" = "CT",
  "Georgia"     = "GA",
  "Maryland"    = "MD",
  "Minnesota"   = "MN",
  "New Mexico"  = "NM",
  "New York"    = "NY",
  "Oregon"      = "OR",
  "Tennessee"   = "TN"
)

oth_states <- c(
  "Michigan" = "MI",
  "Ohio"     = "OH",
  "Utah"     = "UT"
)

all_states <- c(eip_states, oth_states)
print(all_states)
print(length(all_states))


# %% Collate Data ------------------------------------------------------------

years <- 2003:2019

kept_paths <- list.files(paste(datadir, years, sep = "/"), full.names = T)

crn <- lapply(kept_paths, function(x) {
  df <- fread(x)
  df[, state := stringr::str_extract(pattern = "(?<=20[0-9]{2}\\-)[A-Z]{2}",
                                    string = x)]
  }) %>%
  rbindlist

# Apply column names
varinfo <- fread(paste(datadir, "HEADERS.txt", sep = "/"), header = T)
varnames <- c(varinfo[1, ], "state") %>% unlist
varnames

names(crn) <- tolower(varnames)
crn

crnsub <- crn[, .(wbanno, state, lst_date, t_daily_avg, rh_daily_avg)]
crnsub

summary(crnsub)

# Replace -9999 with NA
nasub <- names(crnsub)[grepl("daily", names(crnsub))]

crnsub[, c(nasub) := lapply(.SD, function(x) {
  ifelse(x == -9999, NA, x)
  }
  ), .SDcols = nasub]

# Supplement date information
crnsub[, `:=`(year = str_extract(lst_date, "^20[0-9]{2}"),
              epiweek = epiweek(ymd(lst_date)))]

crnsub[, weekint := ifelse(epiweek %in% (40:53), epiweek - 39, epiweek + 14)]

crnsub

# %% Missing Data Checks ------------------------------------------------------

# MISSING DATA SUMMARY

sapply(crnsub, function(x) sum(is.na(x)))

comp_crn <- crnsub[, .(wbanno,
                       state,
                       epiweek,
                       weekint,
                       t_daily_avg,
                       rh_daily_avg)]

tibble::glimpse(comp_crn)


# FLUSURV-NET STATES MISSING FROM USCRN DATA

# @TODO 2019-12-10:
#  - Connecticut and Maryland not represented in USCRN
#  - Motivation for a sensitivity analysis where we repeat the procedures
#    while including Massachusetts and, say, Virginia as proxy climates?
#  - Go back and pull the CRN data needed

states_in_data <- unique(comp_crn$state)
missing_states <- all_states[!all_states %in% states_in_data]
print(missing_states)


# TEMPERATURE MISSINGNESS BY CLIMATE STATION

wbanno_t_miss <- comp_crn %>%
  .[, .(prop_missing = mean(is.na(t_daily_avg))), wbanno]

tibble::glimpse(wbanno_t_miss)

boxplot(wbanno_t_miss$prop_missing)
summary(wbanno_t_miss$prop_missing)

# TEMPERATURE MISSINGNESS BY EPIWEEK

epiweek_t_miss <- comp_crn[,
  .(prop_missing = mean(is.na(t_daily_avg))),
  weekint]

boxplot(epiweek_t_miss$prop_missing)
summary(epiweek_t_miss$prop_missing)
ggplot(epiweek_t_miss, aes(x = weekint, y = prop_missing)) +
  geom_point(size = 0.8) +
  geom_smooth() +
  theme_clean()


# @NOTE 2019-11-21, Re: Missing Temperature Data
#  - Very low missingness by both station and epiweek
#  - Proceed with "complete case" analysis


# TEMPERATURE MISSINGNESS BY STATE

state_t_miss <- comp_crn[,
  .(prop_missing = mean(is.na(t_daily_avg))),
  state]

state_t_miss
state_t_miss %>%
  ggplot(aes(x = state, prop_missing)) +
  geom_point() +
  theme_clean()


# RELATIVE HUMIDITY MISSINGNESS BY CLIMATE STATION

wbanno_rh_miss <- comp_crn[,
  .(prop_missing = mean(is.na(rh_daily_avg))), wbanno]

wbanno_rh_miss

boxplot(wbanno_rh_miss$prop_missing)
summary(wbanno_rh_miss$prop_missing)

ggplot(wbanno_rh_miss, aes(x = factor(wbanno), y = prop_missing)) +
  geom_point(size = 0.8) +
  theme_clean()


# RELATIVE HUMIDITY MISSINGNESS BY EPIWEEK

epiweek_rh_miss <- comp_crn[, .(prop_missing = mean(is.na(rh_daily_avg))),
                              keyby = weekint]
epiweek_rh_miss

boxplot(epiweek_rh_miss$prop_missing)
summary(epiweek_rh_miss$prop_missing)

ggplot(epiweek_rh_miss, aes(x = weekint, y = prop_missing)) +
  geom_point(size = 0.8) +
  geom_smooth() +
  theme_clean()

# @NOTE:
#  - Time and station coverage for relative humididty much less than for temp
#  - Much higher missingness in general


# RELATIVE HUMIDITY MISSINGNESS BY STATE

state_rh_miss <- comp_crn[,
  .(prop_missing = mean(is.na(rh_daily_avg))),
  state]

state_rh_miss %>%
  .[, state := forcats::fct_reorder(state, prop_missing)] %>%
  ggplot(aes(x = state,
             y = prop_missing)) +
  geom_point() +
  geom_segment(aes(y = 0, yend = prop_missing,
                   x = state, xend = state)) +
  theme_clean()

rh_missing <- comp_crn[, missing := ifelse(is.na(rh_daily_avg), 1, 0)]

rh_missing %>%
  .[, .(prop_missing = mean(missing)), .(state, weekint)] %>%
  ggplot(aes(x = factor(weekint),
             y = prop_missing)) +
  geom_segment(aes(x = weekint, xend = weekint,
                   y = 0, yend = prop_missing)) +
  facet_wrap(~ state) +
  theme_clean()

# %% Temperatures -------------------------------------------------------------

# @CITATION
# Temperature conversion:
# https://www.weather.gov/media/epz/wxcalc/tempConvert.pdf
selweek <- c(37:53, 1:17)

# @TODO 2019-11-26
#  - May need to transform temperature and relative humidity distributions to #    calculate mean and 95% confidence interval
#  - For each epiweek, confidence intervals should use sandwich estimator due
#    to multiple observations from climate stations (based on daily readings)

temps <- crnsub[!is.na(t_daily_avg) & (epiweek %in% selweek),
                .(wbanno, epiweek, weekint, state, t_daily_avg)] %>%
              # calculate fahrenheit
              .[, t_daily_avg_f := (9 / 5) * t_daily_avg + 32] %>%
              .[order(epiweek)]

temps

temps[, freq(weekint)] %>% print

temps[, epiweek := factor(epiweek, levels = selweek)]

temps_avg <-
  temps[, .(mn = mean(t_daily_avg_f),
            sd = sd(t_daily_avg_f),
            n = .N),
          keyby = epiweek]

print(temps_avg)


# %% Relative Humidity --------------------------------------------------------

rh <- crnsub[!is.na(rh_daily_avg) & (epiweek %in% selweek),
                .(epiweek, weekint, rh_daily_avg)] %>%
              .[order(epiweek)]

rh[, .N, epiweek]

rh[, epiweek := factor(epiweek, levels = epiweek_levels)]

rh_avg <-
  rh[, .(mn = mean(rh_daily_avg),
         sd = sd(rh_daily_avg),
         n = .N),
         keyby = epiweek]

print(rh_avg)

# %% Plot Climate Data --------------------------------------------------------

# TEMPERATURE

ggplot(temps, aes(x = epiweek, y = t_daily_avg_f)) +
  geom_point(size = 0.3) +
  theme_clean()

ggplot(temps, aes(x = t_daily_avg_f)) +
  geom_density() +
  facet_wrap(~ weekint) +
  theme_classic()

# RELATIVE HUMIDITY

ggplot(rh, aes(x = epiweek, y = rh_daily_avg)) +
  geom_point(size = 0.3) +
  theme_clean()

ggplot(rh, aes(x = rh_daily_avg)) +
  geom_density() +
  facet_wrap(~ weekint) +
  theme_clean()
