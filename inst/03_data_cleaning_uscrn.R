# %% Setup ---------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

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

datadir <- here::here("data", "raw", "uscrn")

# Global epiweek selection
# (omits epiweek 53, as we're not counting the extra week in leap years)
selweek <- c(38:52, 1:17)

# Set global ggplot2 theme
theme_set(theme_clean())

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
print(crn)

crnsub <- crn[, .(wbanno,
                  state,
                  lst_date,
                  t_daily_avg,
                  rh_daily_avg)]
print(crnsub)
summary(crnsub)

# Replace -9999 with NA
nasub <- names(crnsub)[grepl("daily", names(crnsub))]

crnsub[, c(nasub) := lapply(.SD, function(x) {
  ifelse(x == -9999, NA, x)
  }), .SDcols = nasub]

# Supplement date information
crnsub[, `:=`(year = as.numeric(str_extract(lst_date, "^20[0-9]{2}")),
              epiweek = epiweek(ymd(lst_date)))] %>%
     .[, weekint := assign_weekint(epiweek)]

print(crnsub, topn = 50)
print(crnsub[epiweek == 52])
sort(unique(crnsub$epiweek))
sort(unique(crnsub$weekint))

# %% Missing Data Checks ------------------------------------------------------

# SET UP PLOT LABELS

## Global
miss_ylab <- "Proportion missing"
week_xlab <- "Week of flu season (integer)"
state_xlab <- "State in FluSurv-NET"
stn_xlab <- "Climate station"

# Temperature
temp_title <- "Missing temperature data (USCRN)"

# Relative humidity
humd_title <- "Relative humidity missingness (USCRN)"

# State caption
state_cap <- "Excludes CT and MD due to absence of climate stations."

# MISSING DATA SUMMARY

sapply(crnsub, function(x) sum(is.na(x)))

comp_crn <- crnsub[
  epiweek %in% selweek &
    # drop pandemic flu weeks
    !(year == 2009 & epiweek %in% 38:53) &
    !(year == 2010 & epiweek %in% 1:17) &
    state %in% eip_states, # subset to analysis epiweeks, seasons, and states
  .(wbanno,
    state,
    epiweek,
    weekint,
    year,
    t_daily_avg,
    rh_daily_avg)]

str(comp_crn)
sort(unique(comp_crn$epiweek))

# check all weekint in -2:29
range(sort(unique(comp_crn$weekint))) == c(-1, 30)

# check correct omission of pandemic flu weeks
lapply(setNames(selweek, selweek),
       function(x) comp_crn[epiweek == x, unique(year)])


# EIP STATES MISSING FROM USCRN DATA

# @TODO 2019-12-10:
#  - Connecticut and Maryland not represented in USCRN
#  - Motivation for a sensitivity analysis where we repeat the procedures
#    while including Massachusetts and, say, Virginia as proxy climates?
#  - Go back and pull the CRN data needed

states_in_data <- unique(comp_crn$state)
missing_states <- eip_states[!eip_states %in% states_in_data]
print(missing_states)


# TEMPERATURE MISSINGNESS BY CLIMATE STATION

wbanno_t_miss <- comp_crn %>%
  .[, .(prop_missing = mean(is.na(t_daily_avg))), wbanno]

str(wbanno_t_miss)

boxplot(wbanno_t_miss$prop_missing)
summary(wbanno_t_miss$prop_missing)

ggplot(wbanno_t_miss[, wbanno := factor(wbanno)],
       aes(x = forcats::fct_reorder(wbanno, prop_missing),
           y = prop_missing)) +
  geom_point() +
  geom_segment(
    aes(x = wbanno,
        xend = wbanno,
        y = 0,
        yend = prop_missing)
    ) +
  labs(title = temp_title,
       y = miss_ylab,
       x = stn_xlab) +
  theme(axis.text.x = element_blank())

# TEMPERATURE MISSINGNESS BY EPIWEEK

epiweek_t_miss <- comp_crn[,
  .(prop_missing = mean(is.na(t_daily_avg))),
  weekint]

boxplot(epiweek_t_miss$prop_missing)
summary(epiweek_t_miss$prop_missing)

ggplot(epiweek_t_miss,
       aes(x = weekint,
           y = prop_missing)) +
  geom_point() +
  geom_segment(aes(xend = weekint,
                   y = 0,
                   yend = prop_missing)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  labs(title = temp_title,
       y = miss_ylab,
       x = week_xlab)


# @NOTE 2019-11-21, Re: Missing Temperature Data
#  - Very low missingness by both station and epiweek
#  - Proceed with "complete case" analysis


# TEMPERATURE MISSINGNESS BY STATE

state_t_miss <- comp_crn[,
  .(prop_missing = mean(is.na(t_daily_avg))),
  state]

print(state_t_miss)

state_t_miss %>%
  ggplot(aes(x = forcats::fct_reorder(state, prop_missing),
             y = prop_missing)) +
  geom_point() +
  geom_segment(aes(x = state,
        xend = state,
        y = 0,
        yend = prop_missing)) +
  labs(title = temp_title,
       y = miss_ylab,
       x = state_xlab,
       caption = state_cap) +
  theme(plot.caption = element_text(face = "italic"))


# RELATIVE HUMIDITY MISSINGNESS BY CLIMATE STATION

wbanno_rh_miss <- comp_crn[,
  .(prop_missing = mean(is.na(rh_daily_avg))), wbanno]

print(wbanno_rh_miss)

boxplot(wbanno_rh_miss$prop_missing)
summary(wbanno_rh_miss$prop_missing)

ggplot(wbanno_rh_miss[, wbanno := factor(wbanno)],
  aes(x = forcats::fct_reorder(wbanno, prop_missing),
      y = prop_missing)) +
  geom_point() +
  geom_segment(aes(x = wbanno, xend = wbanno,
                   y = 0, yend = prop_missing)) +
  labs(title = humd_title,
       y = miss_ylab,
       x = stn_xlab) +
  theme(axis.text.x = element_blank())


# RELATIVE HUMIDITY MISSINGNESS BY EPIWEEK

epiweek_rh_miss <- comp_crn[, .(prop_missing = mean(is.na(rh_daily_avg))),
                              keyby = weekint]
print(epiweek_rh_miss)

boxplot(epiweek_rh_miss$prop_missing)
summary(epiweek_rh_miss$prop_missing)

ggplot(epiweek_rh_miss,
       aes(x = weekint,
           y = prop_missing)) +
  geom_point() +
  geom_segment(aes(xend = weekint, y = 0.5, yend = prop_missing )) +
  labs(title = humd_title,
       y = miss_ylab,
       x = week_xlab)


# @NOTE:
#  - Week/station coverage for relative humididty **much** less than for temp
#  - For some stations complete missingness (likely due to adding of relative
#    humidity instruments over time)
#  - Should we do anything to try to correct for this? "Selection" weight
#    by state? FluSurv-NET stats are also weighted by population. Could try
#    something similar.


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
  labs(title = humd_title,
       y = miss_ylab,
       x = state_xlab,
       caption = state_cap) +
  theme(plot.caption = element_text(face = "italic"))

rh_missing <- comp_crn[, missing := ifelse(is.na(rh_daily_avg), 1, 0)]

rh_missing %>%
  .[, .(prop_missing = mean(missing)), .(state, weekint)] %>%
  ggplot(aes(x = weekint, y = prop_missing)) +
  geom_point() +
  geom_segment(aes(x = weekint, xend = weekint,
                   y = 0, yend = prop_missing)) +
  facet_wrap(~ state) +
  theme_clean()


# %% Temperatures -------------------------------------------------------------

# @CITATION
# Temperature conversion:
# https://www.weather.gov/media/epz/wxcalc/tempConvert.pdf

# @TODO 2019-11-26
#  - May need to transform temperature and relative humidity distributions to
#    calculate mean and 95% confidence interval
#  - For each epiweek, confidence intervals should use sandwich estimator due
#    to multiple observations from climate stations (based on daily readings)

temps <- comp_crn[!is.na(t_daily_avg) & (epiweek %in% selweek),
                .(wbanno, epiweek, weekint, state, t_daily_avg)] %>%
              # calculate fahrenheit
              .[, t_daily_avg_f := (9 / 5) * t_daily_avg + 32] %>%
              .[order(weekint)]

print(temps)

temps[, freq(weekint)] %>% print

temps_avg <-
  temps[, .(mean_tempf = mean(t_daily_avg_f),
            sd_tempf = sd(t_daily_avg_f),
            n = .N),
          keyby = weekint] %>%
      .[, mean_tempf_lag1 := shift(mean_tempf, type = "lag")] %>%
      .[, mean_tempf_lag2 := shift(mean_tempf, n = 2, type = "lag")]

print(temps_avg)


# %% Relative Humidity --------------------------------------------------------

rh <- comp_crn[!is.na(rh_daily_avg) & (epiweek %in% selweek),
               .(epiweek, weekint, rh_daily_avg)] %>%
               .[order(weekint)]

rh_avg <-
  rh[, .(mean_rh = mean(rh_daily_avg),
         sd_rh = sd(rh_daily_avg),
         n = .N),
         keyby = weekint] %>%
    .[, mean_rh_lag1 := shift(mean_rh, type = "lag")] %>%
    .[, mean_rh_lag2 := shift(mean_rh, n = 2, type = "lag")]

print(rh_avg)


# %% Plot Climate Data --------------------------------------------------------

# TEMPERATURE

ggplot(temps,
       aes(x = t_daily_avg_f,
           y = factor(weekint),
           fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c()

# RELATIVE HUMIDITY

ggplot(rh,
       aes(x = rh_daily_avg,
           y = factor(weekint),
           fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c()


# %% Merge Data and Write -----------------------------------------------------

nrow(temps_avg)
names(temps_avg)
sapply(temps_avg, class)

nrow(rh_avg)
names(rh_avg)
sapply(rh_avg, class)


cbind(names(temps_avg), names(rh_avg))


dropfrotemp <- c("sd_tempf", "n")
dropfrorh <- c("sd_rh", "n")

uscrn <- merge(temps_avg[!weekint %in% -1:0] %>% .[, (dropfrotemp) := NULL],
               rh_avg[!weekint %in% -1:0] %>% .[, (dropfrorh) := NULL],
               keyby = weekint)

print(uscrn)

# write
saveRDS(comp_crn, here::here("data", "cleaned", "uscrn-include-missing.Rds"))

saveRDS(list(temps = temps, relhumid = rh),
        here::here("data", "cleaned", "uscrn.Rds"))

fwrite(uscrn, here::here("data", "cleaned", "uscrn_sum.csv"))
