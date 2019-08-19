# CDC Flu Season Severity ----------------------------------------------------
#
# Method described in:
#
# Biggerstaff M et al. Systematic Assessment of Multiple Routine and Near
# Real-Time Indicators to Classify the Severity of Influenza Seasons and Pandemics
# in the United States, 2003-2004 Through 2015-2016. Am J Epidemiol
# 2018;187:1040–50. doi:10.1093/aje/kwx334.

pacman::p_load(rvest, stringr, readr, dplyr, data.table)

url <- "https://www.cdc.gov/flu/about/classifies-flu-severity.htm"

cdc_svr <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame()

names(cdc_svr) <- tolower(names(cdc_svr))

write.csv2(cdc_svr, file = "data/cdc_svr.csv")

# Empirical Hospitalization Counts ---------------------------------------------

cdc_svr <- read.csv2("data/cdc_svr.csv")

hspdat_fldr <- "data"
hsp_file <- paste0(hspdat_fldr, "/Weekly_Data_Counts.csv")
hsp_names <- c("seas", "epiweek", "inf.a", "inf.b", "inf.ab", "inf.unk")

# label epiweeks and seasons
epiweek_levels <- paste(c(40:53, 1:17))
epiweek_labels <- 1:31
seas_levels <- paste(2003:2018, str_extract(2004:2019, "[0-9]{2}$"), sep = "-")

# wct = weekly hospitalization counts
whsp_ct <-
  read_csv(hsp_file, skip = 2, col_names = hsp_names) %>%
  mutate(
    epiweek  = factor(
      as.numeric(str_remove(epiweek, "[0-9]{4}\\-")),
      levels = epiweek_levels,
      labels = epiweek_labels),
    seas     = factor(seas, levels = seas_levels),
    # match season with CDC overall severity classification
    severity = cdc_svr$overall[match(seas, cdc_svr$season)],
    sev2     = if_else(severity == "Low", "Low", "High/Moderate")
  ) %>%
  rowwise() %>%
  mutate(inf.tot = sum(inf.a, inf.b, inf.ab, na.rm = TRUE)) %>%
  ungroup() %>%
  # order factor levels
  mutate(severity = factor(severity, levels = c("Low", "Moderate", "High")),
         sev2     = factor(sev2, levels = c("Low", "High/Moderate")))

# check two-level severity variable
table(is.na(whsp_ct$severity))
table(is.na(whsp_ct$sev2))
with(whsp_ct, table(severity, sev2, exclude = NULL))

head(whsp_ct)


# Empirical Hospitalization Rates ----------------------------------------------

hsp_rates <- paste0(
  hspdat_fldr, "/FluSurveillance_EIP_Entire Network_Data.csv"
)

whsp_rt <-
  read_csv(hsp_rates, skip = 2) %>%
  select(-CATCHMENT, -NETWORK) %>%
  tidyr::drop_na(.) %>%
  setDT(.)

names(whsp_rt) <- c("season", "mmwr_year", "mmwr_week", "agecat",
                    "cumrates", "weekrate")

whsp_rt[, severity := fct_collapse(ovrall,
  `High/Moderate` = c("High", "Moderate"),
  Low = "Low"
)]

whsp_rt[, sev2 := names(match_sev2)[match_sev2 == "High"]]
whsp_rt[, .(
  season = factor(season, levels = seas_levels),
  mmwr_week = factor(
    mmwr_week,
    levels = epiweek_levels,
    labels = epiweek_labels
  )
)]
whsp_rt

# Save Empirical Data ----------------------------------------------------------

empdat <- list(
  cdc_svr = cdc_svr,
  whsp_ct = whsp_ct,
  whsp_rt = whsp_rt
)

saveRDS(empdat, "empdat.Rds")
