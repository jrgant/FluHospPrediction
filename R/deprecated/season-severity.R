# %% CDC Flu Season Severity -------------------------------------------------

# @CITATION:

# Biggerstaff M et al. Systematic Assessment of Multiple Routine and Near
# Real-Time Indicators to Classify the Severity of Influenza Seasons and
# Pandemics in the United States, 2003-2004 Through 2015-2016. Am J Epidemiol
# 2018;187:1040–50. doi:10.1093/aje/kwx334.

# @NOTE:
# If simply interested in viewing cleaned data, skip to 'Set Global Options'

library(data.table)

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
