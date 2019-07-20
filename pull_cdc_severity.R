# Scrape CDC Severity Designations ----------------------------------------------

# Method described in:
#
# Biggerstaff M, Kniss K, Jernigan DB, Brammer L, Bresee J, Garg S, et al. 
# Systematic Assessment of Multiple Routine and Near Real-Time Indicators to 
# Classify the Severity of Influenza Seasons and Pandemics in the United States, 
# 2003-2004 Through 2015-2016. Am J Epidemiol 2018;187:1040–50.
# doi:10.1093/aje/kwx334.

library(rvest)
url <- "https://www.cdc.gov/flu/about/classifies-flu-severity.htm"

cdcsv <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame(.)

names(cdcsv) <- tolower(names(cdcsv))

saveRDS(cdcsv, paste0("cdcsv_", Sys.Date(), ".Rds"))
