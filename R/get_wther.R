#setwd("C:/Github/weather")
setwd('C:\\Users\\kevin\\Documents\\sanofi-fluhosp')
library(tidyverse)
library(lubridate)
library(crn)

years <- 2001:2019

for (i in years) {
  downloadCRN(year=i)

  collateDaily()

  assign(paste0('df_y',i), 
         readr::read_delim(paste0('CRN_Daily_', 
                                 year(Sys.Date()), '-', 
                                 month(Sys.Date()), '-', 
                                 str_pad(day(Sys.Date()), 2, side='left', pad='0'), '.dat'), 
                                 ' ', col_names = colnamesDaily))
}

ld_key <-  readr::read_csv(paste0('CRN_Daily_', 
                                  year(Sys.Date()), '-', 
                                  month(Sys.Date()), '-', 
                                  str_pad(day(Sys.Date()), 2, side='left', pad='0'), '.inv'))

df_list <- ls()[str_detect(ls(), 'df_')]

df_all <- bind_rows(mget(df_list)) %>%
  inner_join(., ld_key, by='WBANNO')

df_simple <- df_all %>%
  select(WBANNO, Name, LST_DATE, T_DAILY_MAX, T_DAILY_MIN, RH_DAILY_AVE) %>%
  mutate(state = str_extract(Name, '^[A-Z][A-Z]'),
         day = ymd(LST_DATE),
         T_DAILY_MAX = as.numeric(T_DAILY_MAX),
         T_DAILY_MIN = as.numeric(T_DAILY_MIN),
         RH_DAILY_AVE = as.numeric(RH_DAILY_AVE)) %>%
  mutate_at(vars(T_DAILY_MAX, T_DAILY_MIN, RH_DAILY_AVE), 
            .funs = list(~na_if(., -9999))) %>%
  dplyr::filter(state %in% state.abb)

df_simple_2 <- df_simple %>%
  mutate(week = epiweek(day),
         year = year(day)) %>%
  group_by(state, year, week) %>%
  summarize(t_max = mean(T_DAILY_MAX, na.rm=T),
            t_min = mean(T_DAILY_MIN, na.rm=T),
            rh_ave = mean(RH_DAILY_AVE, na.rm=T))

write.csv(df_simple_2, file = 'tempdta.txt')


