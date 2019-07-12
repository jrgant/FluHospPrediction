
# Packages ------------------------------------------------------------------

# install.packages("pacman")
# install.packages("devtools")
# devtools::install_github("kmcconeghy/flumodelr)
pacman::p_load(flumodelr, magrittr, stringr, viridis, ggthemes, broom, rvest,
               gridExtra)

# Scrape CDC Severity -------------------------------------------------------

# Method described in:
#
# Biggerstaff M, Kniss K, Jernigan DB, Brammer L, Bresee J, Garg S, et al. 
# Systematic Assessment of Multiple Routine and Near Real-Time Indicators to 
# Classify the Severity of Influenza Seasons and Pandemics in the United States, 
# 2003-2004 Through 2015-2016. Am J Epidemiol 2018;187:1040–50.
# doi:10.1093/aje/kwx334.

url <- "https://www.cdc.gov/flu/about/classifies-flu-severity.htm"

cdcsv <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame(.)

names(cdcsv) <- tolower(names(cdcsv))

saveRDS(cdcsv, paste0("cdcsv_", Sys.Date(), ".Rds"))

# Load Data -----------------------------------------------------------------

hsp_file <- "hospdat/Weekly_Data_Counts.csv"
hsp_names <- c("seas", "epiweek", "inf.a", "inf.b", "inf.ab", "inf.unk")

epiweek_levels <- paste(c(35:53, 1:17))
seas_levels <- paste(2003:2018, str_extract(2004:2019, "[0-9]{2}$"), sep = "-")

# wct = weekly hospitalization counts
wct <- 
  read_csv(hsp_file, skip = 2, 
           col_names = hsp_names) %>%
  mutate(
    epiweek  = factor(str_remove(epiweek, "[0-9]{4}\\-"), levels = epiweek_levels),
    seas     = factor(seas, levels = seas_levels),
    # match season with CDC overall severity classification
    severity = cdcsv$overall[match(seas, cdcsv$season)],
    sev2 = if_else(severity == "Low", "Low", "High/Moderate")
    ) %>%
  rowwise() %>%
  mutate(inf.tot = sum(inf.a, inf.b, inf.ab, na.rm = TRUE)) %>%
  ungroup() %>%
  # order factor levels
  mutate(severity = factor(severity, levels = c("Low", "Moderate", "High")),
         sev2 = factor(sev2, levels = c("Low", "High/Moderate")))

# check two-level severity variable
table(is.na(wct$severity))
table(is.na(wct$sev2))
with(wct, table(severity, sev2, exclude = NULL))


# View Hospitalization Curves -----------------------------------------------

# drop pandemic influenza year
# drop 2018-19 due to lack of severity designation
wct_p <- wct %>% filter(!seas %in% c("2009-10", "2018-19"))

# plot theme tweaks
theme_tweak <- 
  theme_clean(base_size = 15) +
  theme(axis.line = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title = element_text(face = "bold"),
                     axis.title.x = element_text(margin = margin(t = 10)),
                     axis.title.y = element_text(margin = margin(r = 10)),
                     legend.position = "right",
                     legend.background = element_blank())


seas_p <- wct_p %>%
  ggplot(aes(x = epiweek,
             y = inf.tot,
             group = seas,
             color = severity)) +
  geom_line(size = 0.5) +
  labs(x = "Epiweek", y = "Influenza hospitalizations (A and B)",
       title = "Hospitalization curves, 2003–2018") +
  scale_color_colorblind("Severity") +
  theme_tweak

seas_p

# Summarize peak weeks
epiweek_subset <- epiweek_levels[!epiweek_levels %in% 35:39]
peaks <- wct_p %>%
  group_by(seas) %>%
  filter(inf.tot == max(inf.tot)) %>%
  select(seas, 
         wk = epiweek,
         inf.tot,
         severity, 
         sev2) %>%
  mutate(wk2 = match(wk, ))

pkwk_p <- peaks %>%
  ggplot(aes(x = sev2, y = wk2)) +
  geom_point(size = 2, alpha = 0.5) +
  scale_y_continuous(labels = c(0, epiweek_subset[15], epiweek_subset[20], epiweek_subset[25])) +
  labs(x = "Severity", y = "Peak week (epiweek)") +
  theme_tweak

pkwk_p

pkcs_p <- peaks %>%
  ggplot(aes(x = sev2, y = inf.tot, label = seas)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(x = "Severity", y = "Peak cases (#)") +
  theme_tweak

pkcs_p


grid.arrange(seas_p, pkwk_p, pkcs_p,
             layout_matrix = matrix(c(1, 1, 2, 1, 1, 3), nrow = 2, byrow = T))

             