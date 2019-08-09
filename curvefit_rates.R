# Hypothetical Curve Simulation

# Approach adapted from:

# Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling of
# Epidemics with an Empirical Bayes Framework. PLoS Comput Biol 2015;11:e1004382.
# doi:10.1371/journal.pcbi.1004382.


# Load packages -------------------------------------------------------------

pacman::p_load(glmgen, ggplot2, ggthemes, viridis, purrr, tidyr, data.table)

source("R/simcrv_funs.R")

# Load data -----------------------------------------------------------------

ed <- readRDS("empdat.Rds")
print(head(ed))

# convert all data frames to data.tables
sapply(ed, setDT)
sapply(names(ed), function(x) sapply(ed[[x]], class))

fct_to_char <- c("season", "severity", "sev2")
ed$whsp_rt[, (fct_to_char) := lapply(.SD, as.character), .SDcols = fct_to_char]
ed$whsp_rt <- ed$whsp_rt[agecat == "Overall", ]

# subset whsp_ct to deisred seasons and epiweeks
# drop pandemic flu and seasons with missing severity data
drop_seas <- c("2009-10", "2018-19")
ed$cdc_svr <- ed$cdc_svr[!season %in% drop_seas]

ew_order <- c(40:53, 1:17)
ed$whsp_rt <- ed$whsp_rt[!season %in% drop_seas & mmwr_week %in% ew_order]

# create a week variable that matches epiweek to integers
# trandfilter() does not take factors
ed$whsp_rt[, week := c(1:31)[match(mmwr_week, ew_order)]]

# view variable classes in both datasets
sapply(ed, function(x) sapply(x, class))
print(ed)
