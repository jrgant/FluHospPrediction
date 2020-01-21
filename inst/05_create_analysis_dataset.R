# %% SETUP ---------------------------------------------------------------------

library(data.table)

# Data

clndir <- here::here("data", "cleaned")

emp <- fread(paste0(clndir, "/empdat.csv"))
crn <- fread(paste0(clndir, "/uscrn.csv"))
sim <- readRDS(paste0(clndir, "/hypothetical-curves.Rds"))

str(emp)
str(crn)
str(sim$outhc)
str(sim$train$trainset)

# %% MERGE DATA ---------------------------------------------------------------

names(emp)
names(crn)
names(sim$train$trainset)
