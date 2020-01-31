# %% SETUP ---------------------------------------------------------------------

suppressMessages(library(FluHospPrediction))

# Data

clndir <- here::here("data", "cleaned")

emp <- fread(paste0(clndir, "/empdat_weeksum.csv"))
crn <- fread(paste0(clndir, "/uscrn.csv"))
sim <- readRDS(paste0(clndir, "/hypothetical-curves.Rds"))

str(emp)
str(crn)

str(sim$outhc)
str(sim$train$trainset)
str(sim$test$testset)


# %% CHECK WEEKINT RANGES ---------------------------------------------------

cbind(unique(emp$weekint),
      unique(crn$weekint),
      unique(sim$train$trainset$weekint),
      unique(sim$test$testset$weekint))


# %% MERGE DATA ---------------------------------------------------------------

dat <- list(train = sim$train$trainset[emp, on = "weekint"][order(cid, weekint)],
            test = sim$test$testset[emp, on = "weekint"][order(cid, weekint)])

saveRDS(dat, file = here::here("data", "cleaned", "analytic_datasets.Rds"))
