# Setup ----------------------------------------------------------------------

# %%
hhc <- readRDS("data/hypothetical-curves.Rds")
names(hhc)

pacman::p_load(data.table, flumodelr, prophet)

setDT(hhc$outhc)
names(hhc$outhc)

hcsub <- hhc$outhc[week >= 1 & week <= 31]
print(hcsub)

# Serfling Model -------------------------------------------------------------

# %%
## flumodelr::flu_serf
flu_fit <- fluserf(fludta, outc = fludeaths, time = yrweek_dt, period = 52)
sapply(flu_fit, class)

# %%
ggplot(flu_fit %>% filter(year > 2008), aes(x = week_in_order)) +
  geom_line(aes(y = fludeaths), color = "red") +
  geom_line(aes(y = y0), color = "blue")


# Modified Serfling Model ----------------------------------------------------

## flumodelr::flum

# Prophet Model --------------------------------------------------------------

## prophet::prophet
