# %% Delete Unneeded USCRN Station Data --------------------------------------

# @NOTE
# This section was run only once, after initial download of the USCRN data.

datadir <- here::here("data", "uscrn")
years <- 2003:2019

# List all files originally downloaded from FTP
manifest <- lapply(years, function(x) {
  list.files(paste(datadir, x, sep = "/"))
  }) %>%
  unlist

print(manifest)

# Save original file manifest
writeLines(
  manifest,
  paste(datadir, "original_file_manifest.txt", sep = "/")
)

files <- data.table(filename = manifest)
files

files[, state := str_extract(filename, "(?<=20[0-9]{2}\\-)[A-Z]{2}")]
files[, year := as.numeric(str_extract(filename, "(?<=\\-)20[0-9]{2}(?=\\-)"))]
files[, keep := ifelse(state %in% all_states, TRUE, FALSE)]

files

freq(files[, .(state, keep)]) %>% print
freq(files[keep == TRUE, state]) %>% print

# Record files to keep and drop
drop <- files[keep == F]
kept <- files[keep == T]

table(drop$keep)
table(kept$keep)

for (i in 1:length(years)) {
  currdrops <- drop[year == years[i], filename]
  file.remove(paste(datadir, years[i], currdrops, sep = "/"))
}
