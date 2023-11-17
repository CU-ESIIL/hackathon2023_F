################################################################################
# TITLE: Biodiversity Calculations for GBIF Occurrence Data 
#
# PURPOSE: This script takes GBIF occurrence data and reformats them to calculate
# biodiversity metrics by year within a polygon. 
#
# AUTHORS:Kelly Kapsar
# CREATED: 2023-11-16
# LAST UPDATED ON: 2023-11-16
# 
################################################################################

library(tidyverse)
library(sf)
library(vegan)

# Set working direction
setwd("/home/jovyan/data-store/hackathon2023_F")

# Specify the path to the folder containing your XML files
# Comment out rim/canyon depending on which you want to process

# FOR RIM FIRE
# gbif_path <- "./Data/plants_in_fire_region.shp"
# fire_path <- "./Data/Rim_fire.shp"
# fire_name <- "Rim Fire"

# FOR CANYON PEAK FIRE
gbif_path <- "./Data/plants_in_fire_region_cameron_peak.shp"
fire_path <- "./Data/cameron_peak_2020.shp"
fire_name <- "Canyon Peak Fire"


# Load in data
gbif <- st_read(gbif_path)
fire <- st_read(fire_path) %>% st_transform(st_crs(gbif))

# Reformat data into format needed by vegan package 
# According to this tutorial: https://rpubs.com/an-bui/vegan-cheat-sheet
# (Using year for site since we're calculating biodiversity by year)
occ <- gbif %>% 
  st_drop_geometry() %>% 
  filter(month %in% 4:9) %>% 
  select(species, year) %>% 
  group_by(year, species) %>% 
  summarize(present=n()) %>% 
  spread(key = species, value = present) %>% 
  replace(is.na(.), 0) %>% 
  arrange(., year)

# For a description of diversity indices see: https://rdrr.io/cran/vegan/man/diversity.html
df <- data.frame(year = occ$year, 
                 nobs = rowSums(occ[,2:ncol(occ)]), 
                 nspp = vegan::specnumber(occ), 
                 shannon_div = round(vegan::diversity(occ), 3))

# Plot results -----------------------------------------------------------

# Diversity index through time 
plt1 <- ggplot(df, aes(x = year, y = shannon_div)) +
  geom_line() + 
  xlab("Year") +
  ylab("Shannon Diversity Index")
ggsave(plt1, filename = paste0("./Figures/GBIF_ShannonIndexThroughTime_", fire_name, ".png"))

# Diversity index vs. number of obs 
plt1 <- ggplot(df, aes(x = nobs, y = shannon_div)) +
  geom_line() + 
  xlab("Number of Observations") +
  ylab("Shannon Diversity Index")
ggsave(plt1, filename = paste0("./Figures/GBIF_ShannonIndexByNobs_", fire_name, ".png"))

# Tried making a ggplot and failed. If someone wants to improve, please do! 
png(paste0("./Figures/StudyArea_WithGBIF_", fire_name, ".png"))
plot(fire$geometry) 
plot(gbif$geometry, add=T, col="red")
dev.off()

# Save outputs 
write.csv(df, paste0("./Data/GBIF_DivByYear_", fire_name ,".csv"))
