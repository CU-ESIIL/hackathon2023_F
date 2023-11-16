
library(tidyverse)
library(sf)
library(vegan)

# Set working direction
setwd("/home/jovyan/data-store/hackathon2023_F")

# Specify the path to the folder containing your XML files
gbif_path <- "./Data/plants_in_fire_region.shp"
fire_path <- "./Data/Rim_fire.shp"

gbif <- st_read(gbif_path)
fire <- st_read(fire_path) %>% st_transform(st_crs(df))

head(df)


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

# Plot results 
plt1 <- ggplot(df, aes(x = year, y = shannon_div)) +
  geom_line() + 
  xlab("Year") +
  ylab("Shannon Diversity Index")
ggsave(plt1, "./Figures/GBIF_ShannonIndexThroughTime.png")


# Tried making a ggplot and failed. If someone wants to improve, please do! 
plot(fire$geometry) 
plot(gbif$geometry, add=T, col="red")
png("StudyArea_WithGBIF.png")

# Save outputs 
write.csv(df, "./Data/GBIF_DivByYear_RimFire.csv")
