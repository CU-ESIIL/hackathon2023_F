
library(tidyverse)
library(sf)

# Set working directory
# setwd("/home/jovyan/data-store/hackathon2023_F")

# Specify the path to the folder containing your XML files
folder_path <- "./Data/GBIF_cameron_peak_bbox_RAW_2015-2023.csv"  
fire_path <- "./Data/cameron_peak_2020.shp"



df <- read.csv(folder_path, sep = "\t")
fire <- st_read(fire_path)

# Clean occurrence data 
df_new <- df %>% 
  select(scientificName, 
         decimalLatitude, 
         decimalLongitude, 
         year, 
         month, 
         day, 
         kingdom) %>% 
  mutate(decimalLatitude = as.numeric(decimalLatitude), 
         decimalLongitude = as.numeric(decimalLongitude)) %>% 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))

# Take a sample to reduce processing time 
# df_sample <- df_new[sample(1:length(df_new$scientificName), size = 10000, replace = FALSE),]

# Convert to spatial 
df_sf <- st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% st_transform(4269)

# Run spatial intersection (boolean column) 
df_sf$fire <- ifelse(st_intersects(df_sf, fire), 1, 0)

# Remove non-fire points
df_sf_fire <- df_sf %>% filter(fire == 1)

# Save output 
st_write(df_sf_fire, "./Data/plants_in_fire_region_cameron_peak.shp")

## Summary statistics 

# Percent of sample data inside fire boundary 
sum(df_sf$fire, na.rm=TRUE)/length(df_sf$fire)

# Plot results 
plot(df_sf_fire$geometry, col="red")
plot(fire$geometry, add=TRUE)

# Calculating area of fire polygon (Sanity check) 
st_area(fire$geometry) # square meters
st_area(fire$geometry)/1000000 # square km
st_area(fire$geometry)/1000000 * 0.386 # square miles
t_area(fire$geometry)/1000000 * 0.386 * 640 #acres 
