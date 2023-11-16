
library(tidyverse)
library(sf)

# Specify the path to the folder containing your XML files
folder_path <- "C:/Users/Kelly Kapsar/Downloads/0061004-231002084531237/occurrence.txt" # This is 9 gb, so difficult to maneuver 
fire_path <- "C:/Users/Kelly Kapsar/Downloads/Rim_fire.shp"

df <- read.table(folder_path, header = TRUE, sep = "\t", fill = TRUE)
fire <- st_read(fire_path)

# Clean occurrence data 
df_new <- df %>% 
  select(scientificName, 
         organismQuantity, 
         decimalLatitude, 
         decimalLongitude, 
         year, 
         month, 
         day, 
         kingdom) %>% 
  mutate(decimalLatitude = as.numeric(decimalLatitude), 
         decimalLongitude = as.numeric(decimalLongitude)) %>% 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>% 
  filter(decimalLatitude > 20, 
         decimalLatitude < 40, 
         decimalLongitude > -125, 
         decimalLongitude < -115)

# Take a sample to reduce processing time 
df_sample <- df_new[sample(1:length(df_new$scientificName), size = 10000, replace = FALSE),]

# Convert to spatial 
df_sf <- st_as_sf(df_sample, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% st_transform(4269)

# Run spatial intersection (boolean column) 
df_sf$fire <- ifelse(st_intersects(df_sf, fire), 1, 0)

# Percent of sample data inside fire boundary 
sum(df_sf$fire, na.rm=TRUE)/length(df_sf$fire)

# Plot results 
plot(df_sf$geometry, col="red")
plot(fire$geometry, add=TRUE)

# Calculating area of fire polygon (Sanity check) 
st_area(fire$geometry) # square meters
st_area(fire$geometry)/1000000 # square km
st_area(fire$geometry)/1000000 * 0.386 # square miles
t_area(fire$geometry)/1000000 * 0.386 * 640 #acres 
