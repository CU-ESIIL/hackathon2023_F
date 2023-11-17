library(sf)
library(gdalcubes)
library(rstac)
library(gdalUtils)
library(terra)
library(rgdal)
library(reshape2)
library(osmdata)
library(dplyr)
library(stars)
library(ggplot2)
library(colorspace)
library(geos)
library(ggthemes)
library(tidyr)
library(tmap)
gdalcubes_options(parallel = 8)

### Initial changes
setwd("/home/jovyan/data-store/hackathon2023_F")

### Read fire regions file
fireRegion <- st_read("Data/Rim_fire.shp")

# Get a 50 mile buffered polygon
metersPerMile <- 1609.34
bufferRadiusMiles <- 50
fireRegionBuffered <- fireRegion %>%
  st_buffer(metersPerMile*bufferRadiusMiles) 
print(fireRegionBuffered)
# and the bounding box to compare
st_bbox(fireRegion)
st_bbox(fireRegionBuffered)
# Calculate area ratio
st_area(fireRegion) / st_area(fireRegionBuffered %>% st_bbox %>% st_as_sfc)

# Rename bounding box
bbox <- st_bbox(fireRegionBuffered)
names(bbox) <- c("left", "bottom", "right", "top")

# Get osmdata 
osm_roads <- opq(bbox) %>% 
  add_osm_feature(key="highway") %>% 
  osmdata_sf() #%>%
  #trim_osmdata(bbox)

# These fail :( and its a problem with cyverse I can't fix
lines <- osm_roads$osm_lines %>%
  st_transform(crs = "EPSG:4269")
fire_poly <- fireRegion$geometry %>%
  st_transform(crs = "EPSG:4326")

agg <- c(fire_poly, osm_roads$osm_lines)

tm_shape(agg) + 
  tm_lines() +
  tm_polygons()
