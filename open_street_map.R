install.packages('rgdal', type = "source", configure.args=c('--with-proj-include=/opt/conda/envs/earth-analytics-python/include','--with-proj-lib=/opt/conda/envs/earth-analytics-python/lib'))
Sys.setenv("PROJ_LIB" = "/opt/conda/envs/earth-analytics-python/share/proj")
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
library(dplyr)
gdalcubes_options(parallel = 8)

### Initial changes
setwd("/home/jovyan/data-store/hackathon2023_F")

# FOR RIM FIRE
gbif_path <- "./Data/plants_in_fire_region.shp"
fire_path <- "./Data/Rim_fire.shp"
fire_name <- "Rim Fire"

# FOR CANYON PEAK FIRE
#gbif_path <- "./Data/plants_in_fire_region_cameron_peak.shp"
#fire_path <- "./Data/cameron_peak_2020.shp"
#fire_name <- "Canyon Peak Fire"

### Read fire regions file
fireRegion <- st_read(fire_path)

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


lines <- osm_roads$osm_lines #%>%
#  st_transform(crs = "EPSG:4269")


fire_poly <- fireRegion %>%
  st_transform(crs = st_crs(osm_roads$osm_lines))

tm_shape(osm_roads$osm_lines) +
  tm_lines(col="black", alpha=0.5) +
tm_shape(fire_poly$geometry) + 
  tm_lines(col="blue") 

# Zoomed in
bbox <- st_bbox(fireRegion)
names(bbox) <- c("left", "bottom", "right", "top")

# Get osmdata in the smaller region
osm_roads <- opq(bbox) %>% 
  add_osm_feature(key="highway") %>% 
  osmdata_sf() #%>%
#trim_osmdata(bbox)


osm_road_lines <- osm_roads$osm_lines %>%
  st_transform(crs = st_crs(fireRegion))

fire_poly <- fireRegion

### Incorporate GBIF data
# Load in data
gbif <- st_read(gbif_path) %>% st_transform(st_crs(fireRegion))

# Exploring 
table(lines$trail_visibility)
table(lines$foot)
table(lines$track)

trails_in_name <- lapply("Trail", grepl, lines$name)[[1]]

trail_indicator <- trails_in_name | 
  osm_road_lines$hiking == "yes" |
  osm_road_lines$foot == "yes" |
  osm_road_lines$foot == "designated" |
  osm_road_lines$foot == "official" |
  osm_road_lines$foot == "permissive" |
  !is.na(osm_road_lines$trail_visibility) |
  !is.na(osm_road_lines$track)

trail_indicator[is.na(trail_indicator)] <- FALSE
# Number of trails roughly
sum(trail_indicator)

tm_shape(osm_road_lines[!trail_indicator,]) +
  tm_lines(col="black") +
  tm_shape(osm_road_lines[trail_indicator,]) +
  tm_lines(col="darkgreen", alpha=0.5, lty="dotted") +
  tm_shape(fire_poly$geometry) + 
  tm_lines(col="blue") +
  tm_shape(gbif$geometry) +
  tm_dots(fill="red",alpha=0.15, size=.35)

