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


lines <- osm_roads$osm_lines #%>%
#  st_transform(crs = "EPSG:4269")


fire_poly <- fireRegion %>%
  st_transform(crs = st_crs(osm_roads$osm_lines))

tm_shape(osm_roads$osm_lines) +
  tm_lines(col="black", alpha=0.5) +
tm_shape(fire_poly$geometry) + 
  tm_lines(col="blue") 

