library("rgdal") 
library("sf")
library("tidyverse")
library('spocc')
library("raster")
library("taxize")
library("data.table")
library("sp")
library("spdep")
library("osmdata")
library("lwgeom")
library("httr")

setwd("/home/jovyan/data-store/hackathon2023_F")

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

bbox <- st_bbox(fireRegionBuffered)


# Collect USGS water data monitoring sites IDs in the bounding box
startDate <- "01-01-2003"
endDate <- "12-31-2023"
bboxString <- paste(round(bbox, 5), collapse = "%2C") #space in link

# Just testing a different characteristic than ammonia
monitorURL <- paste0("https://www.waterqualitydata.us/Result/search?",
                      "bBox=", bboxString,
                      "&characteristic=Sediment",
                      "&startDateLo=", startDate,
                      "&endDateHi=", endDate,
                      "&mimeType=csv",
                      "&zip=yes")
# "&characteristicType=Biological%2C%20Algae",
# "&characteristicType=Biological%2C%20Plants",
# "&characteristicType=Nutrient",
# "&characteristicType=Population%2FCommunity",

# Terrible idea, sediment is 706,179 observations takes a long time to download
response <- GET(monitorURL)

# Also a terrible idea, measurements are categorial the plot will be non-sense
if (status_code(response) == 200) {
  data_file <- "Data/water_quality_data.zip"
  writeBin(content(response, "raw"), data_file)
  cat("Data downloaded successfully.\n")
  
  # Unzip the file and read the CSV
  temp_dir <- tempdir()
  unzip(data_file, exdir = temp_dir)
  csv_file <- list.files(temp_dir, pattern = "*.csv", full.names = TRUE)[1]
  water_quality_data <- read_csv(csv_file)
  
  
  ggplot(water_quality_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() +
    labs(title = "Sediment Over Time",
         x = "Date",
         y = "Sediment") +
    theme_minimal()
} else {
  cat(paste("Error:", status_code(response)), "\n")
}


# Next, looking at the FIRED data
urlFIRED <- "https://scholar.colorado.edu/downloads/zw12z650d" 
fired <- GET(urlFIRED) 
data_file <-"Data/fired.zip" 
writeBin(content(fired, "raw"), data_file)

# Unzip the file
temp_dir <- tempdir()
unzip(data_file, exdir = temp_dir)
shp_file <- list.files(temp_dir, pattern = "*.shp", full.names = TRUE)[1]

fired_shapes <- st_read(shp_file)

# Should really filter by year 2003-2023 first but...
boxRegion <- fireRegionBuffered %>% st_bbox %>% st_as_sfc
# .. can't determine overlap anyway yet since CRS is custom for fired polygons
overlap <- st_overlaps(fired_shapes, boxRegion)
