#########################
# Creating datacube with custom data
## In this code we will build a data cube using a custom time series data (Remotely sensed shannon diversity)

#install.packages(c("gdalcubes", "magrittr", "raster", "stars", "mapview", "viridis"))

library(gdalcubes)
library(dplyr)
library(cubelyr)
library(stars)


##############LCMAP cube #############
#############################################################################################
wd = "E:/Shannon_rim" # path to your data

setwd(wd) # setting up working environmnet

shan.files = list.files("E:/Shannon_rim", pattern = ".tif", recursive = TRUE, full.names = TRUE) # list the files those go to data cube

head(shan.files, 5) # information of the first 5 files

get_year = as.Date(substr(basename(shan.files), 3, 6), "%Y") # get the year information from the file name (reading the characters)
fname_all = basename(tools::file_path_sans_ext(shan.files)) # get the file's base name. This will help to build the bands for the datacube
my_collect = create_image_collection(LCMAP.files, date_time = get_year, band_names = "shannon")
my_collect


extent(my_collect, srs = "EPSG:4326") # image collection spatial reference

## create a cube with different spatial resolution and different space and time extent
lcmap.overview.30m = cube_view(srs = "EPSG:3857", extent = my_collect, dx = 100, dy = 100, dt = "P1Y", resampling = "average"
                               , aggregation = "median") # creating the cube view. Here we do not create the cube but shows the parameters we want to create the cube.

lcmap.overview.30m # will display the summary of the datacube

lcmap.cube.30m = raster_cube(my_collect, lcmap.overview.30m)
lcmap.cube.30m

lcmap.cube.30m.lcpri = select_bands(lcmap.cube.30m, c("shannon"))
# plot the data cube
dev.new(width=2, height=1)
plot(lcmap.cube.30m.lcpri)

# annimate the data cube
animate(lcmap.cube.30m)

# select the bands from the datacube
lcmap.cube.30m_var = reduce_time(lcmap.cube.30m.lcpri, "var(shannon)")

# plot the summary
dev.new(width=2, height=1)
plot(lcmap.cube.30m_var)

