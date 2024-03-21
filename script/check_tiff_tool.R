
# viewing GEE exported .tiff files  ---------------------------------------
library(raster)
library(terra)

filepath <- "C:/Users/Brian Yakiwchuk/Desktop/downloaded_from_drive_gis"
# List all TIFF files in the specified filepath
tiff_files <- list.files(filepath, pattern = "\\.tif$", full.names = TRUE)

#function
check_tiff <- function(i) {
  raster <- tiff_files[[i]]
  test <- rast(raster)
  plot(test, main = basename(raster))
}

check_tiff(3)

saved_rast <- rast(tiff_files[[3]])


##non function 
raster <- tiff_files[[2]]
test <- rast(raster)
plot(test, main = basename(raster))


