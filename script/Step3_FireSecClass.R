##Step 3: Take a GEE exported fire severity tiff classify 
# TO DO 
#1 Classifier function for CBI 
#2 Create a function to give fire severity descrptive stats. 
#2 Figure out how to color the plots to a suitable pallete. Might be easier to do in built in GG plot. 


# library -----------------------------------------------------------------
library(terra)
library(tidyverse)
library(here)
library(terra)
library(sf)
library(dplyr)

# input -------------------------------------------------------------------

metric <- "dNBR.tif" #mchoose metric 
tiff_files <- list.files(here("data", "gee_export_sev"), pattern = metric, ignore.case = TRUE, full.names = TRUE)
raster_test <- rast(tiff_files[2])
plot(raster_test)

# Define colors for each category
colors <- c("#1a9641",   # Enhanced Growth - high (green)
            "#a6d96a",   # Enhanced Growth - low (light green)
            "#ffffbf",   # Unburned (yellow)
            "#fdae61",   # Low Severity (orange)
            "#d7191c",   # Moderate-Low Severity (red)
            "#b2182b",   # Moderate-High Severity (dark red)
            "#67001f")   # High Severity (maroon)



# functions dNBR ---------------------------------------------------------------
##funtion that classifies and catagorizes a raster based on dNBR values  
dNBR_classify <- function (raster) {
  rcl_dNBR <- matrix(c(-1500,-250,1,
                       -250,-100,2,
                       -100,100,3,
                       100,270,4,
                       270,440,5,
                       440,660,6,
                       660,2000,7), byrow = T, ncol = 3)
  raster_class <- terra::classify(raster, rcl_dNBR, right = T)
  dNBR_levels <- data.frame(ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
                            category = c("Enhanced Growth - high",
                                         "Enhanced Growth - low",
                                         "Unburned",
                                         "Low Severity",
                                         "Moderate-Low Severity",
                                         "Moderate-High Severity",
                                         "High Severity"))
  
  levels(raster_class) <- dNBR_levels #assigning catagories to the levels. 
  return(raster_class)
}

r_class_test <- dNBR_classify(raster_test)
plot(r_class_test )#would need to change the pallete before exporting as is. 


##function to provide descriptive stats on fire severity classes and relative area within the burn perimeter. 
descriptive_sev_stats <- function(raster){
  classify_rast <- dNBR_classify(raster)
  mean <- terra::zonal(raster, classify_rast, "mean")
  sd <- terra::zonal(raster, classify_rast, "sd")
  median <- terra::zonal(raster, classify_rast, "median")
  count <- freq(classify_rast)%>%rename(category=value)
  merge <- left_join(mean, median, by = "category")%>%
    left_join(., sd, by = "category")%>%
    left_join(., count, by = "category")%>%
    rename(mean = 2, median =3, sd = 4, px_count = 6)%>%
    mutate(percentage = (px_count/ sum(px_count)*100))%>%
    dplyr::select(!(layer))
  return(merge)
}


test_stat_table <- descriptive_sev_stats(raster_test)

##could merge these to export both the plot and a table as a figure. 
##could add in landscapemetrics 


# function CBI  -----------------------------------------------------------


# process -----------------------------------------------------------------
#dNBR test 
dNBR_classified <- dNBR_classify(rast(tiff_files[1]))
plot(dNBR_classified) 

 

# Step 4 Zonal Stats usinf severity; on the landtrendr Data m ------------------------------------------------------





# old code ----------------------------------------------------------------
raster <- rast(tiff_files[[2]])
raster_test <- rast(tiff_files[[1]])
plot(raster)


rcl_dNBR <- matrix(c(-1500,-250,1,
                     -250,-100,2,
                     -100,100,3,
                     100,270,4,
                     270,440,5,
                     440,660,6,
                     660,2000,7), byrow = T, ncol = 3)

raster_class <- terra::classify(raster, rcl_dNBR, right = T)
plot(raster_class)#if you plot this using terra; the scale will be wrong and it wont look right. 
dNBR_levels <- data.frame(ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
                          category = c("Enhanced Growth - high",
                                       "Enhanced Growth - low",
                                       "Unburned",
                                       "Low Severity",
                                       "Moderate-Low Severity",
                                       "Moderate-High Severity",
                                       "High Severity"))

levels(raster_class) <- dNBR_levels #assigning catagories to the levels. 
plot(raster_class)

dNBR_2003_class_crop <- crop(dNBR_2003_class, aoi_v, mask = T)
plot(aoi_v)
plot(dNBR_2003_class_crop)

#descriptive stats base code
mean <- terra::zonal(rast(tiff_files[1]), dNBR_classified, "mean")
sd <- terra::zonal(rast(tiff_files[1]), dNBR_classified, "sd")
median <- terra::zonal(rast(tiff_files[1]), dNBR_classified, "median")
count <- freq(dNBR_classified)%>%rename(category=value)

test2 <- left_join(mean, median, by = "category")%>%
  left_join(., sd, by = "category")%>%
  left_join(., count, by = "category")%>%
  rename(mean = 2, median =3, sd = 4, count = 6)%>%
  mutate(percentage = (count/ sum(count)*100))%>%
  dplyr::select(-layer)

