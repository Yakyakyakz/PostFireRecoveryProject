
# intent ------------------------------------------------------------------
#import the landtrendr data 
#clip to fires (if not lready) 
##calculate metrics based on each pixel. 
##visualise as a graph (whole fire)
#xoal stats based on fire severity 
#visualise based on fire severity 

# libraries ---------------------------------------------------------------
library(terra)
library(here)
library(tidyverse)
library(raster)
library(sf)
library(ggplot2)

# input -------------------------------------------------------------------

landtrendr_files <- list.files(here("data", "gee_export_landtrd"), pattern = ".tif$", ignore.case = TRUE, full.names = TRUE)

r <- rast(landtrendr_files[1]) #hypothetically I would export each fire like this. 
fires_shape <- read_sf(here("data", "bnp_perim_selected", "fires_1984_50ha.shp")) #this pulls from a output of step 1 DO NOT change


# variables ---------------------------------------------------------------

#extrating names from files 
fire_ID <- as.character(regmatches(landtrendr_files , regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", landtrendr_files )))# takes a filepath and finds the firenumber 
yof <- as.numeric(regmatches(landtrendr_files , regexpr("[0-9]{4}", landtrendr_files ))) ##make this more specific




# funtions ----------------------------------------------------------------
#dNBR 
calc_dNBR <- function (r , yof) {
  dNBR<- r[as.character(yof-1)] - r[as.character(yof+1)]
  names(dNBR) <- "dNBR" #rename the layer
  return(dNBR)
}
dNBR_2003 <- calc_dNBR(r, yof)

##dNBR with 2 years pre fire average. 
calc_dNBR_2y <- function (r , yof) {
  dNBR_2y<- mean(r[as.character(yof-1)],r[as.character(yof-2)]) - r[as.character(yof+1)]
  names(dNBR_2y) <- "dNBR_2y" #rename the layer
  return(dNBR_2y)
}
dNBR_2003_2y <- calc_dNBR_2y(r, yof)#yof can be replaced with numeric 

###Placeholder for all other metrics 

################################################################################

#cselectign the shape of the fire
fire_shape <- fires_shape%>%
  dplyr::filter(Fire_Numbe == !!fire_ID) 
fire_vect<- vect(fire_shape)#converting sf object to a spatvector so it can be used in TERRA
fire_vect <- terra::project(fire_vect, r) #projecting int he same crs as the raster so that the clip will function 



# clipping timeseries to fire permiter  -----------------------------------
r_fire_ts <- crop(r, fire_vect, mask = T)


# calculating the global mean over the time series for the fire perim  ------------------------
mean_ts <- global(r_fire, fun= "mean",  na.rm = TRUE)#98
sd_ts <- global(r_fire, fun= "sd",  na.rm = TRUE)
#mean_ts <- rownames_to_column(mean_ts, var = "Year")



# creating the figure for entire fire perim   ----------------------------------------------------
mean_tidy <- mean_ts %>%
  rownames_to_column(var = "Year") %>%
  mutate(Year = gsub("yr", "", Year))

ggplot(mean_tidy, aes(x = Year, y = mean, group=1))+
  geom_point()+
  geom_line()+
  ggtitle(paste0("Spectral Recovery"," ", fire_ID)) +
  ylab("NBR")


#


#calculate the average dNBR for the whole fire versis the bounding box 
aoi_v_rp <- terra::project(aoi_v, dNBR_2003)#projects the first in the ref of the second. 
average_fire <- zonal(dNBR_2003, aoi_v_rp, fun= "mean",  na.rm = TRUE)#192
average_whole <- global(dNBR_2003, fun= "mean",  na.rm = TRUE)#98

