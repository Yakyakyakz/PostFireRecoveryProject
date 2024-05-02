# library -----------------------------------------------------------------
library(terra)
library(tidyverse)
library(here)
library(terra)
library(sf)
library(dplyr)
library(landscapemetrics)




# input -------------------------------------------------------------------

##fire severity 

metric <- "dnbr" #can eventually chnage this. 
sev_files <- list.files(here("data", "gee_export_sev"), pattern = metric, ignore.case = TRUE, full.names = TRUE)
raster_test <- rast(sev_files[1])
plot(raster_test)

##fire peremiter
fires_shape <- read_sf(here("data", "bnp_perim_selected", "fires_1984_50ha.shp")) #this pulls from a output of step 1 DO NOT change

##time series 
landtrendr_files <- list.files(here("data", "gee_export_landtrd"), pattern = "timeseries",ignore.case = TRUE, full.names = TRUE)
r_fire <- rast(landtrendr_files[1]) #hypothetically I would export each fire like this. 

fire_ID <- as.character(regmatches(landtrendr_files , regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", landtrendr_files )))# takes a filepath and finds the firenumber 
yof <- as.numeric(regmatches(landtrendr_files , regexpr("[0-9]{4}", landtrendr_files ))) ##make this more specific

# functions ---------------------------------------------------------------

##classifies the dNBR values and re-colors them 
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
  colors <- c("#1a9641",   # Enhanced Growth - high (green)
              "#a6d96a",   # Enhanced Growth - low (light green)
              "#ffffbf",   # Unburned (yellow)
              "#fdae61",   # Low Severity (orange)
              "#d7191c",   # Moderate-Low Severity (red)
              "#b2182b",   # Moderate-High Severity (dark red)
              "#67001f")   # High Severity (maroon)
  coltb <- data.frame(value=1:7, col= colors)
  coltab(raster_class) <- coltb
  return(raster_class)
}
r_class_test <- dNBR_classify(raster_test)
plot(r_class_test )#would need to change the pallete before exporting as is. 


#extrating fire ID and yof from the landtrendr file name for linking with the fire shape file. 
fire_vect_function <- function(fires_shape, fire_ID, r) {
  fire_shape <- fires_shape %>%
    filter(Fire_Numbe == !!fire_ID) 
  fire_vect <- vect(fire_shape)
  fire_vect <- terra::project(fire_vect, r)
  return(fire_vect)
}


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
# process -----------------------------------------------------------------
sev_raster <- dNBR_classify(raster_test)
plot(sev_raster)




fire_vect <- fire_vect_function(fires_shape, fire_ID, r_fire) #this creates a vector of the fire in the right projection 

r_fire_ts <- crop(r_fire, fire_vect, mask = T) #this cops the fire raster to the fire vector 
# -------------------------------------------------------------------------

i = sev_files[1] #takes fromt he severity files 

sev_raster <- rast(i)#creates a raster 
fire_ID <- as.character(regmatches(i, regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", i )))# extracts the fire id which will be used. 
fire_vector <- fire_vect_function(fires_shape, fire_ID, sev_raster) ###extracts the correct fire and reprojects into sev raster 
plot(fire_vector)
fire_sev <- crop(sev_raster, fire_vector, mask = T)#cros the severity 
plot(fire_sev)
exp_descriptive <- descriptive_sev_stats(fire_sev) ##this needs to happen before the classification 
fire_sev_classified <- dNBR_classify(fire_sev) #classifies in order to do zonal stats 
plot(fire_sev_classified)
exp_landscapemetrics<- ###placeholder 

fire_sev_classified

##timereis 

ts_select <- landtrendr_files[grep(fire_ID, landtrendr_files)] #select only the ts for the fire above. 
ts_raster_bb <- rast(ts_select)
plot(ts_raster)
ts_raster <- crop(ts_raster_BB, fire_vector, mask = T) #this crops the fire raster to the fire vector 
plot(ts_raster) #masked 


# Calculating GLOBAL (fire-wide) time series  -----------------------------
mean_global_ts <- terra::global(ts_raster, fun= "mean",  na.rm = TRUE)#98
sd_global_ts <- terra::global(ts_raster, fun= "sd",  na.rm = TRUE)



# Adjust the extent of ts_raster to match fire_sev_classified
ts_raster_adjusted <- raster::resample(ts_raster, fire_sev_classified, method = "bilinear")

# Now, both raster objects should have the same extent
sev_zonal_mn <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "mean", na.rm = TRUE)
sev_zonal_sd <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "sd", na.rm = TRUE)

sev_zonal_tidy_mn <- sev_zonal_mn%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "mean")%>%
  mutate(year = str_replace(year, "yr", ""))










# creating the figure for entire fire   ----------------------------------------------------
mean_tidy <- mean_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))

sd_tidy <- sd_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))

fire_ts <- mean_tidy%>%
  left_join(sd_tidy)

ggplot(data = fire_ts, aes(x = year, group =1)) + 
  geom_line(aes(y = mean), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  xlab("Years") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.title = element_blank())

ggplot(mean_tidy, aes(x = year, y = mean, group=1))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste0("Spectral Recovery"," ", fire_ID)) +
  ylab("NBR")


