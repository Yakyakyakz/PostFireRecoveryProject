
# intent ------------------------------------------------------------------
#import the landtrendr data 
#clip to fires (if not lready) 
##calculate metrics based on each pixel. 
##visualise as a graph (whole fire)
#zonal stats based on fire severity 
#visualise based on fire severity (Step 4.1) 

# libraries ---------------------------------------------------------------
library(terra)
library(here)
library(tidyverse)
library(raster)
library(sf)
library(ggplot2)

# input -------------------------------------------------------------------

landtrendr_files <- list.files(here("data", "gee_export_landtrd"), ignore.case = TRUE, full.names = TRUE)
r_fire <- rast(landtrendr_files[1]) #hypothetically I would export each fire like this. 
fires_shape <- read_sf(here("data", "bnp_perim_selected", "fires_1984_50ha.shp")) #this pulls from a output of step 1 DO NOT change


#extrating fire ID and yof from the landtrendr file name for linking with the fire shape file. 
fire_ID <- as.character(regmatches(landtrendr_files , regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", landtrendr_files )))# takes a filepath and finds the firenumber 
yof <- as.numeric(regmatches(landtrendr_files , regexpr("[0-9]{4}", landtrendr_files ))) ##make this more specific

fire_vect_function <- function(fires_shape, fire_ID, r) {
  fire_shape <- fires_shape %>%
    filter(Fire_Numbe == !!fire_ID) 
  fire_vect <- vect(fire_shape)
  fire_vect <- terra::project(fire_vect, r)
  return(fire_vect)
}

fire_vect <- fire_vect_function(fires_shape, fire_ID, r_fire) #this creates a vector of the fire in the right projection 
r_fire_ts <- crop(r_fire, fire_vect, mask = T) #this cops the fire raster to the fire vector 

# funtions ----------------------------------------------------------------
#dNBR 
calc_dNBR <- function (r , yof) {
  dNBR<- r[as.character(yof-1)] - r[as.character(yof+1)]
  names(dNBR) <- "dNBR" #rename the layer
  return(dNBR)
}
dNBR_2003 <- calc_dNBR(r_fire, yof)

##dNBR with 2 years pre fire average. 
calc_dNBR_2y <- function (r , yof) {
  dNBR_2y<- mean(r[as.character(yof-1)],r[as.character(yof-2)]) - r[as.character(yof+1)]
  names(dNBR_2y) <- "dNBR_2y" #rename the layer
  return(dNBR_2y)
}
dNBR_2003_2y <- calc_dNBR_2y(r_fire, yof)#yof can be replaced with numeric 

###Placeholder for all other metrics 

################################################################################




# Calculating GLOBAL (fire-wide) time series  -----------------------------
mean_ts <- terra::global(r_fire, fun= "mean",  na.rm = TRUE)#98
sd_ts <- terra::global(r_fire, fun= "sd",  na.rm = TRUE)


# creating the figure for entire fire   ----------------------------------------------------
mean_tidy <- mean_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))

sd_tidy <- sd_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))

fire_ts <- mean_tidy%>%
  left_join(sd_tidy)

ggplot(data = fire_ts, aes(x = Year, group =1)) + 
  geom_line(aes(y = mean), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  xlab("Years") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(legend.title = element_blank())

ggplot(mean_tidy, aes(x = Year, y = mean, group=1))+
  geom_point()+
  geom_line()+
  ggtitle(paste0("Spectral Recovery"," ", fire_ID)) +
  ylab("NBR")




# Step 4.1 Calculating spectral recovery BY fire severity class -----------

#need the classified fire severity raster from STEP3 script (or pull from above)  
r_class_test # need to run Step 3 script before this will work; this is the export of the CLASSIFIED GEE fire export. 
plot(r_class_test)
r_fire_ts # is my raster stack that is clipped to the fire perim (code above). 
plot(r_fire_ts)

#the extents are close; but not exact, need to resample. 
# Adjust the extent of r_fire_ts to match r_class_test
r_fire_ts_adjusted <- raster::resample(r_fire_ts, r_class_test, method = "bilinear")

# Now, both raster objects should have the same extent
sev_zonal_mn <- zonal(r_fire_ts_adjusted, r_class_test, fun = "mean", na.rm = TRUE)
sev_zonal_sd <- zonal(r_fire_ts_adjusted, r_class_test, fun = "sd", na.rm = TRUE)

sev_zonal_tidy_mn <- sev_zonal_mn%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "mean")%>%
  mutate(year = str_replace(year, "yr", ""))


#figure for just the mean 
ggplot(df_join, aes(x = year, y = mean, group=category, color = category))+
  geom_point()+
  geom_line()+
  ggtitle(paste0("Spectral Recovery"," ", fire_ID)) +
  ylab("NBR")
  
#standard deviation investigation 
sev_zonal_tidy_sd <- sev_zonal_sd%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "sd")%>%
  mutate(year = str_replace(year, "yr", ""))

fire_sev_ts <- sev_zonal_tidy_mn%>%
  left_join(sev_zonal_tidy_sd)
  
#plotted ribbon 
ggplot(data = fire_sev_ts , aes(x = year, group = category)) + 
  geom_line(aes(y = mean, color = category), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = category), alpha = .2) +
  xlab("Years") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(legend.title = element_blank())

#plotted by category
df_selected <- fire_sev_ts %>%
  filter(category %in% c("Enhanced Growth - high"))
#"Moderate-High Severity","Moderate-Low Severity"
#"Enhanced Growth - low" , "Enhanced Growth - high"

ggplot(data = df_selected, aes(x = year, group = category)) + 
  geom_line(aes(y = mean, color = category), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = category), alpha = .2) +
  xlab("Years") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  ylim(-300, 750)+
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(legend.title = element_blank())


# Step 4.2 Calculate recovery metrics based on time series of the ENTIRE fire  --------
fire_ts #is the global timeseries calculated above. 

#create a function that calculated them all into a table
yof # is pulled fromt he file name above. 


## Calculate average BNR over 5 years post fire 
get_avg_NBR <- function(data, yof){
  avg_NBR <- data %>%
    filter(year > as.numeric(yof))%>%
    slice_head(n=5)%>%
    summarize(avg = mean(mean))
  return(avg_NBR)
}
get_avg_NBR(fire_ts, yof) #confirmed worked!

##calculate the 80P NBR value 
##function to get NBR value based on a YEAR
get_NBR <- function(data, input_year){
  output_NBR <- data %>%
    filter(year == input_year) %>%
    dplyr::select("mean")
  return(output_NBR)
}
get_NBR(fire_ts, 2014)#test it works 


############################COME BACK HERE APRIL 15####################
#function to get YEAR from mean ##need to troubleshoot this one - cause the same values might show up multiple times. 
get_year <- function(data, input_NBR){
  output_year <- data %>%
    filter(mean == input_NBR) %>%
    dplyr::select("year")
  return(output_year)
}


get_year(fire_ts, 393.7326)

## Calc NBR-pre (avg of the 2 years before disturbance)
pre_NBR_2yavg <- function(data, yof) {
  NBR_y1 <- as.numeric(get_NBR(data, as.numeric(yof)-1))
  NBR_y2 <- as.numeric(get_NBR(data, as.numeric(yof)-2))
  pre_avg <- (NBR_y2+NBR_y1)/2
  return(pre_avg)
}
pre_NBR_2yavg(fire_ts, yof) #works 

##converted to a function, input is DATA ##only uses 1 year pre 
years_recovery_80p <- function(data, yof){
  p80_NBR_val <- data %>%
    get_NBR(as.numeric(yof -1))%>%
    "*"(.8) 
  years_till_80p<-get_year(data, as.numeric(p80_NBR_val))%>%
    "-"(yof)
  return(years_till_80p)
}

years_recovery_80p(fire_ts, yof)








Years to Recovery (Y2R) 
Relative Recovery indicator (RRI) 
Ratio of 80% Recovery (R80P) 
Year on year average (YrYr) 

## Relative Recovery Index (RRI) = ARI /dNBR
#ARI is (max NBR year 4 or 5 post) - NBR y0. 

RRI <- function(data, yof){
  NBR_y4 <- as.numeric(get_NBR(data, as.numeric(yof)+4))
  NBR_y5 <- as.numeric(get_NBR(data, as.numeric(yof)+5))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)))
  max<- max(NBR_y4, NBR_y5)
  ARI <- max - NBR_y0
  pre2 <- pre_NBR_2yavg(data, yof)
  dNBR <- pre2 - NBR_y0
  RRI <- ARI/dNBR
  return(RRI)
}

RRI(fire_ts, yof) # works 





# Step 4.3 Calculate recoverymetrics for each fire severity class.  -------
fire_sev_ts # so this is my time series that will be used to calculate metrics. 



# Old code ----------------------------------------------------------------

#selectign the shape of the fire
fire_shape <- fires_shape%>%
  dplyr::filter(Fire_Numbe == !!fire_ID) 
fire_vect<- vect(fire_shape)#converting sf object to a spatvector so it can be used in TERRA
fire_vect <- terra::project(fire_vect, r) #projecting int he same crs as the raster so that the clip will function 
