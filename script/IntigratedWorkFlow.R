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
#sev_files <- sev_files[1:2]##used to test code/shoten run time
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

##FUNCT:: classified a dNBR raters based on thresholds
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


#FUNCR:: function to select the correct fire from the shapefiles; then reproject to the raster; returns the CORRECT fire vectory 
fire_vect_function <- function(fires_shape, fire_ID, raster) {
  fire_shape <- fires_shape %>%
    filter(Fire_Numbe == !!fire_ID) 
  fire_vect <- vect(fire_shape)
  fire_vect <- terra::project(fire_vect, raster)
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
  fire_ID_full <- varnames(raster)
  fire_ID <- as.character(regmatches(fire_ID_full , regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", fire_ID_full )))
  merge$fire_ID <- fire_ID ##this is updated. 
  return(merge)
}



# 80p and 50P recovery  ---------------------------------------------------
##function to get NBR value based on a YEAR
get_NBR <- function(data, input_year){
  output_NBR <- data %>%
    filter(year == input_year) %>%
    dplyr::select("mean")
  return(output_NBR)
}
get_NBR(fire_ts, 2014)#test it works 



## Calc NBR-pre (avg of the 2 years before disturbance)
pre_NBR_2yavg <- function(data, yof) {
  NBR_y1 <- as.numeric(get_NBR(data, as.numeric(yof)-1))
  NBR_y2 <- as.numeric(get_NBR(data, as.numeric(yof)-2))
  pre_avg <- as.numeric((NBR_y2+NBR_y1)/2)
  return(pre_avg)
}
pre_NBR_2yavg(fire_ts, yof) #this is FUNKY...sometimes doesnt work 



#function to get a year that a values is exceeded AFTER the YOF
get_year <- function(data_ts, input_NBR, yof){
  get_year <- data_ts%>%
    filter(mean > input_NBR , year > yof)%>%
    slice(1)
  output_year <- as.numeric(pull(get_year, year))
  return(output_year)
}

val <- pre_NBR_2yavg(fire_ts, yof)
get_year(fire_ts, val, yof) #will only work if the post fire reaches that level. 

# YearsToRecovery Function ------------------------------------------------
#function to return the number of year at which the .80 of the pre (2year avg)  was met. 
year_recovery_80p <- function(data, yof){
  p80_NBR_val <- pre_NBR_2yavg(data, yof)%>%
    "*"(.8) 
  years_till_80p<-get_year(data, as.numeric(p80_NBR_val), yof)%>%
    "-"(yof)
  year_80p <- years_till_80p
  return(year_80p)
}

year_recovery_80p(fire_ts, yof)

#function to return the number of year at which the .50 of the pre (2year avg)  was met. 
year_recovery_50p <- function(data, yof){
  p50_NBR_val <- pre_NBR_2yavg(data, yof)%>%
    "*"(.5) 
  years_till_50p<-get_year(data, as.numeric(p50_NBR_val), yof)%>%
    "-"(yof)
  year_50p <- years_till_50p
  return(year_50p)
}

year_recovery_50p(fire_ts, yof)



# Ration of 80% Recovery  -------------------------------------------------
## R80P = Man (4 or 5) / NBR pre (avg)*.8 

R80P  <- function(data, yof){
  NBR_y4 <- as.numeric(get_NBR(data, as.numeric(yof)+4))
  NBR_y5 <- as.numeric(get_NBR(data, as.numeric(yof)+5))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)))
  max<- max(NBR_y4, NBR_y5)
  pre2 <- pre_NBR_2yavg(data, yof)*.8
  R80P <- max/pre2
  return(R80P)
}

test <- R80P(fire_ts, yof) # over 1 means recovered more than 80.  
class(test)

# RelativeRecoveryIndicator  ----------------------------------------------

#ARI is (max NBR year 4 or 5 post) - NBR y0. 

RRI <- function(data, yof){
  NBR_y4 <- as.numeric(get_NBR(data, as.numeric(yof)+4))
  NBR_y5 <- as.numeric(get_NBR(data, as.numeric(yof)+5))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)))
  NBR_y1 <- as.numeric(get_NBR(data, as.numeric(yof)+1))##added these to find the LOWEST value in the years surrounding fire 
  NBR_ym1 <- as.numeric(get_NBR(data, as.numeric(yof)-1))
  max<- max(NBR_y4, NBR_y5)
  min <- min(NBR_y0, NBR_y1, NBR_ym1)
  ARI <- max - min
  pre2 <- pre_NBR_2yavg(data, yof)
  dNBR <- pre2 - NBR_y0
  RRI <- ARI/dNBR
  return(RRI)
}

RRI(fire_ts, yof) # works; inturpretation may be difficult. 


# Year on Year Average 5 ---------------------------------------------------
## YrYr5 = NBR5 - NBR0 /5
YrYr5 <- function(data, yof){
  NBR_y5 <- as.numeric(get_NBR(data, as.numeric(yof)+5))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)+1))
  YrYr5 = (NBR_y5 - NBR_y0)/5
  return(YrYr5)
}

YrYr5(fire_ts, yof)# works; this is a dNBR/year metric 

# Year on Year Average 10 ---------------------------------------------------
## YrYr5 = NBR5 - NBR0 /5
YrYr10 <- function(data, yof){
  NBR_y10 <- as.numeric(get_NBR(data, as.numeric(yof)+10))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)+1))
  YrYr10 = (NBR_y10 - NBR_y0)/10
  return(YrYr10)
}

YrYr10(fire_ts, yof)# works; this is a dNBR/year metric 


## onefunction 

recovery_metric <- function(data, yof, fire_ID){
  a <- R80P(data, yof)
  b<- RRI(data, yof)
  c<-YrYr5(data, yof)
  d<-YrYr10(data, yof)
  g<- year_recovery_50p(data, yof)
  e<- year_recovery_80p(data, yof)
  f <- fire_ID
  results <- list(
    Fire_ID = f,
    R80P = a,
    RRI = b,
    YrYr5 = c,
    YrYr10 = d,
    Year_50p_recorvery = g,
    Year_80p_recovery = e)
  return(results)
}


recovery_metric(fire_ts, yof, fire_ID)



# process -----------------------------------------------------------------

#empty lists for the loops below. 
descriptive_list <- list()
landscapemetric_list <- list()
global_ts <- list()
zonal_ts <- list()

recovery_metrics <- list()


##non looped for fire severity. 

i = sev_files[1] #takes fromt he severity files 

sev_raster <- rast(i)#creates a raster 
fire_ID <- as.character(regmatches(i, regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", i )))# extracts the fire id which will be used. 
fire_vector <- fire_vect_function(fires_shape, fire_ID, sev_raster) ###extracts the correct fire and reprojects into sev raster 
plot(fire_vector)
fire_sev <- crop(sev_raster, fire_vector, mask = T)#cros the severity 
plot(fire_sev)
descriptive_list <- descriptive_sev_stats(fire_sev) ##this needs to happen before the classification 
fire_sev_classified <- dNBR_classify(fire_sev) #classifies in order to do zonal stats 
plot(fire_sev_classified)
exp_landscapemetrics<- ###placeholder 
lsm <- calculate_lsm(landscape =fire_sev_classified, what=c("lsm_l_contag", "lsm_l_ed","lsm_l_shdi" ,"lsm_c_ca", "lsm_c_pland"),progress = TRUE)
fire_sev_classified


### create above as a loop 
 for (i in sev_files){
   sev_raster <- rast(i)#creates a raster 
   fire_ID <- as.character(regmatches(i, regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", i )))# extracts the fire id which will be used. 
   fire_vector <- fire_vect_function(fires_shape, fire_ID, sev_raster) ###extracts the correct fire and reprojects into sev raster 
   plot(fire_vector)
   fire_sev <- crop(sev_raster, fire_vector, mask = T)#cros the severity 
   plot(fire_sev)
   descriptive_list[[i]] <- descriptive_sev_stats(fire_sev) ##this needs to happen before the classification 
   fire_sev_classified <- dNBR_classify(fire_sev) #classifies in order to do zonal stats 
   plot(fire_sev_classified)
   lsm <- calculate_lsm(landscape =fire_sev_classified, what=c("lsm_l_contag", "lsm_l_ed","lsm_l_shdi" ,"lsm_c_ca", "lsm_c_pland"),progress = TRUE)
   lsm$fire_ID <- fire_ID #make sure that the fire_id get assigned. 
   landscapemetric_list[[i]] <- lsm
 } 

descriptive_df <- do.call(rbind, descriptive_list)
lsm_df <- do.call(rbind, landscapemetric_list)
#likley can merge these for each fire; into one table that shows them all relative. 





##timeseries NON looped 
i = sev_files[1] #takes fromt he severity files 

sev_raster <- rast(i)#creates a raster 
fire_ID <- as.character(regmatches(i, regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", i )))# extracts the fire id which will be used. 
fire_vector <- fire_vect_function(fires_shape, fire_ID, sev_raster) ###extracts the correct fire and reprojects into sev raster 
plot(fire_vector)
fire_sev <- crop(sev_raster, fire_vector, mask = T)#cros the severity 
fire_sev_classified <- dNBR_classify(fire_sev) #classifies in order to do zonal stats 
ts_select <- landtrendr_files[grep(fire_ID, landtrendr_files)] #select only the ts for the fire above. 
ts_raster_bb <- rast(ts_select)
ts_raster <- crop(ts_raster_bb, fire_vector, mask = T) #this crops the fire raster to the fire vector 
plot(ts_raster) 
# Calculating GLOBAL (fire-wide) time series  -----------------------------
mean_global_ts <- terra::global(ts_raster, fun= "mean",  na.rm = TRUE)#98
sd_global_ts <- terra::global(ts_raster, fun= "sd",  na.rm = TRUE)

mean_tidy <- mean_global_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))%>%
  mutate( category ="Unclassified Fire Average")

sd_tidy <- sd_global_ts %>%
  rownames_to_column(var = "year") %>%
  mutate(year = gsub("yr", "", year))

fire_ts <- mean_tidy%>%
  left_join(sd_tidy)


# Calculating ZONAL (severity based) time series-------------------------
ts_raster_adjusted <- raster::resample(ts_raster, fire_sev_classified, method = "bilinear") # Adjust the extent of ts_raster to match fire_sev_classified
sev_zonal_mn <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "mean", na.rm = TRUE)
sev_zonal_sd <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "sd", na.rm = TRUE)


##tidy the zonal data;; shoudl i add the Average? 
sev_zonal_tidy_mn <- sev_zonal_mn%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "mean")%>%
  mutate(year = str_replace(year, "yr", ""))
sev_zonal_tidy_sd <- sev_zonal_sd%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "sd")%>%
  mutate(year = str_replace(year, "yr", ""))
fire_sev_ts_df <- sev_zonal_tidy_mn%>%
  left_join(sev_zonal_tidy_sd)%>%
  mutate(fire_ID = fire_ID) ##make sure this is in the for loop 


##above as a for loop #####

for (i in sev_files){
  metric_list <- list() #empty list INTERNAL to loop 
  sev_raster <- rast(i)#creates a raster 
  print("worked")
  fire_ID <- as.character(regmatches(i, regexpr("[0-9]{4}[A-Z]{2}[0-9]{3}", i )))# extracts the fire id which will be used. 
  fire_vector <- fire_vect_function(fires_shape, fire_ID, sev_raster) ###extracts the correct fire and reprojects into sev raster 
  plot(fire_vector)
  fire_sev <- crop(sev_raster, fire_vector, mask = T)#cros the severity 
  fire_sev_classified <- dNBR_classify(fire_sev) #classifies in order to do zonal stats 
  ts_select <- landtrendr_files[grep(fire_ID, landtrendr_files)] #select only the ts for the fire above. 
  ts_raster_bb <- rast(ts_select)
  ts_raster <- crop(ts_raster_bb, fire_vector, mask = T) #this crops the fire raster to the fire vector 
  # Calculating GLOBAL (fire-wide) time series  -----------------------------
  mean_global_ts <- terra::global(ts_raster, fun= "mean",  na.rm = TRUE)#98
  sd_global_ts <- terra::global(ts_raster, fun= "sd",  na.rm = TRUE)
  mean_tidy <- mean_global_ts %>%
    rownames_to_column(var = "year") %>%
    mutate(year = gsub("yr", "", year))%>%
    mutate( category ="Unclassified Fire Average")
  sd_tidy <- sd_global_ts %>%
    rownames_to_column(var = "year") %>%
    mutate(year = gsub("yr", "", year))
  fire_ts <- mean_tidy%>%
    left_join(sd_tidy)%>%
    mutate(fire_ID = fire_ID)
  global_ts[[i]] <- fire_ts
  # Calculating ZONAL (severity based) time series-------------------------
  ts_raster_adjusted <- raster::resample(ts_raster, fire_sev_classified, method = "bilinear") # Adjust the extent of ts_raster to match fire_sev_classified
  sev_zonal_mn <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "mean", na.rm = TRUE)
  sev_zonal_sd <- zonal(ts_raster_adjusted, fire_sev_classified, fun = "sd", na.rm = TRUE)
  ##tidy the zonal data;; shoudl i add the Average? 
  sev_zonal_tidy_mn <- sev_zonal_mn%>%
    pivot_longer(cols = -category, names_to = "year", values_to = "mean")%>%
    mutate(year = str_replace(year, "yr", ""))
  sev_zonal_tidy_sd <- sev_zonal_sd%>%
    pivot_longer(cols = -category, names_to = "year", values_to = "sd")%>%
    mutate(year = str_replace(year, "yr", ""))
  fire_sev_ts_df <- sev_zonal_tidy_mn%>%
    left_join(sev_zonal_tidy_sd)%>%
    mutate(fire_ID = fire_ID) ##make sure this is in the for loop 
  zonal_ts[[i]] <- fire_sev_ts_df
  yof <- as.numeric(regmatches(fire_ID , regexpr("[0-9]{4}", fire_ID ))) 
  for (j in fire_sev_ts_df$category){
    name <- j
    subset <- fire_sev_ts_df%>%
      filter(category == j)
    results <-recovery_metric(subset, yof, fire_ID)
    #print(paste("Storing results for", name))
    #print(results)  
    metric_list[[j]] <- results
  }
  metric_df <- do.call(rbind, metric_list)
  recovery_metrics[[i]] <- metric_df ##this will not work without looping each severity metric 
}


global_df <- do.call(rbind, global_ts)
zonal_df <- do.call(rbind, zonal_ts)
rmet_df <- do.call(rbind, recovery_metrics)


###next will be useful to add the recovery metrics into the loop above. 

##debug

for (i in fire_sev_ts_df$category){
  name <- i
  subset <- fire_sev_ts_df%>%
    filter(category == i)
  results <-recovery_metric(subset, yof, fire_ID)
  metric_list[[i]] <- results
}
metric_df <- do.call(rbind, metric_list)
recovery_metrics[[i]] <- metric_df
recovery_metrics


##carmine meeting - off the cusp 

zonal_df_s <-zonal_df%>%
  filter(fire_ID == "2014BA007")

ggplot(data = zonal_df_s, aes(x = year, group = category)) + 
  geom_line(aes(y = mean, color = category), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = category), alpha = .1) +
  xlab("Years") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  ylim(-300, 750)+
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(legend.title = element_blank())


##figure export 
ggplot(data = global_df, aes(x = year, group = fire_ID)) + 
  geom_line(aes(y = mean), size = 1) + 
  #geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = category), alpha = .08) +
  xlab("Years") + 
  ylab("Fitted NBR")
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.1,.6), legend.direction = "vertical") +
  theme(legend.title = element_blank())


##metrics 


###OLD CODE####
  
  ##this is the merge of the years to 80P recovery 
  ##chatGPT - merges all the functions. TEST. 
  years_recovery_80p_merge <- function(data, yof) {
    pre_NBR_2yavg <- function(data, yof) {
      NBR_y1 <- as.numeric(get_NBR(data, as.numeric(yof) - 1))
      NBR_y2 <- as.numeric(get_NBR(data, as.numeric(yof) - 2))
      pre_avg <- (NBR_y2 + NBR_y1) / 2
      return(pre_avg)
    }
    
    get_year <- function(data_ts, input_NBR, yof) {
      get_year <- data_ts %>%
        filter(mean > input_NBR , year > yof) %>%
        slice(1)
      output_year <- as.numeric(pull(get_year, year))
      return(output_year)
    }
    
    p80_NBR_val <- function(data, yof) {
      pre_avg <- pre_NBR_2yavg(data, yof)
      p80_NBR <- pre_avg * 0.8
      return(p80_NBR)
    }
    
    years_till_80p <- function(data, yof) {
      p80_NBR <- p80_NBR_val(data, yof)
      years_till_80p <- get_year(data, p80_NBR, yof) - yof
      return(years_till_80p)
    }
    
    year_80p <- function(data, yof) {
      years_till <- years_till_80p(data, yof)
      #year_80p <- years_till + yof
      return(years_till)
    }
    
    return(year_80p(data, yof))
  }
  
  years_recovery_80p_merge(fire_ts, yof) #2011 correct!

