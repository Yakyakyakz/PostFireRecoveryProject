
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


plot(r_fire)


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



# Calculating GLOBAL (fire-wide) time series  -----------------------------
mean_ts <- terra::global(r_fire, fun= "mean",  na.rm = TRUE)#98 ##i think this is wrong - nees to be the dropped version. 
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




# Step 4.1 Calculating spectral recovery BY fire severity class -----------

#need the classified fire severity raster from STEP3 script (or pull from above)  
r_class_test # need to run Step 3 script before this will work; this is the export of the CLASSIFIED GEE fire export. 
#this is my fire classified fire severity data; so that I have something to do zonal stats on. 
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


  

#standard deviation investigation 
sev_zonal_tidy_sd <- sev_zonal_sd%>%
  pivot_longer(cols = -category, names_to = "year", values_to = "sd")%>%
  mutate(year = str_replace(year, "yr", ""))

fire_sev_ts <- sev_zonal_tidy_mn%>%
  left_join(sev_zonal_tidy_sd)


#figure for just the mean 
ggplot(fire_sev_ts, aes(x = year, y = mean, group=category, color = category))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste0("Spectral Recovery"," ", fire_ID)) +
  ylab("NBR")
  
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




##calculate the 80P NBR value 
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
  pre_avg <- (NBR_y2+NBR_y1)/2
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



# Step 4.3 Calculate recoverymetrics for each fire severity class.  -------
fire_sev_ts # so this is my time series that will be used to calculate metrics. 



#loop 
metric_list <- list() #empty list 
for (i in fire_sev_ts$category){
  name <- i
  subset <- fire_sev_ts%>%
    filter(category == i)
  results <-recovery_metric(subset, yof, fire_ID)
  metric_list[[i]] <- results
}
print(metric_list)

metric_df <- do.call(rbind, metric_list) #merges the list to a dataframe of all the metrics. 


# Sandbox: playing with the clipped, names exports of GEE  ----------------

landtrendr_files <- list.files(here("data", "gee_export_landtrd"), ignore.case = TRUE, full.names = TRUE)
test<- rast(landtrendr_files[3]) #hypothetically I would export each fire like this. 
plot(test)
fires_shape <- read_sf(here("data", "bnp_perim_selected", "fires_1984_50ha.shp")) #

###
#for loop 
#1a - list files in SEVERITY and TIME SERIES 
#1b - Input SHAPE ; extract ID and Yof 
#2 - Pull in the SEVERITY; CLASSIFY 
#3 - Pull in TS; 
#3a - GLOBAL 

#3b - ZONAL using SEVERITY  
#Build a list of 



# Old code ----------------------------------------------------------------
fire_sec_ts_sel <- fire_sev_ts%>%
  filter(category == "High Severity")%>%
  dplyr::select(-category)

recovery_metric(fire_sec_ts_sel, yof, fire_ID)
#selectign the shape of the fire
fire_shape <- fires_shape%>%
  dplyr::filter(Fire_Numbe == !!fire_ID) 
fire_vect<- vect(fire_shape)#converting sf object to a spatvector so it can be used in TERRA
fire_vect <- terra::project(fire_vect, r) #projecting int he same crs as the raster so that the clip will function 


## Calculate average BNR over 5 years post fire 
get_avg_NBR <- function(data, yof){
  avg_NBR <- data %>%
    filter(year > as.numeric(yof))%>%
    slice_head(n=5)%>%
    summarize(avg = mean(mean))
  return(avg_NBR)
}
get_avg_NBR(fire_ts, yof) #confirmed worked!




# COde April 16 - before I modified to be better.  ----------------------------------------------------------------

###Code before I fiddles to add additonal metrics######################################
YearsToRecovery Function ------------------------------------------------
  #function to return the number of year at which the .80 of the pre (2year avg)  was met. 
  year_recovery_80p <- function(data, yof){
    p80_NBR_val <- pre_NBR_2yavg(data, yof)%>%
      "*"(.8) 
    years_till_80p<-get_year(data, as.numeric(p80_NBR_val), yof)%>%
      "-"(yof)
    year_80p <- years_till_80p +yof
    return(year_80p)
  }

year_recovery_80p(fire_ts, yof)

#function to return the number of year at which the .50 of the pre (2year avg)  was met. 
year_recovery_50p <- function(data, yof){
  p50_NBR_val <- pre_NBR_2yavg(data, yof)%>%
    "*"(.5) 
  years_till_50p<-get_year(data, as.numeric(p50_NBR_val), yof)%>%
    "-"(yof)
  year_50p <- years_till_50p +yof
  return(year_50p)
}

year_recovery_50p(fire_ts, yof)

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
  max<- max(NBR_y4, NBR_y5)
  ARI <- max - NBR_y0
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
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)))
  YrYr5 = (NBR_y5 - NBR_y0)/5
  return(YrYr5)
}

YrYr5(fire_ts, yof)# works; this is a dNBR/year metric 

# Year on Year Average 10 ---------------------------------------------------
## YrYr5 = NBR5 - NBR0 /5
YrYr10 <- function(data, yof){
  NBR_y10 <- as.numeric(get_NBR(data, as.numeric(yof)+10))
  NBR_y0 <- as.numeric(get_NBR(data, as.numeric(yof)))
  YrYr10 = (NBR_y10 - NBR_y0)/10
  return(YrYr10)
}

YrYr10(fire_ts, yof)# works; this is a dNBR/year metric 

rm()

## onefunction 

recovery_metric <- function(data, yof, fire_ID){
  a <- R80P(data, yof)
  b<- RRI(data, yof)
  c<-YrYr5(data, yof)
  d<-YrYr10(data, yof)
  e<- year_recovery_80p(data, yof)
  f <- fire_ID
  results <- list(
    Fire_ID = f,
    R80P = a,
    RRI = b,
    YrYr5 = c,
    YrYr10 = d,
    Year_80p_recovery = e)
  return(results)
}


recovery_metric(fire_ts, yof, fire_ID)



# Step 4.3 Calculate recoverymetrics for each fire severity class.  -------
fire_sev_ts # so this is my time series that will be used to calculate metrics. 


fire_sec_ts_sel <- fire_sev_ts%>%
  filter(category == "High Severity")%>%
  dplyr::select(-category)

recovery_metric(fire_sec_ts_sel, yof, fire_ID)

data_list <- list()
for (i in fire_sev_ts$category){
  name <- i
  subset <- fire_sev_ts%>%
    filter(category == i)
  results <-recovery_metric(subset, yof, fire_ID)
  data_list[[i]] <- results
}
print(data_list)

df <- do.call(rbind, data_list)

dataframe <- as.data.frame(data_list)

class(fire_sev_ts)
class(yof)
class(fire_ID)

# Example usage
result <- recovery_metric_by_category(fire_sev_ts, yof, fire_ID)

results <- fire_sev_ts %>%
  group_by(category) %>%
  summarize(result = recovery_metric(., yof, fire_ID))

results <- fire_sev_ts %>%
  group_by(category) %>%
  summarize(result = recovery_metric(data = fire_sev_ts$category, yof = yof, fire_ID = fire_ID))


recovery_metric <- function(data, yof, fire_ID){
  tryCatch({
    # Calculate recovery metrics
    a <- R80P(data, yof)
    b <- RRI(data, yof)
    c <- YrYr5(data, yof)
    d <- YrYr10(data, yof)
    e <- year_recovery_80p(data, yof)
    
    # Handle NA or zero values
    a <- ifelse(is.na(a) | a == 0, NA, a)
    b <- ifelse(is.na(b) | b == 0, NA, b)
    c <- ifelse(is.na(c) | c == 0, NA, c)
    d <- ifelse(is.na(d) | d == 0, NA, d)
    e <- ifelse(is.na(e) | e == 0, NA, e)
    
    f <- fire_ID
    
    # Create dataframe with results
    results <- data.frame(
      Fire_ID = f,
      R80P = a,
      RRI = b,
      YrYr5 = c,
      YrYr10 = d,
      Year_80p_recovery = e
    )
    
    return(results)
  }, error = function(e) {
    message("Error occurred: ", conditionMessage(e))
    # You can add additional error handling logic here
    return(NULL)
  })
}


