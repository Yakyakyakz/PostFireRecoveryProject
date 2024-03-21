
# library -----------------------------------------------------------------

library(here)
library(terra)
library(sf)
library(tidyverse)

# Inputs ------------------------------------------------------------------

fires <- read_sf(here("data", "bnp_perim_all", "fire_incl_islands.shp")) #this is the fire database with field domain PRESERVED 


# functions ---------------------------------------------------------------

clean_fire_data <- function(fire_db, year_1, area_ha) {
  s_fires <- fire_db %>%
    select(c(Fire_Numbe, Start_Date, Date, Fire_Name, Area_ha))%>%
    filter(Date >year_1, Area_ha >area_ha)
  return (s_fires)
}
# step1: import fire database, clean and select ---------------------------
s_fires <- clean_fire_data(fires, 1984, 50) #select from fire input, after 1984 and greater than 3000ha

write.csv(s_fires, file =here("data", "bnp_perim_selected", "fires_1984_50ha.csv"), row.names = FALSE)#export for import to GEE 
?write.csv
fire_list <- as.list(s_fires)
st_write(s_fires, here("data",  "bnp_perim_selected", "fires_1984_50ha.shp") )

##non-function code 
# s_fires <- fires %>%
#   select(c(Fire_Numbe, Start_Date, Date, Fire_Name, Area_ha))%>%
#   filter(Date >1984, Area_ha >50) ## starting with HIGH to speed up code 
# plot(s_fires[1])
# print(s_fires)


###
###
### below needs to be worked on. ### 
## Step 2: Clip fires to NBR stack 
path <- (here("data", "nbr_stack_bb"))
tiff_files <- list.files(path, pattern = ".tif", full.names = T)


nbr_1 <- rast(here("data", "nbr_stack_bb", "NBR_stack (1).tif"))
Nbr_2 <- rast(here("data", "nbr_stack_bb", "NBR_stack (2).tif"))

list <- list.files((here("data", "nbr_stack_bb")))

i <- list[1]
e_rast <- rast() ## need to make this the right resoltuioon, 

for (i in list){
  r <- rast(here("data", "nbr_stack_bb", i))
  merge <-merge(e_rast, r)
  return()
}


list <- as.list(nbr_1,nbr)

nbr_merge <- merge(nbr_1,Nbr_2)
r_list <- rast()

for (tif in tiff_files){
  r_list <- merge ()
}

#project the entire thing
# need to clip a single fire to the banff raster
#subset raster and the shape go into functions. 
#functions need the yof - easy - within the function; select the DATE column an dset is a YOF 
#then calculate a metric
#save it in the table
#move to the next fire. 

lcc <- rast(here("data", "nbr_stack_bb", "NBR_stack (2).tif"))

test <- subset(lcc, 19)
plot(test)

## for i in names() or row() its ina list so youll need to [[]]


