################################################################################
#
# Wind data: Extract data from netcdfs, allign with bird positions, and
#             calculate bird/wind data
# Based on 1-mgc_wind_extract-to-latlon_part2_Aug2021.R by Melinda. Rewritten
# for use in the Terra package becasue rgdal is no longer supported.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

# location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
# szn = "2021_2022"

locations = c("Bird_Island", "Midway")

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Loop to run thru all samples
for (location in locations) {
  if (location == "Bird_Island") {
    szns = c("2019_2020", "2020_2021", "2021_2022")
  } else if (location == "Midway") {
    szns = c("2018_2019", "2021_2022", "2022_2023")
  }
  for (szn in szns) {
    cat("Processing location:",location,"Season:",szn,"\n")

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/",szn,"/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/ERA5_SingleLevels_10m/",szn,"/")


# User Functions ----------------------------------------------------------

wrap360 <- function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Both bearings must be [0,360)
bearingAngle <- function(bird_bearing,wind_bearing) {
  LHturn <- wrap360(bird_bearing - wind_bearing)
  RHturn <- wrap360(wind_bearing - bird_bearing)
  return(pmin(LHturn,RHturn))
}

# Save compiled GPS data with wind U and V --------------------------------

setwd(wind_L1_dir)
wind_mat <- read.csv(paste0(szn,"_allbirds_GPS_with_wind.csv"))

# ADD BIRD BEHAVIOR -------------------------------------------------------

bird_list <- unique(wind_mat$id)

int_now <- 600 
hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)

for (i in 1:length(bird_list)) {
  
  # Isolate bird
  mi <- wind_mat %>% filter(id==bird_list[i])
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(mi$tripID)
  
  for (j in 1:length(trips)) {
    
    mij <- mi %>% filter(tripID==trips[j])
    
    if (nrow(mij) >= 6*2) { # If file is so short that it's less than 2 hours , don't do anything.
      
      mij$wind_vel <- NA # m/s
      mij$wind_dir <- NA # THE DIRECTION THE WIND IS COMING FROM
      mij$bird_dir <-NA # THE DIRECTION THE BIRD IS HEADING IN
      mij$bird_vel <-NA  # km/hr 
      mij$bwa <- NA # Bird-wind angle [0,180) degrees
      mij$w_rel <- NA # wind bearing relative to bird heading
      
      # Add bird speed and bearing ----------------------------------------------
      
      # Bird velocity and heading
      mij$lon <- Lon360to180(mij$lon)
      mij$bird_vel <- c(distHaversine(mij %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
      mij$bird_dir <- wrap360(geosphere::bearing(mij %>% dplyr::select(lon,lat))) # set bearing to [0,360)
      
      # Wind velocity and heading
      ddff <- uv2ddff(mij)
      mij$wind_vel <- ddff$ff # m/s
      mij$wind_dir <- ddff$dd # [0,360) degrees
      
      # bird-wind angle
      mij$bwa <- bearingAngle(mij$bird_dir,wrap360(mij$wind_dir-180)) # [0,180) degrees
      
      # In a 360 degree plot which direction is the bird traveling relative to 
      # the wind? 
      mij$w_rel <- (wrap360(mij$wind_dir-180)-mij$bird_dir) %% 360
      
      # Save file
      mij$datetime <- as.character(format(mij$datetime)) # safer for writing csv in character format
      filename_chunk_id <- as.character(mij$tripID[j])
      write_csv(mij, paste0(wind_L2_dir, filename_chunk_id, "_bwa.csv"))
      
      rm("mij")
    }
  }
}
}
}
