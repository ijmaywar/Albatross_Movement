################################################################################
#
# Calculate bird-wave angles.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2018_2019"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
nc_dir <- paste0(GD_dir,"L0/",location,"/wave_Data/",szn,"/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wave_L1_dir <- paste0(GD_dir,"L1/",location,"/Wave_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wave/",szn,"/")
wave_L2_dir <- paste0(GD_dir,"L2/",location,"/Wave_Data/ERA5_SingleLevels_10m/",szn,"/")

# User Functions ----------------------------------------------------------

wrap360 <- function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Both bearings must be [0,360)
bearingAngle <- function(bird_dir,wave_dir) {
  LHturn <- wrap360(bird_dir - wave_dir)
  RHturn <- wrap360(wave_dir - bird_dir)
  return(pmin(LHturn,RHturn))
}

# Save compiled GPS data with wave U and V --------------------------------

setwd(wave_L1_dir)
wave_mat <- read.csv(paste0(szn,"_allbirds_GPS_with_wave.csv"))

# ADD BIRD BEHAVIOR -------------------------------------------------------

bird_list <- unique(wave_mat$id)

int_now <- 600 
hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)

for (i in 1:length(bird_list)) {
  
  # Isolate bird
  mi <- wave_mat %>% filter(id==bird_list[i])
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(mi$tripID)
  
  for (j in 1:length(trips)) {
    
    mij <- mi %>% filter(tripID==trips[j])
    
    if (nrow(mij) >= 6*2) { # If file is so short that it's less than 2 hours , don't do anything.
      
      # mij$wave_dir <- NA # THE DIRECTION THE WAVE IS COMING FROM
      mij$bird_dir <- NA # THE DIRECTION THE BIRD IS HEADING IN
      # mij$bird_vel <-NA  # km/hr 
      mij$b_wave_a <- NA # Bird-wave angle [0,180) degrees
      mij$wave_rel <- NA # wave bearing relative to bird heading
      
      # Add bird speed and bearing ----------------------------------------------
      
      # Bird velocity and heading
      mij$lon <- Lon360to180(mij$lon)
      # mij$bird_vel <- c(distHaversine(mij %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
      mij$bird_dir <- wrap360(geosphere::bearing(mij %>% dplyr::select(lon,lat))) # set bearing to [0,360)
      
      # bird-wave angle
      mij$b_wave_a <- bearingAngle(mij$bird_dir,wrap360(mij$mwd-180)) # [0,180) degrees
      
      # In a 360 degree plot which direction is the bird traveling relative to 
      # the wave? 
      # mij$wave_rel <- (mij$bird_dir+(360-wrap360(mij$mwd-180))) %% 360
      mij$wave_rel <- (wrap360(mij$mwd-180)-mij$bird_dir) %% 360

      # Save file
      mij$datetime <- as.character(format(mij$datetime)) # safer for writing csv in character format
      filename_chunk_id <- as.character(mij$tripID[1])
      write_csv(mij, paste0(wave_L2_dir, filename_chunk_id, "_b_wave_a.csv"))
      
      rm("mij")
    }
  }
}
