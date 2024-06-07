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

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2021_2022"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

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

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
env_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/",szn,"/")
env_L2_dir <- paste0(GD_dir,"L2/",location,"/Env_Data/ERA5_SingleLevels_10m/",szn,"/")

# Save compiled GPS data with wind U and V --------------------------------

# Find wind data
setwd(env_L1_dir)
env_L1_files <- list.files(pattern='*.csv') 

# ADD BIRD BEHAVIOR -------------------------------------------------------

int_now <- 600 
hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)

# Merge files

Waves_21_22_2 <- read_csv(env_L1_files[1])
Waves_21_22_1 <- read_csv(env_L1_files[2])
Wind_21_22 <- read_csv(env_L1_files[3])

Waves_21_22 <- rbind(Waves_21_22_1[1:154029,],Waves_21_22_2[154030:nrow(Waves_21_22_2),])
  
env_data <- merge(Wind_21_22,Waves_21_22,by=colnames(Waves_21_22)[1:5])
env_data$datetime <- as.POSIXct(env_data$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")

tripnames <- unique(env_data$tripID)

for (i in 1:length(tripnames)) {
  
  # UPDATE THIS - JUST FIGURE OUT THE "_" SEPARATION METHOD
  tripname <- tripnames[i]
  birdname <- str_sub(tripname,1,-3)
  
  env_data_i <- env_data %>% filter(tripID==tripname)

  # Set up columns
  env_data_i$bird_dir <-NA # THE DIRECTION THE BIRD IS HEADING IN
  env_data_i$bird_vel <-NA  # km/hr 
  
  # Wind stats
  env_data_i$wind_vel <- NA # m/s
  env_data_i$wind_dir <- NA # THE DIRECTION THE WIND IS COMING FROM
  env_data_i$bird_wind_angle <- NA # Bird-wind angle [0,180) degrees
  env_data_i$wind_rel <- NA # wind bearing relative to bird heading. The direction the wind is travelling toward
  
  # Wave stats
  env_data_i$bird_wave_angle <- NA # Bird-wave angle [0,180) degrees
  env_data_i$wave_rel <- NA # wave bearing relative to bird heading. The direction the wave is travelling toward
  env_data_i$bird_swell_angle <- NA # Bird-swell angle [0,180) degrees
  env_data_i$swell_rel <- NA # swell bearing relative to bird heading. The direction the wave is travelling toward
  env_data_i$bird_ww_angle <- NA # Bird-windwave angle [0,180) degrees
  env_data_i$ww_rel <- NA # windwave bearing relative to bird heading. The direction the wave is travelling toward
  
  # Bird velocity and heading
  env_data_i$lon <- Lon360to180(env_data_i$lon)
  env_data_i$bird_vel <- c(distHaversine(env_data_i %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
  env_data_i$bird_dir <- wrap360(geosphere::bearing(env_data_i %>% dplyr::select(lon,lat))) # set bearing to [0,360)
  
  # Wind stats
  ddff <- uv2ddff(env_data_i)
  env_data_i$wind_vel <- ddff$ff # m/s
  env_data_i$wind_dir <- ddff$dd # [0,360) degrees
  env_data_i$bird_wind_angle <- bearingAngle(env_data_i$bird_dir,wrap360(env_data_i$wind_dir-180)) # [0,180) degrees
  env_data_i$wind_rel <- (wrap360(env_data_i$wind_dir-180)-env_data_i$bird_dir) %% 360
  
  # Wave stats
  env_data_i$bird_wave_angle <- bearingAngle(env_data_i$bird_dir,wrap360(env_data_i$mwd-180)) # [0,180) degrees
  env_data_i$wave_rel <- (wrap360(env_data_i$mwd-180)-env_data_i$bird_dir) %% 360
  env_data_i$bird_swell_angle <- bearingAngle(env_data_i$bird_dir,wrap360(env_data_i$mdts-180)) # [0,180) degrees
  env_data_i$swell_rel <- (wrap360(env_data_i$mdts-180)-env_data_i$bird_dir) %% 360
  env_data_i$bird_ww_angle <- bearingAngle(env_data_i$bird_dir,wrap360(env_data_i$mdww-180)) # [0,180) degrees
  env_data_i$ww_rel <- (wrap360(env_data_i$mdww-180)-env_data_i$bird_dir) %% 360
  
  # Save file
  env_data_i$datetime <- as.character(format(env_data_i$datetime)) # safer for writing csv in character format
  write_csv(env_data_i, paste0(env_L2_dir, tripname, "_Env_L2_BWAs.csv"))
  
}
}
}

