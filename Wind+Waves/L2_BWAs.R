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
# szn = "2019_2020"

# locations = c("Bird_Island", "Midway")
locations = c("Midway")

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



# Loop to run thru all samples -------------------------------------------------

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
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")
wave_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wave/",szn,"/")
env_L2_dir <- paste0(GD_dir,"L2/",location,"/Env_Data/ERA5_SingleLevels_10m/",szn,"/")

# Save compiled GPS data with wind U and V --------------------------------

# Download wind data
setwd(wind_L1_dir)
wind_L1_data <- read_csv(paste0(szn,"_allbirds_GPS_with_wind.csv"))

# Download wave data
setwd(wave_L1_dir)
wave_L1_data <- read_csv(paste0(szn,"_allbirds_GPS_with_wave.csv"))

# Get gps filenames
setwd(GPS_dir)
gpsfiles<-list.files(pattern='*.csv')

# ADD BIRD BEHAVIOR -------------------------------------------------------

int_now <- 600 
hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)

for (i in 1:length(gpsfiles)) {
  
  birdname <- str_sub(gpsfiles[i],1,-17)

  # Isolate environmental data for bird
  wind_data_i <- wind_L1_data %>% filter(id==birdname)
  wave_data_i <- wave_L1_data %>% filter(id==birdname)
  
  # Merge envrionmental data into one df
  env_data_i <- merge(wind_data_i,wave_data_i,by=c("id","datetime","lon","lat","tripID"),all=TRUE)
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(env_data_i$tripID)
  
  for (j in 1:length(trips)) {
    
    env_data_ij <- env_data_i %>% filter(tripID==trips[j])
    
    if (nrow(env_data_ij) >= 6*2) { # If file is so short that it's less than 2 hours , don't do anything.
      
      env_data_ij$bird_dir <-NA # THE DIRECTION THE BIRD IS HEADING IN
      env_data_ij$bird_vel <-NA  # km/hr 
      
      # Wind stats
      env_data_ij$wind_vel <- NA # m/s
      env_data_ij$wind_dir <- NA # THE DIRECTION THE WIND IS COMING FROM
      env_data_ij$bird_wind_angle <- NA # Bird-wind angle [0,180) degrees
      env_data_ij$wind_rel <- NA # wind bearing relative to bird heading. The direction the wind is travelling toward
      
      # Wave stats
      env_data_ij$bird_wave_angle <- NA # Bird-wave angle [0,180) degrees
      env_data_ij$wave_rel <- NA # wave bearing relative to bird heading. The direction the wave is travelling toward
      env_data_ij$bird_swell_angle <- NA # Bird-swell angle [0,180) degrees
      env_data_ij$swell_rel <- NA # swell bearing relative to bird heading. The direction the wave is travelling toward
      env_data_ij$bird_ww_angle <- NA # Bird-windwave angle [0,180) degrees
      env_data_ij$ww_rel <- NA # windwave bearing relative to bird heading. The direction the wave is travelling toward
      
      # Bird velocity and heading
      env_data_ij$lon <- Lon360to180(env_data_ij$lon)
      env_data_ij$bird_vel <- c(distHaversine(env_data_ij %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
      env_data_ij$bird_dir <- wrap360(geosphere::bearing(env_data_ij %>% dplyr::select(lon,lat))) # set bearing to [0,360)
      
      # Wind stats
      ddff <- uv2ddff(env_data_ij)
      env_data_ij$wind_vel <- ddff$ff # m/s
      env_data_ij$wind_dir <- ddff$dd # [0,360) degrees
      env_data_ij$bird_wind_angle <- bearingAngle(env_data_ij$bird_dir,wrap360(env_data_ij$wind_dir-180)) # [0,180) degrees
      env_data_ij$wind_rel <- (wrap360(env_data_ij$wind_dir-180)-env_data_ij$bird_dir) %% 360
      
      # Wave stats
      env_data_ij$bird_wave_angle <- bearingAngle(env_data_ij$bird_dir,wrap360(env_data_ij$mwd-180)) # [0,180) degrees
      env_data_ij$wave_rel <- (wrap360(env_data_ij$mwd-180)-env_data_ij$bird_dir) %% 360
      env_data_ij$bird_swell_angle <- bearingAngle(env_data_ij$bird_dir,wrap360(env_data_ij$mdts-180)) # [0,180) degrees
      env_data_ij$swell_rel <- (wrap360(env_data_ij$mdts-180)-env_data_ij$bird_dir) %% 360
      env_data_ij$bird_ww_angle <- bearingAngle(env_data_ij$bird_dir,wrap360(env_data_ij$mdww-180)) # [0,180) degrees
      env_data_ij$ww_rel <- (wrap360(env_data_ij$mdww-180)-env_data_ij$bird_dir) %% 360
      
      # Save file
      env_data_ij$datetime <- as.character(format(env_data_ij$datetime)) # safer for writing csv in character format
      filename_chunk_id <- as.character(env_data_ij$tripID[j])
      write_csv(env_data_ij, paste0(env_L2_dir, filename_chunk_id, "_BWAs.csv"))
      
    }
  }
}
}
}
