################################################################################
#
# Wind data: Extract data from netcdfs, allign with bird positions
# Based on 1-mgc_wind_extract-to-latlon_part2_Aug2021.R by Melinda. Rewritten
# for use in the Terra package becasue rgdal is no longer supported.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2020_2021"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
nc_wind_dir <- paste0(GD_dir,"L0/",location,"/Env_Data/",szn,"/ERA5_Wind_SingleLevels_10m/")
nc_wave_dir <- paste0(GD_dir,"L0/",location,"/Env_Data/",szn,"/ERA5_Wave_SingleLevels_10m/")
env_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/",szn,"/")

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

# Get compiled GPS data of all birds in szn ------

setwd(GPS_dir)
files <- list.files(pattern='*.csv') # GPS files 

# Create compiled file
for (i in 1:length(files)) {
  mi <- read_csv(files[i])
  if (i==1) {
    m <- mi
  } else {
    m<- rbind(m,mi)
  }
}

m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")

# Make sure all birds are found within the grid extent
# Bird Island grid extent: -30N -70S -120W -10E (big range due to Wandering!)
# Midway grid extent: 50N 20S 140W -140E
# NOTE: Midway data must be downloaded in two parts because you cannot cross the 
# 180 lon line when specifying grid extent: latN latS lonW 180 (E_Midway) and 
# latN latS -180 latE (W_Midway)

max(m$lat)
min(m$lat)                         
Lon360to180(max(m$lon))
Lon360to180(min(m$lon))
min(m$datetime)
max(m$datetime)

###########################################################################################################
if (location == "Midway") { 
  
  # Wind data
  # COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
  setwd(nc_wind_dir)
  wind_files <- list.files(pattern='*.nc') 
  wind_E_Jan_Feb_component <- rast(wind_files[1])
  wind_W_Jan_Feb_component <- rast(wind_files[2])
  wind_raster <- merge(wind_E_Jan_Feb_component,wind_W_Jan_Feb_component)
  wind_raster # Make sure you got the right stuff!
  
  all_wind_times <- unique(time(wind_raster))
  all_wind_times_num <- as.numeric(unique(time(wind_raster)))
  
  new_wind_columns <- unique(unlist(str_split(names(wind_raster), "_"))[seq(1,2*length(names(wind_raster)),2)])
  
  # Wave data
  setwd(nc_wave_dir)
  wave_files <- list.files(pattern='*.nc') 
  wave_E_Jan_component <- rast(wave_files[1])
  wave_W_Jan_component <- rast(wave_files[2])
  wave_E_Feb_component <- rast(wave_files[3])
  wave_W_Feb_component <- rast(wave_files[4])
  wave_Jan_raster <- merge(wave_W_Jan_component,wave_E_Jan_component)
  wave_Feb_raster <- merge(wave_W_Feb_component,wave_E_Feb_component)
  wave_raster <- c(wave_Jan_raster,wave_Feb_raster)
  wave_raster # Make sure you got the right stuff!
  
  all_wave_times <- unique(time(wave_raster))
  all_wave_times_num <- as.numeric(unique(time(wave_raster)))
  
  new_wave_columns <- unique(unlist(str_split(names(wave_raster), "_"))[seq(1,2*length(names(wave_raster)),2)])

} else if (location == "Bird_Island") {
  
  # Wind data
  setwd(nc_wind_dir)
  wind_files <- list.files(pattern='*.nc') 
  wind_yr1_raster <- rast(wind_files[1])
  wind_yr2_raster <- rast(wind_files[2])
  wind_raster <- c(wind_yr1_raster,wind_yr2_raster)
  wind_raster # Make sure you got the right stuff!
  
  all_wind_times <- unique(time(wind_raster))
  all_wind_times_num <- as.numeric(unique(time(wind_raster)))
  
  new_wind_columns <- unique(varnames(wind_raster))
  
  # Wave data
  setwd(nc_wave_dir)
  wave_files <- list.files(pattern='*.nc') 
  if (month(min(m$datetime))==12) {
    wave_Dec_raster <- rast(wave_files[1])
    wave_Jan_raster <- rast(wave_files[2])
    wave_raster <- c(wave_Dec_raster,wave_Jan_raster)
  } else {
    wave_Nov_raster <- rast(wave_files[1])
    wave_Dec_raster <- rast(wave_files[2])
    wave_Jan_raster <- rast(wave_files[3])
    wave_raster <- c(wave_Nov_raster,wave_Dec_raster,wave_Jan_raster)
  }
  wave_raster # Make sure you got the right stuff!
  
  all_wave_times <- unique(time(wave_raster))
  all_wave_times_num <- as.numeric(unique(time(wave_raster)))
  
  new_wave_columns <- unique(varnames(wave_raster))
  
}


###########################################################################################################

# Loop through m and add env data ----------------------------------------------

# Create env data columns for m
new_columns=c(new_wind_columns,new_wave_columns)
for (new_col in new_columns) {
  m[new_col] <- NA
}

all_trips <- unique(m$tripID)

for (tripname in all_trips) {
  
  m_trip <- m %>% filter(tripID==tripname)
  
  # If the trip is less than 2 hours, throw it out.
  if (nrow(m_trip) < 6*2) {
    next
  }
  
  for (j in 1:nrow(m_trip)) {
  
    timej <- m_trip$datetime[j]
    timej_num <- as.numeric(timej)
    
    # Isolate coordinates
    xy_j <- as.data.frame(cbind(m_trip$lon[j], m_trip$lat[j]))
    colnames(xy_j) <- c("lon","lat")
    xy_j$lon <- Lon360to180(xy_j$lon) 
    
    # Extract wind data --------------------------------------------------------
    timej_diff_wind <- abs(all_wind_times_num-timej_num)
    if (min(timej_diff_wind) > 1800) { # 1800 seconds is 30 minutes
      print("Current datetime is outside of the data provided by NETCDF files.")
      break
    }
    
    closest_wind_dt <- all_wind_times[which.min(timej_diff_wind)]  # Find the min: tells which layer to pull out and isolate. 
    # Isolate raster at time j
    wind_raster_timej <- wind_raster[[which(time(wind_raster)==closest_wind_dt)]]
    # Extract wind data for time j at location x and y
    wind_data_j <- extract(wind_raster_timej, xy_j, ID=FALSE)
    
    if (nrow(wind_data_j) != 1) {
      print("Number of wind measurements chosen != 1.")
      break
    } else if (all(is.na(wind_data_j))) {
      print(paste0("wind data not found for lon: ",as.character(xy_j)[1],
                   ", lat: ",as.character(xy_j)[2], 
                   ", time: ",as.character(closest_dt),
                   " for ",m_trip$id[j],"."))
    }
    
    # Extract wave data --------------------------------------------------------
    timej_diff_wave <- abs(all_wave_times_num-timej_num)
    if (min(timej_diff_wave) > 1800) {
      print("Current datetime is outside of the data provided by NETCDF files.")
      break
    }
    
    closest_wave_dt <- all_wave_times[which.min(timej_diff_wave)]  # Find the min: tells which layer to pull out and isolate. 
    # Isolate raster at time j
    wave_raster_timej <- wave_raster[[which(time(wave_raster)==closest_wave_dt)]]
    # Extract wave data for time j at location x and y
    wave_data_j <- extract(wave_raster_timej, xy_j, ID=FALSE)
    
    if (nrow(wave_data_j) != 1) {
      print("Number of wave measurements chosen != 1.")
      break
    } else if (all(is.na(wave_data_j))) {
      print(paste0("wave data not found for lon: ",as.character(xy_j)[1],
                   ", lat: ",as.character(xy_j)[2], 
                   ", time: ",as.character(closest_dt),
                   " for ",m_trip$id[j],"."))
    }
    
    env_data_j <- cbind(wind_data_j,wave_data_j)
    # Rename columns to match the variables
    names(env_data_j) <- unlist(str_split(colnames(env_data_j), "_"))[seq(1,2*length(colnames(env_data_j)),2)]
    # Ensure that the data is in the right order and add data to m_trip
    if (all.equal(new_columns,names(env_data_j))) {
      # Add data to m_trip  
      m_trip[j,new_columns] <- env_data_j
    } else {
      disp("The data is out of order. Breaking")
      break
    }
    
  }
  
  # Save file
  m_trip$datetime <- as.character(format(m_trip$datetime)) # safer for writing csv in character format
  write_csv(m_trip,file=paste0(env_L1_dir,tripname,"_Env_L1.csv"))
  
}
