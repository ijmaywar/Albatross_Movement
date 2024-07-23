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
nc_dir <- paste0(GD_dir,"L0/",location,"/Env_Data/",szn,"/ERA5_Wind_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")

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


# Download the netcdf files from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 
wind_files

###########################################################################################################
# For Midway wind data ---------------------------------------------------------
# ONLY FOR MIDWAY

# COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
wind_E_Jan_Feb_component <- rast(wind_files[1])
wind_W_Jan_Feb_component <- rast(wind_files[2])
wind_raster <- merge(wind_E_Jan_Feb_component,wind_W_Jan_Feb_component)
wind_raster # Make sure you got the right stuff!

all_times <- unique(time(wind_raster))
all_times_num <- as.numeric(unique(time(wind_raster)))

new_columns <- unique(unlist(str_split(names(wind_raster), "_"))[seq(1,2*length(names(wind_raster)),2)])

# For Bird Island wind data ----------------------------------------------------
# ONLY FOR BIRD_ISLAND

# Load netcdf file directly for Bird Island
wind_Nov_Dec_raster <- rast(wind_files[1])
wind_Jan_Feb_raster <- rast(wind_files[2])
wind_raster <- c(wind_Nov_Dec_raster,wind_Jan_Feb_raster)
wind_raster # Make sure you got the right stuff!

all_times <- unique(time(wind_raster))
all_times_num <- as.numeric(unique(time(wind_raster)))

new_columns <- unique(varnames(wind_raster))
###########################################################################################################

# Loop through m and add wind information: u, v ---------------------------

for (new_col in new_columns) {
  m[new_col] <- NA
}

for (j in 1:nrow(m)) {
  
  timej <- m$datetime[j]
  timej_num <- as.numeric(timej)
  
  # Find index of current_time in all times. Use that index to pull out relevant raster layer.
  timej_diff <- abs(all_times_num-timej_num) # taking difference between all gps points
  # DateTimes in the exact middle will be assigned to the first index
  # 1800 seconds is 30 minutes
  min_time_diff <- 1800
  if (min(timej_diff) > min_time_diff) {
    print("Current datetime is outside of the data provided by NETCDF files.")
    break
  }
  
  closest_dt <- all_times[which.min(timej_diff)]  # Find the min: tells which layer to pull out and isolate. 
  
  # Isolate raster at time j
  wind_raster_timej <- wind_raster[[which(time(wind_raster)==closest_dt)]]
  
  # isolate coordinates
  xy_j <- as.data.frame(cbind(m$lon[j], m$lat[j]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract wind data for time j at location x and y
  wind_data_j <- extract(wind_raster_timej, xy_j, ID=FALSE)
  
  if (nrow(wind_data_j) != 1) {
    print("Number of wind measurements chosen != 1.")
    break
  } else if (all(is.na(wind_data_j))) {
    print(paste0("wind data not found for lon: ",as.character(xy_j)[1],
                 ", lat: ",as.character(xy_j)[2], 
                 ", time: ",as.character(closest_dt),
                 " for ",m$id[j],"."))
  }
  
  # Ensure that the data are still in the correct order
  if (!all.equal(colnames(m)[6:ncol(m)],
                 unlist(str_split(colnames(wind_data_j), "_"))
                 [seq(1,2*length(colnames(wind_data_j)),2)])) {
    disp("The raster data are out of order of the columns in m")
    break
  }
  
  m[j,6:ncol(m)] <- wind_data_j
  
}


# Save compiled GPS data with wind U and V --------------------------------

m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write_csv(m,file=paste0(wind_L1_dir,szn,"_allbirds_GPS_with_wind.csv"))

temp <- read_csv("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1ZwIQqcVnA34cxm_324gF2ie4UtWb8-Qb/Thorne Lab Shared Drive/Data/Albatross/L1/Midway/Env_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/2018_2019/2018_2019_allbirds_GPS_with_wind.csv")
temp$datetime <- as.POSIXct(temp$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
# compare to prev version
all.equal(m,temp)
