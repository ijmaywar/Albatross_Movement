################################################################################
#
# Extract wave data for GPS locations provided by 600s interpolated data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2022_2023"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Env_Data/",szn,"/ERA5_Wave_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wave_L1_dir <- paste0(GD_dir,"L1/",location,"/Env_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wave/",szn,"/")

# User Functions ----------------------------------------------------------

wrap360 <- function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Get compiled GPS data of all birds in szn -----------------------------------

setwd(GPS_dir)
files <- list.files(pattern='*.csv') # GPS files 

# Create compiled file
for (i in 1:length(files)) {
  mi <- read.csv(files[i])
  if (i==1) {
    m <- mi
  } else {
    m<- rbind(m,mi)
  }
}

# Make sure all birds are found within the grid extent
# Set the grid to download each necesary component of ERA5 data.

# Roughly:
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
wave_files <- list.files(pattern='*.nc') 

# For Midway wave data --------------------------------------------------------------
# DON'T RUN THIS FOR BIRD_ISLAND

wave_files
# COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
wave_E_Jan_component <- rast(wave_files[1])
wave_W_Jan_component <- rast(wave_files[2])
wave_E_Feb_component <- rast(wave_files[3])
wave_W_Feb_component <- rast(wave_files[4])
wave_Jan_raster <- merge(wave_W_Jan_component,wave_E_Jan_component)
wave_Feb_raster <- merge(wave_W_Feb_component,wave_E_Feb_component)
wave_raster <- c(wave_Jan_raster,wave_Feb_raster)
wave_raster # Make sure you got the right stuff!

all_times <- unique(time(wave_raster))
all_times_num <- as.numeric(unique(time(wave_raster)))

new_columns <- unique(unlist(str_split(names(wave_raster), "_"))[seq(1,2*length(names(wave_raster)),2)])
for (new_col in new_columns) {
  m[new_col] <- NA
}

# For Bird Island wave data -----------------------------------------------
# DON'T RUN THIS FOR MIDWAY

# Load netcdf file directly for Bird Island
wave_Nov_raster <- rast(wave_files[1])
wave_Dec_raster <- rast(wave_files[2])
wave_Jan_raster <- rast(wave_files[3])
wave_raster <- c(wave_Nov_raster,wave_Dec_raster,wave_Jan_raster)
wave_raster # Make sure you got the right stuff!

all_times <- unique(time(wave_raster))
all_times_num <- as.numeric(unique(time(wave_raster)))

# Create columns for wave data
new_columns <- unique(varnames(wave_raster))
for (new_col in new_columns) {
  m[new_col] <- NA
}

# Loop through m and add wave information --------------------------------------

for (j in 1:nrow(m)) {
  
  timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
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
  wave_raster_timej <- wave_raster[[which(time(wave_raster)==closest_dt)]]

  # isolate coordinates
  xy_j <- as.data.frame(cbind(m$lon[j], m$lat[j]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract wave data for time j at location x and y
  wave_data_j <- extract(wave_raster_timej, xy_j, ID=FALSE)
  
  if (nrow(wave_data_j) != 1) {
    print("Number of wave measurements chosen != 1.")
    break
  } else if (all(is.na(wave_data_j))) {
    print("wave data for this coordinate cannot be found.")
    break
  }
  
  # Ensure that the data are still in the correct order
  if (!all.equal(colnames(m)[6:ncol(m)],
                 unlist(str_split(colnames(wave_data_j), "_"))
                 [seq(1,2*length(colnames(wave_data_j)),2)])) {
    disp("The raster data are out of order of the columns in m")
    break
  }
  
  m[j,6:ncol(m)] <- wave_data_j
  
}


# Save compiled GPS data with wave U and V --------------------------------

m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
write_csv(m,file=paste0(wave_L1_dir,szn,"_allbirds_GPS_with_wave.csv"))
