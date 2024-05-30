################################################################################
#
# Extract wave data for GPS locations provided by 600s interpolated data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Wave_Data/",szn,"/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wave_L1_dir <- paste0(GD_dir,"L1/",location,"/Wave_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wave/",szn,"/")

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

# COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
wave_E_component <- rast(wave_files[5])
wave_W_component <- rast(wave_files[6])
wave_raster <- merge(wave_W_component,wave_E_component)

# Create data vectors
wave_t1 # Make sure you got the right stuff!
times_t1 <- time(wave_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wave_t1_u <- subset(wave_t1, 1:(nlyr(wave_t1)/2)) 
wave_t1_v <- subset(wave_t1, (nlyr(wave_t1)/2)+1:nlyr(wave_t1))

u_stack <- c(wave_t1_u)
v_stack <- c(wave_t1_v)
all_times <- c(times_t1)
all_times_num <- as.numeric(all_times)


# For Bird Island wave data -----------------------------------------------
# DON'T RUN THIS FOR MIDWAY

# Load netcdf file directly for Bird Island
wave_t1 <- rast(wave_files[1])
# Create data vectors
wave_t1 # Make sure you got the right stuff!
times_t1 <- time(wave_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wave_t1_u <- subset(wave_t1, 1:(nlyr(wave_t1)/2)) 
wave_t1_v <- subset(wave_t1, (nlyr(wave_t1)/2)+1:nlyr(wave_t1))

# Load netcdf file directly for Bird Island
wave_t2 <- rast(wave_files[2])
# Create data vectors
wave_t2 # Make sure you got the right stuff!
times_t2 <- time(wave_t2)  # stores times from each file 
times_t2 <- unique(times_t2) # find unique values because there should be two of every datetime (for u and v)
wave_t2_u <- subset(wave_t2, 1:(nlyr(wave_t2)/2)) 
wave_t2_v <- subset(wave_t2, (nlyr(wave_t2)/2)+1:nlyr(wave_t2))

# Stack rasters
# Single levels
u_stack <- c(wave_t1_u, wave_t2_u)
v_stack <- c(wave_t1_v, wave_t2_v)
all_times <- c(times_t1, times_t2)
all_times_num <- as.numeric(all_times)

# Loop through m and add wave information: u, v ---------------------------

# create u and v wave vectors
for (j in 1:nrow(m)) {
  
  timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
  timej_num <- as.numeric(timej)
  
  # Find index of current_time in all times. Use that index to pull out relevant raster layer.
  timej_diff <- abs(all_times_num-timej_num) # taking difference between all gps points
  raster_dt_index <- which.min(timej_diff)  # Find the min: tells which layer to pull out and isolate. 
  # DateTimes in the exact middle will be assigned to the first index
  # 1800 seconds is 30 minutes
  min_time_diff <- 1800
  
  if (min(timej_diff) > min_time_diff) {
    print("Current datetime is outside of the data provided by NETCDF files.")
    break
  }
  
  # Isolate u and v rasters at time j
  ustack_timej <- subset(u_stack, raster_dt_index) 
  vstack_timej <- subset(v_stack, raster_dt_index)
  
  # isolate coordinates
  xy_j <- as.data.frame(cbind(m$lon[j], m$lat[j]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract u and v components for time j at location x and y
  u_j <- extract(ustack_timej, xy_j, ID=FALSE)
  v_j <- extract(vstack_timej, xy_j, ID=FALSE)
  
  if (length(u_j) != 1) {
    print("Number of wave measurements chosen != 1.")
    break
  } else if (is.na(u_j[[1]]) || is.na(v_j[[1]])) {
    print("wave data for this coordinate cannot be found.")
    break
  }
  
  m$u[j]<- u_j[[1]]
  m$v[j]<- v_j[[1]]
  
}


# Save compiled GPS data with wave U and V --------------------------------

m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write_csv(m,file=paste0(wave_L1_dir,szn,"_allbirds_GPS_with_wave.csv"))
# write_csv(m,file=paste0(wave_L1_dir,szn,"_WAAL_GPS_with_wave.csv"))
