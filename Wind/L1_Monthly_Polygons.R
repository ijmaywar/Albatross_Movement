################################################################################
#
# Wind data: Extract data from netcdfs, allign with spatial polygons created by KDEs
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"

if (location == 'Bird_Island') {
  nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/compiled_2019_2022/ERA5_Monthly_Avg_10m/")
} else if (location == 'Midway') {
  nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/compiled_2018_2023/ERA5_Monthly_Avg_10m/")
}

GPS_dir <- paste0(GD_dir,"L4/",location,"/Tag_Data/GPS/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/ERA5_Monthly_Avg_10m/KDEs/",szn,"/")
# ? Where is this directory???

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


# Download the netcdf files from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 

# For Midway wind data --------------------------------------------------------------
# DON'T RUN THIS FOR BIRD_ISLAND

# COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
wind_E_t1 <- rast(wind_files[5])
wind_W_t1 <- rast(wind_files[6])
wind_t1 <- merge(wind_W_t1,wind_E_t1)

# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

u_stack <- c(wind_t1_u)
v_stack <- c(wind_t1_v)
all_times <- c(times_t1)
all_times_num <- as.numeric(all_times)


# For Bird Island wind data -----------------------------------------------
# DON'T RUN THIS FOR MIDWAY

# COMBINE E AND W COMPONENTS OF NETCDF FILES for Bird_Island
wind_t1 <- rast(wind_files[1])

# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

u_stack <- c(wind_t1_u)
v_stack <- c(wind_t1_v)
all_times <- c(times_t1)
all_times_num <- as.numeric(all_times)




# Load netcdf file directly for Bird Island
wind_t1 <- rast(wind_files[1])
# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

# Load netcdf file directly for Bird Island
wind_t2 <- rast(wind_files[2])
# Create data vectors
wind_t2 # Make sure you got the right stuff!
times_t2 <- time(wind_t2)  # stores times from each file 
times_t2 <- unique(times_t2) # find unique values because there should be two of every datetime (for u and v)
wind_t2_u <- subset(wind_t2, 1:(nlyr(wind_t2)/2)) 
wind_t2_v <- subset(wind_t2, (nlyr(wind_t2)/2)+1:nlyr(wind_t2))

# For monthly data, you don't need to stack rasters
u_stack <- wind_t1_u
v_stack <- wind_t1_v
all_times <- times_t1
all_times_num <- as.numeric(all_times)


# Loop through m and add wind information: u, v ---------------------------

# create u and v wind vectors
for (j in 1:nrow(m)) {
  
  timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
  timej_num <- as.numeric(timej)
  
  # Find index of current_time in all times. Use that index to pull out relevant raster layer.
  timej_diff <- abs(all_times_num-timej_num) # taking difference between all gps points
  raster_dt_index <- which.min(timej_diff)  # Find the min: tells which layer to pull out and isolate. 
  # DateTimes in the exact middle will be assigned to the first index
  
  # 1341600 seconds is 15.5 days
  min_time_diff <- 1341600
  
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
    print("Number of wind measurements chosen != 1.")
    break
  } else if (is.na(u_j[[1]]) || is.na(v_j[[1]])) {
    print("Wind data for this coordinate cannot be found.")
    break
  }
  
  m$u[j]<- u_j[[1]]
  m$v[j]<- v_j[[1]]
  
}


# Save compiled GPS data with wind U and V --------------------------------

m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write_csv(m,file=paste0(wind_L1_dir,szn,"_allbirds_GPS_with_wind.csv"))
# write_csv(m,file=paste0(wind_L1_dir,szn,"_WAAL_GPS_with_wind.csv"))
