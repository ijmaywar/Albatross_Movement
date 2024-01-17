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

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2021_2022"
interp = "600s"

# Packages  ---------------------------------------------------------

# require(sf)
require(tidyverse)
# require(raster)
# library(nngeo)
library(lubridate)
library(terra)
# library(stars)
# library(ncmeta)
# library(ncdf4)
# library(RNetCDF)
# library(lattice)
# library(rasterVis) #vectorplot
# library(colorRamps) #matlab.like
# library(viridisLite)
# library(colorspace)
# library(DescTools) #closest
# library(imputeTS) #na.interpolation
library(geosphere)
library(foehnix)
# require(ggplot2)
# require(RColorBrewer)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/",interp,"/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/",szn,"/",interp,"/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/",szn,"/",interp,"/")


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

# Save compiled file
# m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write.csv(m, file=paste0(GPS_dir,"compiled/",szn,"_compiled.csv"), row.names=FALSE)

# Make sure all birds are found within the grid extent
# Bird Island grid extent: -30N -70S -120W -10E (big range due to Wandering!)
# Midway grid extent: 50N 20S 140W -140E
  # NOTE: Midway data must be downloaded in two parts because you cannot cross the 
  # 180 lon line when specifying grid extent: latN latS lonW 180 and latN latS -180 latE

max(m$lat)
min(m$lat)
Lon360to180(max(m$lon))
Lon360to180(min(m$lon))
min(m$datetime)
max(m$datetime)


# Download the netcdf files from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 

# For Midway wind data --------------------------------------------------------------

# COMBINE E AND W COMPONENTS OF NETCDF FILES for Midway
wind_E_t1 <- rast(wind_files[1])
wind_W_t1 <- rast(wind_files[2])
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

# Load netcdf file directly for Bird Island
wind_t1 <- rast(wind_files[6])
# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

# Load netcdf file directly for Bird Island
wind_t2 <- rast(wind_files[7])
# Create data vectors
wind_t2 # Make sure you got the right stuff!
times_t2 <- time(wind_t2)  # stores times from each file 
times_t2 <- unique(times_t2) # find unique values because there should be two of every datetime (for u and v)
wind_t2_u <- subset(wind_t2, 1:(nlyr(wind_t2)/2)) 
wind_t2_v <- subset(wind_t2, (nlyr(wind_t2)/2)+1:nlyr(wind_t2))

# Stack rasters
u_stack <- c(wind_t1_u, wind_t2_u)
v_stack <- c(wind_t1_v, wind_t2_v)
all_times <- c(times_t1, times_t2)
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
   if (min(timej_diff) > 1800) {
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

# m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write_csv(m,file=paste0(wind_L1_dir,szn,"_allbirds_GPS_with_wind.csv"))
# wind_mat <- m

# If "_allbirds_GPS_with_wind.csv" already exists then load it instead
# wind_mat <- read.csv(paste0(wind_L1_dir,szn,"_allbirds_GPS_with_wind.csv"))

# ADD BIRD BEHAVIOR -------------------------------------------------------

bird_list <- unique(wind_mat$id)

if (interp=="600s") {
  int_now <- 600 
}

hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)

for (i in 1:length(bird_list)) {
  
  # Isolate bird
  mi <- wind_mat %>% filter(id==bird_list[i])
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(mi$tripID)
  
  for (j in 1:length(trips)) {
    
    mij <- mi %>% filter(tripID==trips[j])
    
    if (nrow(mij) >= 6*2) { # If file is so short that it's less than 2 hours , don't do anything.
    
      mij$wind_vel <- NA
      mij$wind_dir <- NA # m/s, THE DIRECTION THE WIND IS COMING FROM
      mij$bird_dir <-NA
      mij$bird_vel <-NA  # km/hr, THE DIRECTION THE BIRD IS HEADING IN
      mij$bwa <- NA
      mij$w_rel <- NA # wind bearing relative to bird heading
      
      # Add bird speed and bearing ----------------------------------------------
      
      # Bird velocity and heading
      mij$lon <- Lon360to180(mij$lon)
      mij$bird_vel <- c(distHaversine(mij %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
      mij$bird_dir <- geosphere::bearing(mij %>% dplyr::select(lon,lat))
      mij$bird_dir <- wrap360(mij$bird_dir) # set bearing to [0,360)
      
      # Wind velocity and heading
      ddff <- uv2ddff(mij)
      mij$wind_vel <- ddff$ff # m/s
      mij$wind_dir <- ddff$dd # [0,360) degrees
      
      # bird-wind angle
      mij$bwa <- bearingAngle(mij$bird_dir,mij$wind_dir) # [0,180) degrees
      
      # In a 360 degree plot which direction is the bird traveling relative to 
      # the wind? 
      mij$w_rel <- (mij$bird_dir+(360-mij$wind_dir)) %% 360
      
      # Save file
      mij$datetime <- as.character(format(mij$datetime)) # safer for writing csv in character format
      filename_chunk_id <- as.character(mij$tripID[1])
      write_csv(mij, paste0(wind_L2_dir, filename_chunk_id, "_bwa.csv"))
      
      rm("mij")
    }
  }
}



