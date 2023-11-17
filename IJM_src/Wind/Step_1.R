################################################################################
#
# Wind data: first step
# Based on 1-mgc_wind_extract-to-latlon_part2_Aug2021.R by Melinda. Rewritten
# for use in the Terra package becasue rgdal is no longer supported.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
interp = "600s"

# Packages  ---------------------------------------------------------

require(sf)
require(tidyverse)
require(raster)
library(nngeo)
library(lubridate)
# library(rgdal)
library(terra)
library(stars)
library(ncmeta)
library(ncdf4)
library(RNetCDF)
library(lattice)
# library(rCAT) #rad2deg()
library(rasterVis) #vectorplot
library(colorRamps) #matlab.like
library(viridisLite)
library(colorspace)
library(DescTools) #closest
library(imputeTS) #na.interpolation
# library(swfscMisc)
library(geosphere)
library(foehnix)
require(ggplot2)
require(RColorBrewer)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/",interp,"/")
wind_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/",szn,"/",interp,"/")


# User Functions ----------------------------------------------------------

wrap360 <- function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Note: Important: Run code at bottom of script for uv2ddff function

########################################################################
# 1. Precursor to Analysis
# Obtain wind dataset from ECMWF ERA5 at sea level (1000 hPa)        <- why does this say sea level, shouldn't it be 10 m? (IJM)
# Grid Extent: -30N -70S -80E -20E
# Here: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=form
########################################################################


# Import Wind data --------------------------------------------------------
# (Downloaded as NetCDF grids from ECMWF)
# Stack all wind datasets.
# Each stack will be associated with a date and hourly timestamp (24 per day)
setwd(nc_dir)

wind_files <- list.files(pattern='*.nc') 

wind_t1 <- rast(wind_files[2])
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

wind_t2 <- rast(wind_files[3])
wind_t2 # Make sure you got the right stuff!
times_t2 <- time(wind_t2)  # stores times from each file 
times_t2 <- unique(times_t2) # find unique values because there should be two of every datetime (for u and v)
wind_t2_u <- subset(wind_t2, 1:(nlyr(wind_t2)/2)) 
wind_t2_v <- subset(wind_t2, (nlyr(wind_t2)/2)+1:nlyr(wind_t2))


# Stack rasters  ....................
u_stack <- c(wind_t1_u, wind_t2_u)
v_stack <- c(wind_t1_v, wind_t2_v)

all_times <- c(times_t1, times_t2)
all_times_num <- as.numeric(all_times)

# all_times <- rbind(as_tibble(times_t1), as_tibble(times_t2))
# all_times_num <- as.numeric(unlist(all_times)) # append times: one big columns of time


# Read in bird locations and gather wind data ----------------------

setwd(GPS_dir)
files <- list.files() # GPS files 

# Get compiled GPS data of all birds in szn ------

# Create compiled file
# for (i in 1:length(files)) {
#   mi<-read.csv(files[i])
#   if (i==1) {
#     m<-mi
#   }else{
#     m<-rbind(m,mi)
#   }
# } 

# # Save compiled file
# m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format
# write.csv(m, file=paste0(GPS_dir,"/compiled/",szn,"_compiled.csv"), row.names=FALSE)

# OR load compiled file
m <- read.csv(paste0(GPS_dir,"/compiled/",szn,"_compiled.csv"))
m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format

# Loop through m and add wind information: u, v ---------------------------

# create u and v wind vectors
m$u <- NA
m$v <- NA

for ( j in 1:length(m$id)) {
  
  timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
  timej_num <- as.numeric(timej)
  # Find index of current_time in all times. Use that index to pull out relevant raster layer. 
  timej_diff <- abs(all_times_num-timej_num) # taking difference between all gps points
  raster_dt_index <- which.min(timej_diff)  # Find the min: tells which layer to pull out and isolate. 
                                            # DateTimes in the exact middle will be assigned to the first index
  
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
    break
  }
  
  m$u[j]<- u_j
  m$v[j]<- v_j
  
}


# Save compiled GPS data with wind U and V --------------------------------

write_csv(m,file=paste0(wind_dir,szn,"_allbirds_GPS_with_wind.csv"))



# ADD BIRD BEHAVIOR -------------------------------------------------------

# If already in environment 
wind_mat <- m
bird_list <- unique(wind_mat$id)

## If you need to load it
# ind_mat <- read.csv(paste0(wind_dir,szn,"_allbirds_GPS_with_wind.csv"))
for (i in 1:length(m$id)) {
  
  # Isolate bird
  mi <- wind_mat %>% filter(id==bird_list[i])
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(mi$tripID)
  
  # IGNORE SHORT TRIPS??????????????????????????????????????????????????????????
  
  for (j in 1:length(trips)) {
    
    mij <- mi %>% filter(tripID==trips[j])
    # wind_mii <-wind_mat[wind_mat$id == birdi,]
    
    mij$wind_vel <- NA
    mij$wind_dir <- NA # m/s
    mij$bird_dir <-NA
    mij$bird_vel <-NA  # km/hr
    mij$bwa <- NA
    mij$bwa_class <- NA
    
    
    # Add bird speed and bearing ----------------------------------------------

    if (interp=="600s") {
      int_now <- 600 
    }
    
    hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)
    
    mij$lon <- Lon360to180(mij$lon)
    mij$bird_vel <- c(distHaversine(mij %>% dplyr::select(lon,lat))/(1000*hour_int),NA)
    mij$bird_dir <- geosphere::bearing(mij %>% dplyr::select(lon,lat))
  
    ddff <- uv2ddff(mij)
    mij$wind_vel <- ddff$ff # m/s
    mij$wind_dir <- ddff$dd # 360 degrees
    
    # NEED TO CHECK IF WIND_DIR AND BIRD_DIR BEING 360 AND 180 IS OK.
    
    # bird-wind angle
    mij$bwa <-abs(Lon360to180(mij$bird_dir-mij$wind_dir)) 
    
    # # # Loop Through Each Line in mii
    for (k in 1:length(mii$id)) {
      
      # -----------------------------------------------------------------------------
      # Calculate Wind Speed and Direction
      # -----------------------------------------------------------------------------
      ddff <- uv2ddff(mii$u[k],mii$v[k])
      mii$wind_vel[k] <- ddff$ff # m/s
      mii$wind_dir[k]<-ddff$dd # 360 degrees
      
      
      # -----------------------------------------------------------------------------
      # Calculate bird-wind-angle 
      # -----------------------------------------------------------------------------
      mii$bwa[k] <-abs(Lon360to180(mii$bird_dir[k]-mii$wind_dir[k])) 
      
      
      # -----------------------------------------------------------------------------
      # Classify bird-wind-angle Class
      # -----------------------------------------------------------------------------
      # Definitions from Spear and Ainley 1997
      # Headwind  = abs(bird-wind): 0-59 
      # Crosswind = abs(bird-wind): 60-119 
      # Tailwind =  abs(bird-wind): 120-180 
      
      if (is.na(mii$bwa[k])) {
        mii$bwa_class[k] <- NA
      }else{
        if (mii$bwa[k] < 50) {
          mii$bwa_class[k] <- "Head-Wind"
        }else if (mii$bwa[k] > 130) {
          mii$bwa_class[k] <- "Tail-Wind"
        }else{
          mii$bwa_class[k] <- "Cross-Wind"
        }
      }
    }
    
    
    # -----------------------------------------------------------------------------
    # Save Bird i , trip_i, 30-s dataset 
    # -----------------------------------------------------------------------------
    
    
    filename_chunk_id <- as.character(mii$tripID[1])
    filenamej<-paste0(dropdir, filename_chunk_id, '_30s_bwa.csv')
    write_csv(mii, filenamej)
    
    rm(list=ls()[! ls() %in% c("mi", "birdi", "trips","files", "wind_mat", "i", "j", "dropdir", "uv2ddff", "wrap360", "Lon360to180")])
    
  }
}

  












################################################################################
# Now Apply To 30-second dataset (to match behavior and HR scale)
################################################################################

# 1. Import bird i
# 2. Add wind to closest hour match
# 3. Interpolate u and v
# 4. Calculate Wind Speed and Direction
# 5. Using GPS info, calculate bird bearing
# 6. Calculate bird-wind-angle 
# 7. Classify bird-wind-angle Class
# 8. Calculate Bird Speed


dropdir <- '/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data/Conners_Analysis/Wind/wind_paired-data/2018-2019_BBAL/2021-Sep-Conners/d1_Dataset_30s_Wind/'

# -----------------------------------------------------------------------------
# Import Wind Mat
# -----------------------------------------------------------------------------
wind_mat<- read_csv('/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data/Conners_Analysis/Wind/wind_paired-data/BBAL_GHAL/2018-2019/2021-Sep-Conners/d0_Dataset_Hourly-Lat-Lon_with_Wind-U-V/allbirds_windcomponents.csv')

# If needed, adjust time column in windmat
library(lubridate)
wind_mat$datetime<-mdy_hm(wind_mat$datetime) # 2020-2021 data

# -----------------------------------------------------------------------------
# Loop Through 30-s Bird Lat Lon Mats
# -----------------------------------------------------------------------------
setwd(paste0('/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data/Conners_Bird_Island/2018_2019/BBAL_fromRAP/Tag_Data/L1/GPS_L1_3_interpolated/30s/'))
files<-list.files(pattern='*.csv') # GPS files 

for ( i in 2:length(files)) {
  # -----------------------------------------------------------------------------
  # Import bird i 
  # -----------------------------------------------------------------------------
  mi<-read.csv(files[i])
  birdi<-mi$id[1]
  
  # Loop through individual trips (most birds just have one.)
  trips<-unique(mi$tripID)
  
  for (j in 1:length(trips)) {
    
    mii <- mi[which(mi$tripID==trips[j]),]
    wind_mii <-wind_mat[wind_mat$id == birdi,]
    
    # -----------------------------------------------------------------------------
    # Match Closest Hour with U and V
    # -----------------------------------------------------------------------------
    mii$u <- NA
    mii$v <- NA
    mii$wind_vel <- NA
    mii$wind_dir <- NA # m/ s
    mii$bird_dir <-NA
    mii$bird_vel <-NA  # km/ hr
    mii$bwa <- NA
    mii$bwa_class <- NA
    
    
    hi_res_timematch<-as.numeric(as.POSIXct(as.character(mii$datetime)))
    for (j in 1:length(wind_mii$id)) {
      j_match<-as.numeric(as.POSIXct(as.character(wind_mii$datetime[j])))
      match_ix<-Closest( hi_res_timematch, j_match, which=TRUE) # Which in m_hi is nearest j_match (m$datetime[j])
      mii$u[match_ix] <- wind_mii$u[j]
      mii$v[match_ix] <- wind_mii$v[j]
    }
    rm(j)
    
    if (length(which(!is.na(mii$u)))<2) { # If file is so short that it's less than 2 hours , don't do anything.
      
    }else{
      # -----------------------------------------------------------------------------
      # Interpolate U and V
      # -----------------------------------------------------------------------------
      # Note this is a simple linear interpolation ... therefore, depending on analyses, something more rigorous may be needed. 
      mii$u<-na_interpolation(mii$u, option = "linear") # linear interpolation 
      mii$v<-na_interpolation(mii$v, option = "linear")
      
      # -----------------------------------------------------------------------------
      # Add Bird Speed, Bearing
      # -----------------------------------------------------------------------------
      int_now <- 30
      hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)
      
      for (k in 1:length(mii$id)-1) {
        distanceij_km<-swfscMisc::distance(mii$lat[k],mii$lon[k],mii$lat[k+1],mii$lon[k+1], units = "km", method = "haversine")
        mii$bird_vel[k]<-distanceij_km/hour_int
        mii$bird_dir[k]<-as.numeric(bearing(mii$lat[k],mii$lon[k],mii$lat[k+1],mii$lon[k+1])[1])
      }
      rm(k)
      
      
      # # # Loop Through Each Line in mii
      for (k in 1:length(mii$id)) {
        
        # -----------------------------------------------------------------------------
        # Calculate Wind Speed and Direction
        # -----------------------------------------------------------------------------
        ddff <- uv2ddff(mii$u[k],mii$v[k])
        mii$wind_vel[k] <- ddff$ff # m/s
        mii$wind_dir[k]<-ddff$dd # 360 degrees
        
        
        # -----------------------------------------------------------------------------
        # Calculate bird-wind-angle 
        # -----------------------------------------------------------------------------
        mii$bwa[k] <-abs(Lon360to180(mii$bird_dir[k]-mii$wind_dir[k])) 
        
        
        # -----------------------------------------------------------------------------
        # Classify bird-wind-angle Class
        # -----------------------------------------------------------------------------
        # Definitions from Spear and Ainley 1997
        # Headwind  = abs(bird-wind): 0-59 
        # Crosswind = abs(bird-wind): 60-119 
        # Tailwind =  abs(bird-wind): 120-180 
        
        if (is.na(mii$bwa[k])) {
          mii$bwa_class[k] <- NA
        }else{
          if (mii$bwa[k] < 50) {
            mii$bwa_class[k] <- "Head-Wind"
          }else if (mii$bwa[k] > 130) {
            mii$bwa_class[k] <- "Tail-Wind"
          }else{
            mii$bwa_class[k] <- "Cross-Wind"
          }
        }
      }
      
      
      # -----------------------------------------------------------------------------
      # Save Bird i , trip_i, 30-s dataset 
      # -----------------------------------------------------------------------------
      
      
      filename_chunk_id <- as.character(mii$tripID[1])
      filenamej<-paste0(dropdir, filename_chunk_id, '_30s_bwa.csv')
      write_csv(mii, filenamej)
      
      rm(list=ls()[! ls() %in% c("mi", "birdi", "trips","files", "wind_mat", "i", "j", "dropdir", "uv2ddff", "wrap360", "Lon360to180")])
      
    }
  }
  
  
  
  
  rm(list=ls()[! ls() %in% c("files", "wind_mat", "i", "dropdir", "uv2ddff", "wrap360", "Lon360to180")])
  
  
}









# -----------------------------------------------------------------
# uv2ddff
# -----------------------------------------------------------------
uv2ddff <- function(u, v = NULL, rad = FALSE){
  # if input u is zoo or data.frame
  zoo_index <- NULL # Default
  if (inherits(u, c("zoo", "data.frame"))) {
    # If input 'u' is zoo: keep index
    if (inherits(u, "zoo")) zoo_index <- index(u)
    if (!all(c("u", "v") %in% names(u)))
      stop("necessary colums \"u\" and/or \"v\" missing")
    # If "v" is set in addition: warn
    if (!is.null(v)) {
      warning(sprintf("input \"u\" to uv2ddff is \"%s\":", class(u)),
              "\"v\" specified as well but will be ignored!")
    }
    v = as.numeric(u$v)
    u = as.numeric(u$u)
    # if u has 2 columns the second column is taken as v
  } else if (NCOL(u) == 2) {
    v <- u[,2]
    u <- u[,1]
  } else {
    if (is.null(v)) stop("input \"v\" missing")
    # If lenths do not match and none of them is of length 1: stop.
    if (!(length(u) == length(v)) & !any(c(length(u), length(v)) == 1L)) {
      stop("Length of \"u\" and \"v\" not identical")
      # Else recycle the one with length one to the length of the other one.
      # Damn it, so much one's in one sentence!
    } else if (length(u) == 1) {
      u <- rep(u, length(v))
    } else if (length(v) == 1) {
      v <- rep(v, length(u))
    }
  }
  # polar coordinates:
  ff <- sqrt(u^2 + v^2)
  dd <- atan(v/u) + (u < 0) * pi
  # Only non-na combis
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # convert angle to meteorological convention
  dd <- 3 * pi / 2 - dd
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # if rad (radiants) = F we have to convert to degrees.
  if (!rad) dd <- dd * 180 / pi
  res <- data.frame(dd, ff)
  if (is.null(zoo_index)) return(res) else return(zoo(res, zoo_index))
}

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

