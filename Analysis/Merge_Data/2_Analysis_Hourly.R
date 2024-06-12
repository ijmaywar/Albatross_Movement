################################################################################
#
# Wind data: Take 600s Acc data and summarize it hourly. This code removes hours
# in which there are less than 6 measurements taken. There should be 6 measurements
# in every hour because the data is read at 600s(10 min) intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

# location = 'Bird_Island'
# szn = "2021_2022"

locations = c("Bird_Island", "Midway")

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

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(lubridate)
library(purrr)
library(geosphere)

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
read_dir <- paste0(GD_dir, "Analysis/Maywar/Merged_Data/Merged_600s/",location,"/",szn,"/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Merged_Data/Merged_Hourly/",location,"/",szn,"/")

setwd(read_dir)
files <- list.files(pattern='*.csv')

# Loop thru and process  ---------------------------------------------------------------

for (i in 1:length(files)) {
  m <- read_csv(files[i])
  m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
  
  birdname_trip <- paste(unlist(str_split(files[i],"_"))[1:4],collapse="_")
  birdname <- str_sub(birdname_trip,1,-3)
  
  m$rounded_hour <- round_date(m$datetime, unit = "hour")
  m$timediff <- abs(as.numeric(difftime(m$datetime,m$rounded_hour)))
  
  m_hourly <- m %>% filter(timediff==min(timediff))
  m_hourly$trim <- 0
  
  # Remove all rows that have less than 6 measurements within that given hour
  # and sum flaps and heartbeats
  # and set GLS_state to wet if any of the 6 rows within an hour are wet.
  # and remove rows that have less than 6 measurements of GLS/ECG within that given hour
  for (j in 1:nrow(m_hourly)) {
    current_hour <- m_hourly$rounded_hour[j]
    current_m <- m %>% filter(rounded_hour == current_hour)
    
    if (nrow(current_m) != 6) {
      m_hourly$trim[j] <- 1
    } else {
      
      # If any of the flaps are NA, the hour summary needs to also be NA
      if (any(is.na(current_m$flaps))) {
        m_hourly$flaps[j] <- NA
      } else {
        m_hourly$flaps[j] <- sum(current_m$flaps)
      }
      
      # # IF any of the OWBs are NA, the hour summary needs to also be NA
      # if (any(is.na(current_m$OWB_state))) {
      #   m_hourly$OWB_state[j] <- NA
      # } else if (any(current_m$OWB_state == "on")) {
      #   # If the bird is on-water at all during the hour, the OWB_state is on-water
      #   m_hourly$OWB_state[j] <- "on"
      # }
      
      # If any of the heartbeats are NA, the hour summary needs to also be NA
      if (any(is.na(current_m$Heartbeats))) {
        m_hourly$Heartbeats[j] <- NA
      } else {
        m_hourly$Heartbeats[j] <- sum(current_m$Heartbeats)
      }
      
      # If any of the GLS are NA, the hour summary needs to also be NA
      if (any(is.na(current_m$GLS_state))) {
        m_hourly$GLS_state[j] <- NA
      } else if (any(current_m$GLS_state == "wet")) {
        # If the bird is wet at all during the hour, the GLS_state is wet
        m_hourly$GLS_state[j] <- "wet"
      }
    }
  }
  
  # Re-calculate bird_dir, bird_vel, BWAs
  for (trip in unique(m_hourly$tripID)) {
    # Bird stats
    m_hourly$bird_vel <- c(distHaversine(m_hourly %>% dplyr::select(lon,lat))/1000,NA)
    m_hourly$bird_dir <- wrap360(geosphere::bearing(m_hourly %>% dplyr::select(lon,lat))) # set bearing to [0,360)
    
    # Wind stats
    m_hourly$bird_wind_angle <- bearingAngle(m_hourly$bird_dir,wrap360(m_hourly$wind_dir-180)) # [0,180) degrees
    m_hourly$wind_rel <- (wrap360(m_hourly$wind_dir-180)-m_hourly$bird_dir) %% 360
    
    # Wave stats
    m_hourly$bird_wave_angle <- bearingAngle(m_hourly$bird_dir,wrap360(m_hourly$mwd-180)) # [0,180) degrees
    m_hourly$wave_rel <- (wrap360(m_hourly$mwd-180)-m_hourly$bird_dir) %% 360
    m_hourly$bird_swell_angle <- bearingAngle(m_hourly$bird_dir,wrap360(m_hourly$mdts-180)) # [0,180) degrees
    m_hourly$swell_rel <- (wrap360(m_hourly$mdts-180)-m_hourly$bird_dir) %% 360
    m_hourly$bird_ww_angle <- bearingAngle(m_hourly$bird_dir,wrap360(m_hourly$mdww-180)) # [0,180) degrees
    m_hourly$ww_rel <- (wrap360(m_hourly$mdww-180)-m_hourly$bird_dir) %% 360
  }
  
  m_hr_trim <- m_hourly %>% filter(trim == 0)
  m_hr_trim <- m_hr_trim %>% dplyr::select(-timediff,-trim,-rounded_hour)
  
  if (nrow(m_hr_trim>0)) {
    # If the trimmed file isn't empty, save it.
    m_hr_trim$datetime <- as.character(format(m_hr_trim$datetime)) # safer for writing csv in character format  
    write.csv(m_hr_trim, file=paste0(write_dir,birdname_trip,"_Analysis_Hourly.csv"), row.names=FALSE)
  }
}
}
}
