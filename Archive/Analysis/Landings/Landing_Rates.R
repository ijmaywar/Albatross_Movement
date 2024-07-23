################################################################################
#
# Look at landing rates for birds that have GLS tags.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szns = c("2019_2020","2021_2022")

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Loop thru all samples -----------------------------------------------------------
for (szn in szns) {
  cat("Processing location:",location,"Season:",szn,"\n")
  
  # Set Environment ---------------------------------------------------------
  
  GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Landing_Rates/")
  GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
  GPS_L1_2_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/")
  
  setwd(GPS_L1_2_dir)
  GPS_files <- list.files(pattern='*.csv')
  
  setwd(GLS_L1_dir)
  GLS_files <- list.files(pattern='*.csv')
  
  n_GLS_files <- length(GLS_files)
  m_LandingRates <- data.frame(Deployment_ID = character(n_GLS_files),
                               Num_Landings = numeric(n_GLS_files),
                               Dur_Landings_hrs = numeric(n_GLS_files),
                               GLS_duration_hrs = numeric(n_GLS_files))

  # Loop thru birds ---------------------------------------------------------
  for (i in 1:n_GLS_files) {
    m_GLS <- read_csv(GLS_files[i])
    m_GLS$starttime <- as.POSIXct(m_GLS$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
    m_GLS$endtime <- as.POSIXct(m_GLS$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
    m_GLS_data_start <- m_GLS$starttime[1]
    m_GLS_data_end <- m_GLS$endtime[nrow(m_GLS)]
    
    birdname <- str_sub(GLS_files[i],1,-12)
    
    GPSname <- paste0(birdname,"_GPS_L1_2.csv")
    if (!GPSname %in% GPS_files) {
      next
    }
    
    m_GPS <- read_csv(paste0(GPS_L1_2_dir,GPSname))
    m_GPS$datetime <- as.POSIXct(m_GPS$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    m_GPS_starttime <- m_GPS$datetime[1]
    m_GPS_endtime <- m_GPS$datetime[nrow(m_GPS)]
    
    if (m_GLS_data_start > m_GPS_starttime) {
      disp("GLS data starts after the bird is starting its first trip")
      break
    }
    
    if (m_GLS_data_end < m_GPS_endtime) {
      disp("GLS data ends before bird finishes its last trip")
      break
    }
    
    # Parse thru trips
    current_trips <- as.factor(unique(m_GPS$tripID))
    for (trip in current_trips) {
      trip_m_GPS <- m_GPS %>% filter(tripID == trip)
      if (trip == current_trips[1]) {
        GPS_dur_hrs <- as.numeric(difftime(trip_m_GPS$datetime[nrow(trip_m_GPS)],
                                        trip_m_GPS$datetime[1],units="hours"))
      } else {
        GPS_dur_hrs <- GPS_dur_hrs + as.numeric(difftime(trip_m_GPS$datetime[nrow(trip_m_GPS)],
                                                   trip_m_GPS$datetime[1],units="hours"))
      }
    }
    
    m_GLS_trimmed <- m_GLS
    GLS_start_row <- tail(which(m_GLS$starttime <= m_GPS_starttime),1)
    m_GLS_trimmed$starttime[GLS_start_row] <- m_GPS_starttime
    GLS_end_row <- head(which(m_GLS$endtime >= m_GPS_endtime),1)
    m_GLS_trimmed$endtime[GLS_end_row] <- m_GPS_endtime
    m_GLS_trimmed <- m_GLS_trimmed[GLS_start_row:GLS_end_row,]
    
    m_GLS_trimmed_wet <- m_GLS_trimmed %>% filter(state=="wet")
    m_GLS_trimmed_wet <- m_GLS_trimmed_wet %>% mutate(duration_sec = as.numeric(
                         difftime(m_GLS_trimmed_wet$endtime,m_GLS_trimmed_wet$starttime,units="secs")))
    
    m_LandingRates$Deployment_ID[i] <- birdname
    m_LandingRates$Num_Landings[i] <- nrow(m_GLS_trimmed_wet)
    m_LandingRates$Dur_Landings_hrs[i] <- sum(m_GLS_trimmed_wet$duration_sec)/(60)
    m_LandingRates$GLS_duration_hrs[i] <- GPS_dur_hrs
    
  }
  
  if (szn == szns[1]) {
    all_m_LandingRates <- m_LandingRates
  } else {
    all_m_LandingRates <- rbind(all_m_LandingRates,m_LandingRates)
  }
}

# Write csv file
write.csv(all_m_LandingRates, file=paste0(write_dir,birdname,"_FNL_search.csv"), row.names=FALSE)
