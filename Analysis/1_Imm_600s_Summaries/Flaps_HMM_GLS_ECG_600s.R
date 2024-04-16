################################################################################
#
# Add heartbeat ECG data to Flap data, HMM states, and GLS states in 600s intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szn = "2021_2022"
min_peak_prob = 0.85 # All peaks with a probability less than this value will be removed

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Analysis_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM/",location,"/",szn,"/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/",location,"/",szn,"/")

# Copy files from Flaps_HMM for Midway birds because there are no GLS or ECG deployments
if (location == 'Midway') {
  setwd(Analysis_dir)
  Analysis_files <- list.files(pattern='*.csv')
  for (i in 1:length(Analysis_files)) {
    m <- read_csv(Analysis_files[i])
    m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
    
    birdname_trip <- str_sub(Analysis_files[i],1,-20)
    
    m$GLS_state <- NA
    m$Heartbeats <- NA
    
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_600s.csv"), row.names=FALSE)
  }
} else {
  
  # For Bird_Island birds
  
  GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
  setwd(GLS_L1_dir)
  GLS_files <- list.files(pattern='*.csv')
  
  if (szn == "2019_2020" | szn == "2021_2022") {
    ECG_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/ECG/ECG_NRL/",szn,"/")
    setwd(ECG_L1_dir)
    ECG_files <- list.files(pattern='*.csv')
  }
  
  setwd(Analysis_dir)
  Analysis_files <- list.files(pattern='*.csv')
  
  # Add Heartbeat Data ---------------------------------------------------------------
  
  for (i in 1:length(Analysis_files)) {
    m <- read_csv(Analysis_files[i])
    m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
    
    if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
      print("Data continuity check failed.")
      break
    }
    
    birdname <- str_sub(Analysis_files[i],1,-22)
    birdname_trip <- str_sub(Analysis_files[i],1,-20)
    
    GLS_filename <- paste0(birdname,"_GLS_L1.csv")
    ECG_filename <- paste0(birdname,"_ECG_L1.csv")
    
    if (sum(GLS_files==GLS_filename)==0) {
      m$GLS_state <- NA
    } else if (sum(GLS_files==GLS_filename)>1) {
      disp("More than 1 GLS file.")
      break
    } else {
      # Attach GLS data
      Immersion_data <- read_csv(paste0(GLS_L1_dir,GLS_filename))
      Immersion_data$starttime <- as.POSIXct(Immersion_data$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
      Immersion_data$endtime <- as.POSIXct(Immersion_data$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
      Immersion_data_wet <- Immersion_data %>% filter(state=="wet")
      
      m$GLS_state <- "dry"
      for (j in 1:nrow(m)) {
        hour_start <- m$datetime[j]
        Immersion_data_wet <- Immersion_data_wet %>% mutate(starttime_diff_to_hour = as.numeric(difftime(starttime,hour_start,units="hours")))
        if (any(Immersion_data_wet$starttime_diff_to_hour>=0 & Immersion_data_wet$starttime_diff_to_hour<1)) {
          # If a starttime occurs within an hour of the current hour_start, the bird is wet during this hour.
          m$GLS_state[j] <- "wet"
        } else {
          # If an endtime occurs within an hour of the current hour_start, the bird is wet during this hour.
          Immersion_data_wet <- Immersion_data_wet %>% mutate(endtime_diff_to_hour = as.numeric(difftime(endtime,hour_start,units="hours")))
          if (any(Immersion_data_wet$endtime_diff_to_hour>=0 & Immersion_data_wet$endtime_diff_to_hour<1)) {
            m$GLS_state[j] <- "wet"          
          }
        }
      }
    }
    
    # Attach ECG data
    if (szn == "2019_2020" | szn == "2021_2022") {
      if (sum(ECG_files==ECG_filename)==0) {
        m$Heartbeats <- NA
      } else if (sum(ECG_files==ECG_filename)>1) {
        disp("More than 1 ECG file.")
        break
      } else {
        # Attach ECG data
        ECG_data <- read_csv(paste0(ECG_L1_dir,"/",ECG_filename))
        ECG_data$DateTime <- as.POSIXct(ECG_data$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
        
        # find the seconds passed after the first GPS measurement for all flaps
        ECG_data$GPSsec <- as.numeric(difftime(ECG_data$DateTime,m$datetime[1],units='secs'))
        
        # Find the min and max GPSsec
        ECG_sample_first_sec <- min(ECG_data$GPSsec)
        ECG_sample_last_sec <- max(ECG_data$GPSsec)
        
        # Filter for HBs with a probability geq(0.85)
        ECG_data <- ECG_data %>% filter(probability>=min_peak_prob)
        
        breaks <- seq(0,600*nrow(m),by=600)
        categories <- cut(ECG_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
        frequency_table <- table(categories)
        data <- as.data.frame(frequency_table)
        
        # Edit data so that values are NA for when the NRL isn't on/isn't being read.
        first_break <- findInterval(ECG_sample_first_sec,breaks)
        if (first_break!=0) {
          data$Freq[1:first_break] <- NA
        }
        
        last_break <- findInterval(ECG_sample_last_sec,breaks)
        if (last_break<=nrow(m)) {
          data$Freq[last_break:nrow(data)] <- NA
        }
        
        # Add data to 600s summary
        m$Heartbeats <- data$Freq
      }
    } else {
      m$Heartbeats <- NA
    }
    
    # Save m
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_600s.csv"), row.names=FALSE)
  }
}
