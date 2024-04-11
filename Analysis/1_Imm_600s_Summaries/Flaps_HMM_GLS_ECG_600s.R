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
GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
ECG_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/ECG/ECG_NRL/",szn,"/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/",location,"/",szn,"/")

setwd(GLS_L1_dir)
GLS_files <- list.files(pattern='*.csv')

setwd(ECG_L1_dir)
ECG_files <- list.files(pattern='*.csv')

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
    Immersion_data <- read.csv(paste0(GLS_L1_dir,GLS_filename))
    Immersion_data$starttime <- as.POSIXct(Immersion_data$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
    Immersion_data$endtime <- as.POSIXct(Immersion_data$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
    Immersion_data_wet <- Immersion_data %>% filter(state=="wet")
    
    # Attach GLS state
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
  
  if (sum(ECG_files==ECG_filename)==0) {
    m$Heartbeats <- NA
  } else if (sum(ECG_files==ECG_filename)>1) {
    disp("More than 1 ECG file.")
    break
  } else {
    # Attach ECG data
    ECG_data <- read.csv(paste0(ECG_L1_dir,"/",ECG_filename))
    
    # Filter for HBs with a probability geq(0.85)
    ECG_data <- ECG_data %>% filter(probability>=min_peak_prob)
    
    # find the seconds passed after the first GPS measurement for all flaps
    ECG_data$GPSsec <- as.numeric(difftime(ECG_data$DateTime,m$datetime[1],units='secs'))
    
    breaks <- seq(0,600*nrow(m),by=600)
    categories <- cut(ECG_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
    frequency_table <- table(categories)
    data <- as.data.frame(frequency_table)
    m$Heartbeats <- data$Freq
  }
  
  # Save m
  m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
  write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_600s.csv"), row.names=FALSE)
}




################################################################################
# Load files
# setwd(Sensor_L0_dir)
# sensor_files <- list.files(pattern='*.txt')
# 
# setwd(HRL_L0_dir)
# hrl_files<-list.files(pattern='*.txt')
# nfiles <- length(hrl_files)
# timing_df <- data.frame(dep_ID=vector("character",length=nfiles),timing=vector("character",length=nfiles))
# 
# for (i in 1:nfiles) {
#   namesplit <- strsplit(hrl_files[i],"_")[[1]]
#   
#   Analysis_ID <- paste(namesplit[1],namesplit[2],namesplit[3],sep="_")
#   birdmeta <- fullmeta %>% filter(Analysis_OG_ID == Analysis_ID)
#   if (nrow(birdmeta)==0){
#     print("Cannot find bird in metadata.")
#     break
#   }
#   
#   Sensor_ID <- paste(namesplit[1],namesplit[2],namesplit[3],namesplit[4],namesplit[5],"SensorData.txt",sep="_")
#   setwd(Sensor_L0_dir)
#   sensor_data <- read.table(Sensor_ID, header=TRUE, sep = ",")
#   
#   setwd(HRL_L0_dir)
#   hrl_data <- read.table(hrl_files[i],header = TRUE, sep = "\t")
#   
#   # Build datetime column
#   start_dt <- as.POSIXct(paste(birdmeta$AxyON_date_yyyymmdd,birdmeta$AxyON_time_hhmmss), format = "%Y%m%d %H%M", tz = "GMT")
#   dt_col <- seq(from = start_dt, by = 1/sample_freq, length.out = length(raw$EEG))
#   dt_col_exact <- seq(from = start_dt, by = 1/sample_freq_exact, length.out = length(raw$EEG))
#   
#   final_dt <- tail(dt_col, n = 1)
#   final_dt_exact <- tail(dt_col_exact, n = 1)
#   RecaptureDateTime <- as.POSIXct(paste(birdmeta$Recapture_Date_yyyymmdd,birdmeta$Recapture_Time_hhmm), format = "%Y%m%d %H%M", tz = "GMT")
#   time_short <- RecaptureDateTime - final_dt
#   time_short_exact <- RecaptureDateTime - final_dt_exact
#   
#   timing_df$dep_ID[i] <- birdmeta$Deployment_ID
#   timing_df$timing[i] <- time_short
#   }
