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

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
min_peak_prob = 0 # What was the min_peak_prob used to create 600s data?

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(lubridate)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
if (min_peak_prob == 0) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_0/",location,"/",szn,"/")
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG/p_0/",location,"/",szn,"/")
} else if (min_peak_prob == 0.85) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_085/",location,"/",szn,"/")
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG/p_085/",location,"/",szn,"/")
}

setwd(read_dir)
files <- list.files(pattern='*.csv')

# Loop thru and process  ---------------------------------------------------------------

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
  birdname_trip <- str_sub(files[i],1,-28)
  birdname <- str_sub(files[i],1,-30)
  birdspp <- str_sub(birdname,1,4)
  
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
    m_hourly$flaps[j] <- sum(current_m$flaps)
    m_hourly$Heartbeats[j] <- sum(current_m$Heartbeats)
    if (nrow(current_m) != 6) {
      m_hourly$trim[j] <- 1
    }
    if (any(current_m$GLS_state == "wet")) {
      m_hourly$GLS_state[j] <- "wet"
    }
  }
  
  m_hr_trim <- m_hourly %>% filter(trim == 0)
  m_hr_trim <- m_hr_trim %>% dplyr::select(-timediff,-trim,-rounded_hour)
  
  if (nrow(m_hr_trim>0)) {
    # If the trimmed file isn't empty, save it.
    m_hr_trim$datetime <- as.character(format(m_hr_trim$datetime)) # safer for writing csv in character format  
    write.csv(m_hr_trim, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_Hourly.csv"), row.names=FALSE)
  }
}


