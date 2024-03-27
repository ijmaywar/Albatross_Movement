################################################################################
#
# Combine flap, heartbeat data, and GPS data in 600s intervals.
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
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")
ECG_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/ECG/ECG_NRL/",szn,"/")
write_dir <- paste0(GD_dir, "Projects/Maywar/Flaps_600s/ECG/",location,"/",szn,"/")

setwd(Acc_L3_dir)
Acc_files <- list.files(pattern='*.csv')
Acc_files_depID <- str_sub(Acc_files,1,-25)

setwd(ECG_L1_dir)
ECG_files <- list.files(pattern='*.csv')

# Add Heartbeat Data ---------------------------------------------------------------

for (i in 1:length(ECG_files)) {
  HR_data <- read_csv(ECG_files[i])
  HR_data$DateTime <- as.POSIXct(HR_data$DateTime)
  
  birdname <- str_sub(ECG_files[i],1,-8)
  
  find_Acc_file <- 
  m <- read.csv(paste0(Acc_L3_dir,rdspp,"/",HMM_filename))
  
  
  
  
  
  
}





################################################################################
# Load files
setwd(Sensor_L0_dir)
sensor_files <- list.files(pattern='*.txt')

setwd(HRL_L0_dir)
hrl_files<-list.files(pattern='*.txt')
nfiles <- length(hrl_files)
timing_df <- data.frame(dep_ID=vector("character",length=nfiles),timing=vector("character",length=nfiles))

for (i in 1:nfiles) {
  namesplit <- strsplit(hrl_files[i],"_")[[1]]
  
  Acc_ID <- paste(namesplit[1],namesplit[2],namesplit[3],sep="_")
  birdmeta <- fullmeta %>% filter(Acc_OG_ID == Acc_ID)
  if (nrow(birdmeta)==0){
    print("Cannot find bird in metadata.")
    break
  }
  
  Sensor_ID <- paste(namesplit[1],namesplit[2],namesplit[3],namesplit[4],namesplit[5],"SensorData.txt",sep="_")
  setwd(Sensor_L0_dir)
  sensor_data <- read.table(Sensor_ID, header=TRUE, sep = ",")
  
  setwd(HRL_L0_dir)
  hrl_data <- read.table(hrl_files[i],header = TRUE, sep = "\t")
  
  # Build datetime column
  start_dt <- as.POSIXct(paste(birdmeta$AxyON_date_yyyymmdd,birdmeta$AxyON_time_hhmmss), format = "%Y%m%d %H%M", tz = "GMT")
  dt_col <- seq(from = start_dt, by = 1/sample_freq, length.out = length(raw$EEG))
  dt_col_exact <- seq(from = start_dt, by = 1/sample_freq_exact, length.out = length(raw$EEG))
  
  final_dt <- tail(dt_col, n = 1)
  final_dt_exact <- tail(dt_col_exact, n = 1)
  RecaptureDateTime <- as.POSIXct(paste(birdmeta$Recapture_Date_yyyymmdd,birdmeta$Recapture_Time_hhmm), format = "%Y%m%d %H%M", tz = "GMT")
  time_short <- RecaptureDateTime - final_dt
  time_short_exact <- RecaptureDateTime - final_dt_exact
  
  timing_df$dep_ID[i] <- birdmeta$Deployment_ID
  timing_df$timing[i] <- time_short
  }
