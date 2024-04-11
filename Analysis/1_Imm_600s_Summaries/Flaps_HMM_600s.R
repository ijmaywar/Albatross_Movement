################################################################################
#
# Combine flap data, HMM immersion state, and GPS data in 600s intervals.
#
################################################################################

# Use Command, Alt/Options, R to run the whole code

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2022_2023"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")
GPS_3S_L3_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/3_states/")
GPS_2S_L3_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/2_states/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM/",location,"/",szn,"/")

setwd(Acc_L3_dir)
Acc_files <- list.files(pattern='*.csv')

# Add Immersion Data ---------------------------------------------------------------

for (i in 1:length(Acc_files)) {
  m <- read.csv(Acc_files[i])
  m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
  
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
    print("Data continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(Acc_files[i],1,-23)
  birdname <- str_sub(Acc_files[i],1,-25)
  birdspp <- str_sub(birdname,1,4)
  
  # Attach HMM 3S state
  HMM_3S_filename <- paste0(birdname,"_GPS_L3_600s.csv")
  HMM_3S_data <- read.csv(paste0(GPS_3S_L3_dir,birdspp,"/",HMM_3S_filename))
  HMM_3S_data <- HMM_3S_data %>% filter(trip_ID==birdname_trip)
  m$HMM_3S_state <- HMM_3S_data$state
  
  # Attach HMM 2S state
  HMM_2S_filename <- paste0(birdname,"_GPS_L3_600s.csv")
  HMM_2S_data <- read.csv(paste0(GPS_2S_L3_dir,birdspp,"/",HMM_2S_filename))
  HMM_2S_data <- HMM_2S_data %>% filter(trip_ID==birdname_trip)
  m$HMM_2S_state <- HMM_2S_data$state
  
  m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
  write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_600s.csv"), row.names=FALSE)
  
}
