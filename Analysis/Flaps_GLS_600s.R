################################################################################
#
# Combine flap data, GLS immersion state, and GPS data in 600s intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szn = "2021_2022"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/NEW_STRUCTURE/"
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")
GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
write_dir <- paste0(GD_dir, "Projects/Maywar/Flaps_ImmersionState_600s/GLS/",location,"/",szn,"/")

setwd(GLS_L1_dir)
GLS_files <- list.files(pattern='*.csv')

setwd(Acc_L3_dir)
Acc_files <- list.files(pattern='*.csv')

# Add Immersion data ---------------------------------------------------------------

for (i in 1:length(Acc_files)) {
  m <- read.csv(Acc_files[i])
  m$datetime <- as.POSIXct(m$datetime)
  
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
    print("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(Acc_files[i],1,-23)
  birdname <- str_sub(Acc_files[i],1,-25)
  birdspp <- str_sub(birdname,1,4)
  
  # Attach Immersion Data
  GLS_filename <- paste0(birdname,"_GLS_L1.csv")
  if (sum(GLS_files==GLS_filename)==1) {
    Immersion_data <- read.csv(paste0(GLS_L1_dir,GLS_filename))
    Immersion_data$starttime <- as.POSIXct(Immersion_data$starttime,format="%d-%b-%Y %H:%M:%S")
    Immersion_data$endtime <- as.POSIXct(Immersion_data$endtime,format="%d-%b-%Y %H:%M:%S")
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
    
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_GLS_600s.csv"), row.names=FALSE)
    
  }
}
