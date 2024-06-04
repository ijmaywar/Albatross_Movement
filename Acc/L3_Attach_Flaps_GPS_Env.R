################################################################################
#
# Combine L2 Env data with L2 Acc data and count the number of 
# flaps within 10-minute GPS intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

# location = 'Midway' # Options: 'Bird_Island', 'Midway'
# szn = "2018_2019"

locations = c("Bird_Island", "Midway")

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(readr)

# Loop thru all samples -----------------------------------------------------------
for (location in locations) {
  if (location == "Bird_Island") {
    # szns = c("2019_2020", "2020_2021", "2021_2022")
    szns = c("2020_2021", "2021_2022")
  } else if (location == "Midway") {
    # szns = c("2018_2019", "2021_2022", "2022_2023")
    szns = c("2021_2022", "2022_2023")
  }
  for (szn in szns) {
    cat("Processing location:",location,"Season:",szn,"\n")

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
env_L2_dir <- paste0(GD_dir,"L2/",location,"/Env_Data/ERA5_SingleLevels_10m/",szn,"/")
wave_L2_dir <- paste0(GD_dir,"L2/",location,"/Wave_Data/ERA5_SingleLevels_10m/",szn,"/")
write_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

setwd(env_L2_dir)
env_files <- list.files(pattern='*.csv')
all_trips <- sub("_BWAs.csv$","",env_files)

# Add flaps ---------------------------------------------------------------

for (i in 1:length(env_files)) {
  
  m <- read.csv(env_files[i])
  m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
 
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
    print("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(env_files[i],1,-10)
  birdname <- str_sub(env_files[i],1,-12)
  birdspp <- str_sub(birdname,1,4)
  
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)

  # Attach number of flaps
  acc_filename <- paste0(birdname,"_Acc_L2.csv")
  if (sum(acc_files==acc_filename)==1) {
    flap_data <- read.csv(paste0(Acc_dir,acc_filename))
    flap_data$DateTime <- as.POSIXct(flap_data$DateTime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
  
    # find the seconds passed after the first GPS measurement for all flaps
    flap_data$GPSsec <- as.numeric(difftime(flap_data$DateTime,m$datetime[1],units='secs'))
    
    breaks <- seq(0,600*nrow(m),by=600)
    categories <- cut(flap_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
    frequency_table <- table(categories)
    data <- as.data.frame(frequency_table)
    
    # Edit data so that values are NA for when the Acc isn't on/isn't being read.
    Acc_end_dt <- as.POSIXct(birdmeta$Acc_L1_Stop,format="%m/%d/%y %H:%M:%S", tz="GMT")
    last_acc_sec <- as.numeric(difftime(Acc_end_dt,m$datetime[1],units='secs'))
    last_break <- findInterval(last_acc_sec,breaks)
    if (last_break < length(breaks)) {  
      data$Freq[last_break:nrow(data)] <- NA
    }
    
    m$flaps <- data$Freq
  
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(write_dir,birdname_trip,"_Acc_L3_env_10min.csv"), row.names=FALSE)
  
  }
}
}
}
