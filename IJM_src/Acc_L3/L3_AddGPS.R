################################################################################
#
# Add GPS info to flaps
#
################################################################################

# Clear envrionment -------------------------------------------------------

rm(list = ls())
# rm(list = ls(all=TRUE))

# User Inputed Values -----------------------------------------------------

szn <- '2019_2020'
location <- 'Bird_Island' # Options: 'Bird_Island', 'Midway'
interval <- '600s'

# Libraries ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)

# User functions ----------------------------------------------------------

# Set Environment ---------------------------------------------------------
GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
GPS_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/",interval,"/")

# Import metdata
fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))
fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

# Find Acc files
setwd(Acc_dir)
AccFiles<-list.files(pattern='*.csv')

# Loop thru and process ---------------------------------------------------

for (i in 1:length(AccFiles)) {
  
  # Find dep_ID
  dep_ID <- str_sub(AccFiles[i],1,-12)
  Spp <- substr(dep_ID,1,4)
  
  # Load Acc data
  AccData <- read.csv(AccFiles[i])
  
  # Find matching L3_GPS file
  GPS_FileName <- paste0(GPS_dir,Spp,"/",dep_ID,"_GPS_L3_",interval,".csv")
  GPSData <- read.csv(GPS_FileName) 
  
  # Extract state 1 intervals
  GPS_1 <- GPSData %>% filter(state==1) %>% select(datetime,trip_ID)
  
  # # Find state for each flap
  # Acc_dt <- AccData$DateTime
  # Acc_states <- rep(2,length(Acc_dt))
  # GPS_row <- 1
  # for (Acc_row in 1:length(Acc_dt)) {
  #   while (difftime(Acc_dt[Acc_row],GPS_1$datetime[GPS_row], units="mins") >= 0)  {
  #     GPS_row <- GPS_row+1
  #   }
  #   
  #   if (GPS_row > 1) {
  #     GPS_row <- GPS_row-1
  #   }
  #   
  #   dt_lock_diff <- as.numeric(difftime(Acc_dt[Acc_row],GPS_1$datetime[GPS_row], units="mins"))
  #   if (dt_lock_diff >= 0 && dt_lock_diff < 10) {
  #     # flap occurs within 10 mins of nearest state 1
  #     Acc_states[Acc_row] <- 1
  #   }
  # }
  # 
  # AccData$state <- Acc_states
    
  # ALTERNATIVELY PASS THRU ENTIRE GPS DATA (and add lon, lat)
  Acc_dt <- AccData$DateTime
  Acc_states <- rep(0,length(Acc_dt))
  Acc_lon <- rep(0,length(Acc_dt))
  Acc_lat <- rep(0,length(Acc_dt))
  Acc_trip <- rep(0,length(Acc_dt))
  GPS_row <- 1
  for (Acc_row in 1:length(Acc_dt)) {
    while (difftime(Acc_dt[Acc_row],GPSData$datetime[GPS_row], units="mins") >= 0)  {
      GPS_row <- GPS_row+1
    }
    
    GPS_row <- GPS_row-1
    
    Acc_states[Acc_row] <- GPSData$state[GPS_row]
    Acc_lon[Acc_row] <- GPSData$lon[GPS_row]
    Acc_lat[Acc_row] <- GPSData$lat[GPS_row]
    Acc_trip[Acc_row] <- GPSData$trip_ID[GPS_row]
    
  }
  
  AccData$state <- Acc_states
  AccData$lon <- Acc_lon
  AccData$lat <- Acc_lat
  AccData$trip_ID <- Acc_trip
  
  Acc_filtered <- AccData %>% filter(state==2, trip_ID)
  # remove between trip rows.
  
  
  
  
  
  df$datetime <- as.character(format(bird_commuting_df$datetime)) # safer for writing csv in character format  
  





