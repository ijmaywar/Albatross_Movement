################################################################################
#
# Combine L2 Wind data with L2 Acc data to count the number of flaps within 
# 10-minute GPS intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2022_2023"

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(readr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/ERA5_SingleLevels_10m/",szn,"/")
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")

# Load Acc_start_stop files
Acc_start_stop_list <- list.files(paste0(GD_dir,"Analysis/Maywar/Acc_start_stop/"),full.names = TRUE)
Acc_start_stop_list <- Acc_start_stop_list[grepl(location,Acc_start_stop_list) & grepl(szn,Acc_start_stop_list)]
if (length(Acc_start_stop_list)==2) {
  ASS_1 <- read_csv(Acc_start_stop_list[1], col_types = cols(Deployment_ID=col_character(),
                                                             start_delay=col_character(),
                                                             stop_early=col_character()))
  ASS_2 <- read_csv(Acc_start_stop_list[2], col_types = cols(Deployment_ID=col_character(),
                                                             start_delay=col_character(),
                                                             stop_early=col_character()))
  Acc_start_stop <- rbind(ASS_1,ASS_2)
} else if (length(Acc_start_stop_list)==1) {
  Acc_start_stop <- read_csv(Acc_start_stop_list)
}

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

setwd(wind_L2_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_bwa.csv$","",files)

# Add flaps ---------------------------------------------------------------

for (i in 1:length(files)) {
  
  m <- read.csv(files[i])
  m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
 
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
    print("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(files[i],1,-9)
  birdname <- str_sub(files[i],1,-11)
  birdspp <- str_sub(birdname,1,4)
  
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
  L1_acc_filepath <- paste0(GD_dir,"L1/",location,"/Tag_Data/Acc/",birdmeta$Aux_TagCat,"/",szn,"/",birdname,"_Acc_L1.csv")
  
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
    
    # The last row of data isn't a full 10 minutes
    # The GPS ends before the final datetime + 10 minutes
    # So we cannot summarize flaps during this time
    data$Freq[nrow(data)] <- NA
    
    # Edit data so that values are NA for when the Acc isn't on/isn't being read.
    ASS_current <- Acc_start_stop %>% filter(Deployment_ID == birdname)
    Acc_end_dt <- ASS_current[["Acc_end_dt"]]
    if (Acc_end_dt!="NaT") {
      Acc_end_dt <- as.POSIXct(Acc_end_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")
      last_acc_sec <- as.numeric(difftime(Acc_end_dt,m$datetime[1],units='secs'))
      last_break <- findInterval(last_acc_sec,breaks)
      if (last_break<length(breaks)) {  
        data$Freq[last_break:nrow(data)] <- NA
      }
    }
    
    m$flaps <- data$Freq
  
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(Acc_L3_dir,birdname_trip,"_Acc_L3_wind_10min.csv"), row.names=FALSE)
  
  }
}
