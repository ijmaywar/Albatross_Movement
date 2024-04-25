################################################################################
#
# Look at flapping rates before and after bird is on water
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szns = c("2019_2020","2021_2022")
int_dur_sec <- 30

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
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_near_landings/",int_dur_sec,"s/",location,"/",szn,"/")
GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")

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
Acc_start_stop$Acc_end_dt <- as.POSIXct(Acc_start_stop$Acc_end_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")

setwd(Acc_dir)
Acc_files <- list.files(pattern='*.csv')

setwd(GLS_L1_dir)
GLS_files <- list.files(pattern='*.csv')

# Loop thru birds ---------------------------------------------------------
for (i in 1:length(GLS_files)) {
  
  m_GLS <- read_csv(GLS_files[i])
  m_GLS$starttime <- as.POSIXct(m_GLS$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_GLS$endtime <- as.POSIXct(m_GLS$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_GLS_data_start <- m_GLS$starttime[1]
  m_GLS_data_end <- m_GLS$endtime[nrow(m_GLS)]
  
  m_GLS_wet <- m_GLS %>% filter(state=="wet")
  
  birdname <- str_sub(GLS_files[i],1,-12)
  Acc_filename <- paste0(birdname,"_Acc_L2.csv")
  if (!Acc_filename %in% Acc_files) {
    next
  }
  
  m_Acc <- read_csv(paste0(Acc_dir,Acc_filename))
  m_Acc$DateTime <- as.POSIXct(m_Acc$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  m_GLS_wet <- m_GLS_wet %>% mutate(duration_sec = as.numeric(difftime(m_GLS_wet$endtime,m_GLS_wet$starttime,units="secs")))
  
  # Find intervals before and after every GLS == wet bout 
  m_GLS_wet$before_dt <- m_GLS_wet$starttime - seconds(int_dur_sec)
  m_GLS_wet$after_dt <- m_GLS_wet$endtime + seconds(int_dur_sec)
  
  m_GLS_wet$flaps_before <- NA
  m_GLS_wet$flaps_after <- NA
  
  for (j in 1:nrow(m_GLS_wet)) {
    m_GLS_wet$flaps_before[j] <- m_Acc %>%
      filter(DateTime >= m_GLS_wet$before_dt[j] & DateTime <= m_GLS_wet$starttime[j]) %>%
      nrow()
    
    m_GLS_wet$flaps_after[j] <- m_Acc %>%
      filter(DateTime >= m_GLS_wet$endtime[j] & DateTime <= m_GLS_wet$after_dt[j]) %>%
      nrow()
    
    if (j>1) {
      # Add NA values to durations before wet periods in which flaps are being counted during another wet period
      if (m_GLS_wet$before_dt[j] < m_GLS_wet$endtime[j-1]) {
        m_GLS_wet$flaps_before[j] <- NA
      }
    }
    
    if (j<nrow(m_GLS_wet)) {
      # Add NA values to durations after wet periods in which flaps are being counted during another wet period
      if (m_GLS_wet$after_dt[j] > m_GLS_wet$starttime[j+1]) {
        m_GLS_wet$flaps_after[j] <- NA
      }
    }

  }
  
  # Find start and end times for acc data
  m_GPS <- read_csv(paste0(GD_dir, "L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/",birdname,"_GPS_L1_2.csv"))
  m_GPS$datetime <- as.POSIXct(m_GPS$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  Acc_start_dt <- m_GPS$datetime[1]
  ASS_current <- Acc_start_stop %>% filter(Deployment_ID == birdname)
  if (nrow(ASS_current)==0) {
    next
  }
  Acc_end_dt <- ASS_current[["Acc_end_dt"]]
  if (is.na(Acc_end_dt)) {
    Acc_end_dt <- m_GPS$datetime[nrow(m_GPS)]
  }
  
  # Add NA values when acc isn't recording
  # Add NAs to durations before wet periods in which the Acc hasn't started recording data
  if (Acc_start_dt > m_GLS_wet$before_dt[1]) {
    m_GLS_wet$flaps_before[1:(tail(which(Acc_start_dt > m_GLS_wet$before_dt),1))] <- NA
  }
  
  # Add NAs to durations after wet periods in which the Acc hasn't started recording data
  if (Acc_start_dt > m_GLS_wet$endtime[1]) {
    m_GLS_wet$flaps_after[1:(tail(which(Acc_start_dt > m_GLS_wet$endtime),1))] <- NA
  }
  
  # Add NAs to durations before wet periods in which the Acc had already stopped recording data
  if (Acc_end_dt < m_GLS_wet$starttime[nrow(m_GLS_wet)]) {
    m_GLS_wet$flaps_before[(head(which(Acc_end_dt < m_GLS_wet$starttime),1)):nrow(m_GLS_wet)] <- NA
  }
  
  # Add NAs to durations after wet periods in which the Acc had already stopped recording data
  if (Acc_end_dt < m_GLS_wet$after_dt[nrow(m_GLS_wet)]) {
    m_GLS_wet$flaps_after[(head(which(Acc_end_dt < m_GLS_wet$after_dt),1)):nrow(m_GLS_wet)] <- NA
  }
  
  # Write csv file
  m_GLS_wet$starttime <- as.character(format(m_GLS_wet$starttime)) # safer for writing csv in character format  
  m_GLS_wet$endtime <- as.character(format(m_GLS_wet$endtime))
  m_GLS_wet$before_dt <- as.character(format(m_GLS_wet$before_dt))
  m_GLS_wet$after_dt <- as.character(format(m_GLS_wet$after_dt))
  write.csv(m_GLS_wet, file=paste0(write_dir,birdname,"_Flaps_near_landings_",int_dur_sec,"s",".csv"), row.names=FALSE)
  
}
}