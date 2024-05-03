################################################################################
#
# Look at landing rates for birds that have GLS tags.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szns = c("2019_2020","2021_2022")

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
  write_dir <- paste0(GD_dir, "Analysis/Maywar/FNL_search/")
  GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
  
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
    
    m_GPS <- read_csv(paste0(GD_dir, "L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/",birdname,"_GPS_L1_2.csv"))
    m_GPS$datetime <- as.POSIXct(m_GPS$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
    GPS_startime <- m_GPS$datetime[1]
    GPS_endtime <- m_GPS$datetime[nrow(m_GPS)]
    
    m_GLS_wet <- m_GLS_wet %>% mutate(duration_sec = as.numeric(difftime(m_GLS_wet$endtime,m_GLS_wet$starttime,units="secs")))
    
    if (m_GLS_data_start < GPS_startime) {
      
    }
    
    # how do multiple trips play out? 
    
    
    for (int in int_dur_sec) {
      
      before_dt_colname <- paste0("before_dt_",as.character(int))  
      after_dt_colname <- paste0("after_dt_",as.character(int))
      flaps_before_NoOvr_colname <- paste0("flaps_before_",as.character(int),"_NoOvr")
      flaps_after_NoOvr_colname <- paste0("flaps_after_",as.character(int),"_NoOvr")
      flaps_before_colname <- paste0("flaps_before_",as.character(int))
      flaps_after_colname <- paste0("flaps_after_",as.character(int))
      
      # Find intervals before and after every GLS == wet bout 
      m_GLS_wet <- m_GLS_wet %>% mutate(!!before_dt_colname := m_GLS_wet$starttime - seconds(int))
      m_GLS_wet <- m_GLS_wet %>% mutate(!!after_dt_colname := m_GLS_wet$endtime + seconds(int))
      m_GLS_wet <- m_GLS_wet %>% mutate(!!flaps_before_NoOvr_colname := NA)
      m_GLS_wet <- m_GLS_wet %>% mutate(!!flaps_after_NoOvr_colname := NA)
      m_GLS_wet <- m_GLS_wet %>% mutate(!!flaps_before_colname := NA)
      m_GLS_wet <- m_GLS_wet %>% mutate(!!flaps_after_colname := NA)
      
      # Add NA values when acc isn't recording
      # Add NAs to durations before wet periods in which the Acc hasn't started recording data
      if (Acc_start_dt > m_GLS_wet[[1,before_dt_colname]]) {
        m_GLS_wet[1:(tail(which(Acc_start_dt > m_GLS_wet[[before_dt_colname]]),1)),sym(flaps_before_colname)] <- NA
        m_GLS_wet[1:(tail(which(Acc_start_dt > m_GLS_wet[[before_dt_colname]]),1)),sym(flaps_before_NoOvr_colname)] <- NA
      }
      
      # Add NAs to durations after wet periods in which the Acc hasn't started recording data
      if (Acc_start_dt > m_GLS_wet$endtime[1]) {
        m_GLS_wet[1:(tail(which(Acc_start_dt > m_GLS_wet$endtime),1)),sym(flaps_after_colname)] <- NA
        m_GLS_wet[1:(tail(which(Acc_start_dt > m_GLS_wet$endtime),1)),sym(flaps_after_NoOvr_colname)] <- NA
      }
      
      # Add NAs to durations before wet periods in which the Acc had already stopped recording data
      if (Acc_end_dt < m_GLS_wet$starttime[nrow(m_GLS_wet)]) {
        m_GLS_wet[(head(which(Acc_end_dt < m_GLS_wet$starttime),1)):nrow(m_GLS_wet),sym(flaps_before_colname)] <- NA
        m_GLS_wet[(head(which(Acc_end_dt < m_GLS_wet$starttime),1)):nrow(m_GLS_wet),sym(flaps_before_NoOvr_colname)] <- NA
      }
      
      # Add NAs to durations after wet periods in which the Acc had already stopped recording data
      if (Acc_end_dt < m_GLS_wet[[nrow(m_GLS_wet),after_dt_colname]]) {
        m_GLS_wet[(head(which(Acc_end_dt < m_GLS_wet[[after_dt_colname]]),1)):nrow(m_GLS_wet),sym(flaps_after_colname)] <- NA
        m_GLS_wet[(head(which(Acc_end_dt < m_GLS_wet[[after_dt_colname]]),1)):nrow(m_GLS_wet),sym(flaps_after_NoOvr_colname)] <- NA
      }
      
      m_GLS_wet[before_dt_colname] <- as.character(format(m_GLS_wet[[before_dt_colname]])) # safer for writing csv in character format  
      m_GLS_wet[after_dt_colname] <- as.character(format(m_GLS_wet[[after_dt_colname]]))
      
    }
    
    # Write csv file
    m_GLS_wet$starttime <- as.character(format(m_GLS_wet$starttime)) # safer for writing csv in character format  
    m_GLS_wet$endtime <- as.character(format(m_GLS_wet$endtime))
    
    write.csv(m_GLS_wet, file=paste0(write_dir,birdname,"_FNL_search.csv"), row.names=FALSE)
    
  }
}