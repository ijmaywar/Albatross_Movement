################################################################################
#
# Compare times identified as on-water by movvar() of heave axis acc data to 
# times idenitifed as on-water by GLS.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szn = "2019_2020"

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
    
# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_L4_dir <- paste0(GD_dir,"L4/",location,"/Tag_Data/Acc/",szn,"/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Compare_GLS_OWB/",location,"/",szn,"/")
GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")

setwd(Acc_L4_dir)
Acc_files <- list.files(pattern='*.csv')

setwd(GLS_L1_dir)
GLS_files <- list.files(pattern='*.csv')

# Loop thru birds ---------------------------------------------------------
for (i in 1:length(GLS_files)) {
  
  m_GLS <- read_csv(GLS_files[i])
  m_GLS$starttime <- as.POSIXct(m_GLS$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_GLS$endtime <- as.POSIXct(m_GLS$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_GLS_wet <- m_GLS %>% filter(state=="wet")
  
  birdname <- str_sub(Acc_files[i],1,-16)
  
  m_OWB <- read_csv(paste0(Acc_L4_dir,Acc_files[i]))
  m_OWB$Start_water_dt <- as.POSIXct(m_OWB$Start_water_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_OWB$Stop_water_dt <- as.POSIXct(m_OWB$Stop_water_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  
  m_GLS_wet$duration_secs <- as.numeric(difftime(m_GLS_wet$endtime,m_GLS_wet$starttime,units="secs"))
  GLS_wet_duration_secs <- sum(m_GLS_wet$duration_secs)
  GLS_duration_secs <- as.numeric(difftime(m_GLS$endtime[nrow(m_GLS)],m_GLS_wet$starttime[1],units="secs"))
  
  m_OWB$duration_secs <- as.numeric(difftime(m_OWB$Stop_water_dt,m_OWB$Start_water_dt,units="secs"))
  OWB_duration_secs <- sum(m_OWB$duration_secs)
  # Acc_duration_secs <- as.numeric(difftime(m_GLS$endtime[nrow(m_GLS)],m_GLS_wet$starttime[1],units="secs"))
  
  intervals_m_GLS_wet <- interval(m_GLS_wet$starttime,m_GLS_wet$endtime)
  intervals_m_OWB <- interval(m_OWB$Start_water_dt,m_OWB$Stop_water_dt)
  
  overlaps <- intervals_m_GLS_wet %>% interval_overlaps(intervals_m_OWB)
  
  total_overlap_duration <- sum(as.duration(overlaps))
}