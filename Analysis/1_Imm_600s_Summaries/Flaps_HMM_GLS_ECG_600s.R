################################################################################
#
# Add heartbeat ECG data to Flap data, HMM states, and GLS states in 600s intervals.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island'
szn = "2021_2022"
min_peak_prob = 0 # All heartbeats (ECG) with a probability less than this value will be removed

# User defined functions ---------------------------------------------------

HMMstate <- function(m,birdname_trip,states) {
  dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/",states,"_states/")
  HMM_filename <- paste0(str_sub(birdname_trip,end=-3),"_GPS_L3_600s.csv")
  HMM_data <- read.csv(paste0(dir,str_sub(birdname_trip,1,4),"/",HMM_filename))
  HMM_data <- HMM_data %>% filter(trip_ID==birdname_trip)
  return(HMM_data$state)
}

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")
if (min_peak_prob == 0) {
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_0/",location,"/",szn,"/")
} else if (min_peak_prob == 0.85) {
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_085/",location,"/",szn,"/")
}

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(Acc_L3_dir)
Acc_files <- list.files(pattern='*.csv')

# Add Immersion Data ---------------------------------------------------------------
if (location == 'Midway') {
  for (i in 1:length(Acc_files)) {
      m <- read_csv(Acc_files[i])
      m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
      
      if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
        print("Data continuity check failed.")
        break
      }
      
      birdname_trip <- str_sub(Acc_files[i],1,-23)
      birdname <- str_sub(Acc_files[i],1,-25)
      birdspp <- str_sub(birdname,1,4)
      
      m$HMM_2S_state <- HMMstate(m,birdname_trip,states=2)
      m$HMM_3S_state <- HMMstate(m,birdname_trip,states=3)
      
      m$GLS_state <- NA
      m$Heartbeats <- NA
      
      m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
      write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_600s.csv"), row.names=FALSE)
    } 
  } else if (location == "Bird_Island") {

    GLS_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
    setwd(GLS_L1_dir)
    GLS_files <- list.files(pattern='*.csv')
    
    if (szn == "2019_2020" | szn == "2021_2022") {
      ECG_L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/ECG/ECG_NRL/",szn,"/")
      setwd(ECG_L1_dir)
      ECG_files <- list.files(pattern='*.csv')
      ECG_start_stop <- read_csv(paste0(GD_dir,'L0/',location,'/Tag_Data/',szn,'/Aux/NRL/L0_1_Decompressed/2_ECG/start_stop_indices.csv'))
    }
    
    setwd(Acc_L3_dir)
    Acc_files <- list.files(pattern='*.csv')
    
    # Add Heartbeat and GLS Data ---------------------------------------------------------------
    
    for (i in 1:length(Acc_files)) {
      m <- read_csv(Acc_files[i])
      m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
      if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
        print("Data continuity check failed.")
        break
      }
      
      birdname_trip <- str_sub(Acc_files[i],1,-23)
      birdname <- str_sub(Acc_files[i],1,-25)
      
      m$HMM_2S_state <- HMMstate(m,birdname_trip,states=2)
      m$HMM_3S_state <- HMMstate(m,birdname_trip,states=3)
      
      GLS_filename <- paste0(birdname,"_GLS_L1.csv")
      ECG_filename <- paste0(birdname,"_ECG_L1.csv")
      
      if (sum(GLS_files==GLS_filename)==0) {
        m$GLS_state <- NA
      } else if (sum(GLS_files==GLS_filename)>1) {
        disp("More than 1 GLS file.")
        break
      } else {
        # Attach GLS data
        Immersion_data <- read_csv(paste0(GLS_L1_dir,GLS_filename))
        Immersion_data$starttime <- as.POSIXct(Immersion_data$starttime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
        Immersion_data$endtime <- as.POSIXct(Immersion_data$endtime,format="%d-%b-%Y %H:%M:%S",tz="GMT")
        Immersion_data_wet <- Immersion_data %>% filter(state=="wet")
        Immersion_data_start <- Immersion_data$starttime[1]
        Immersion_data_end <- Immersion_data$endtime[nrow(Immersion_data)]
        
        m$GLS_state <- "dry"
        
        for (j in 1:nrow(m)) {
          ten_min_start <- m$datetime[j]
          Immersion_data_wet <- Immersion_data_wet %>% mutate(starttime_diff = as.numeric(difftime(starttime,ten_min_start,units="mins")))
          if (any(Immersion_data_wet$starttime_diff>=0 & Immersion_data_wet$starttime_diff<10)) {
            # If a starttime occurs within an hour of the current ten_min_start, the bird is wet during this 10 min interval.
            m$GLS_state[j] <- "wet"
          } else {
            # If an endtime occurs within an hour of the current ten_min_start, the bird is wet during this 10 min interval.
            Immersion_data_wet <- Immersion_data_wet %>% mutate(endtime_diff = as.numeric(difftime(endtime,ten_min_start,units="mins")))
            if (any(Immersion_data_wet$endtime_diff>=0 & Immersion_data_wet$endtime_diff<10)) {
              m$GLS_state[j] <- "wet"          
            }
          }
        }
        
        # Add NAs to 10 min intervals where the GLS is not recording data
        if (m$datetime[1] < Immersion_data_start) {
          m$GLS_state[1:(tail(which(m$datetime < Immersion_data_start),1))] <- NA
        }
        # -1 because the GLS stops recording in the middle of the 10s interval
        if (m$datetime[nrow(m)] > Immersion_data_end) {
          m$GLS_state[(head(which(m$datetime > Immersion_data_end),1)-1):nrow(m)] <- NA
        }
      }
      
      # Attach ECG data
      if (szn == "2019_2020" | szn == "2021_2022") {
        if (sum(ECG_files==ECG_filename)==0) {
          m$Heartbeats <- NA
        } else if (sum(ECG_files==ECG_filename)>1) {
          disp("More than 1 ECG file.")
          break
        } else {
          
          # Attach ECG data
          ECG_data <- read_csv(paste0(ECG_L1_dir,"/",ECG_filename))
          ECG_data$DateTime <- as.POSIXct(ECG_data$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
          
          # find the seconds passed after the first GPS measurement for all flaps
          ECG_data$GPSsec <- as.numeric(difftime(ECG_data$DateTime,m$datetime[1],units='secs'))
          
          # Find the min and max GPSsec
          birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
          bird_ESS <- ECG_start_stop %>% filter(bird == birdname)
          AuxON_dt <- as.POSIXct(paste(birdmeta$AuxON_date_yyyymmdd,birdmeta$AuxON_time_hhmmss),format="%Y%m%d %H%M%S",tz="GMT")
          first_ECG_dt <- AuxON_dt + seconds(bird_ESS$start/600)
          last_ECG_dt <- AuxON_dt + seconds(bird_ESS$stop/600)
          
          # Filter for HBs with a probability geq(min_peak_prob)
          ECG_data <- ECG_data %>% filter(probability>=min_peak_prob)
          
          breaks <- seq(0,600*nrow(m),by=600)
          categories <- cut(ECG_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
          frequency_table <- table(categories)
          data <- as.data.frame(frequency_table)
          
          # Add data to 600s summary
          m$Heartbeats <- data$Freq
          
          # The last row is not a full 600s of data, so Heartbeats is NA
          m$Heartbeats[nrow(m)] <- NA
          
          # Add NAs to 10 min intervals where the ECG is not recording data
          if (m$datetime[1] < first_ECG_dt) {
            m$Heartbeats[1:(tail(which(m$datetime < first_ECG_dt),1))] <- NA
          }
          # -1 because the ECG stops recording in the middle of the 10s interval
          if (m$datetime[nrow(m)] > last_ECG_dt) {
            m$Heartbeats[(head(which(m$datetime > last_ECG_dt),1)-1):nrow(m)] <- NA
          }
        }
      } else {
        m$Heartbeats <- NA
      }
      
      # Save m
      m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
      write.csv(m, file=paste0(write_dir,birdname_trip,"_Flaps_HMM_GLS_ECG_600s.csv"), row.names=FALSE)
    }
  }