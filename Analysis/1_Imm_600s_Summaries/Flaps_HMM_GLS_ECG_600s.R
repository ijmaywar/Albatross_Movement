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

locations = c("Bird_Island", "Midway")
min_peak_prob = 0 # All heartbeats (ECG) with a probability less than this value will be removed

# User defined functions ---------------------------------------------------

HMMstate <- function(birdname_trip,states) {
  dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/",states,"_states/")
  HMM_filename <- paste0(str_sub(birdname_trip,end=-3),"_GPS_L3_600s.csv")
  HMM_data <- read_csv(paste0(dir,str_sub(birdname_trip,1,4),"/",HMM_filename))
  HMM_data <- HMM_data %>% filter(trip_ID==birdname_trip)
  return(HMM_data$state)
}

generate_sequence <- function(start, end, rows) {
  if (is.na(start) & is.na(end)) {
    NA
  } else if (is.na(start) & !is.na(end)) {
    seq(1:end)
  } else if (!is.na(start) & is.na(end)) {
    if (start==rows) {
      start
    } else {
      seq(start:rows)
    }
  } else {
    seq(start,end)
  }
}

extract_OWB_state <- function(m,OWB_filename) {
  m_OWB <- read_csv(paste0(Acc_L4_dir,OWB_filename))
  m_OWB$Start_water_dt <- as.POSIXct(m_OWB$Start_water_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_OWB$Stop_water_dt <- as.POSIXct(m_OWB$Stop_water_dt,format="%d-%b-%Y %H:%M:%S",tz="GMT")
  m_OWB_data_start <- m_OWB$Start_water_dt[1]
  m_OWB_data_end <- m_OWB$Stop_water_dt[nrow(m_OWB)]
  
  OWB_state <- rep("off",times=nrow(m))
  
  # find the seconds passed after the first GPS measurement for all GLS==wet start and stop times
  m_OWB$GPSsec_start <- as.numeric(difftime(m_OWB$Start_water_dt ,m$datetime[1],units='secs'))
  m_OWB$GPSsec_end <- as.numeric(difftime(m_OWB$Stop_water_dt,m$datetime[1],units='secs'))
  
  breaks <- seq(0,600*nrow(m),by=600)
  m_OWB$start <- cut(m_OWB$GPSsec_start,breaks,include.lowest=TRUE,right=FALSE,labels=FALSE)
  m_OWB$end <- cut(m_OWB$GPSsec_end,breaks,include.lowest=TRUE,right=FALSE,labels=FALSE)
  m_OWB$indexes_covered <- pmap(list(m_OWB$start,m_OWB$end,nrow(m)),generate_sequence)
  for (z in 1:nrow(m_OWB)) {
    OWB_state[m_OWB$indexes_covered[[z]]] <- "on"
  }
  
  # Add NAs to 10 min intervals where the Acc is not recording data
  OWB_state[which(is.na(m$flaps))] <- NA
  
  # return m
  return(list(OWB_state=OWB_state,m_OWB=m_OWB))
}

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Loop thru all samples -----------------------------------------------------------
for (location in locations) {
  if (location == "Bird_Island") {
    szns = c("2019_2020", "2020_2021", "2021_2022")
  } else if (location == "Midway") {
    szns = c("2018_2019", "2021_2022", "2022_2023")
  }
  for (szn in szns) {
    cat("Processing location:",location,"Season:",szn,"\n")

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")
Acc_L4_dir <- paste0(GD_dir,"L4/",location,"/Tag_Data/Acc/",szn,"/")
if (min_peak_prob == 0) {
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_0/",location,"/",szn,"/")
} else if (min_peak_prob == 0.85) {
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_600s/Flaps_HMM_GLS_ECG/p_085/",location,"/",szn,"/")
}

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(Acc_L3_dir)
Acc_files <- list.files(pattern='*.csv')

# Add Midway Data ---------------------------------------------------------------
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
      
      m$HMM_2S_state <- HMMstate(birdname_trip,states=2)
      m$HMM_3S_state <- HMMstate(birdname_trip,states=3)
      
      m$GLS_state <- NA
      m$Heartbeats <- NA
      
      OWB_filename <- paste0(birdname,"_Acc_L4_OWB.csv")
      # Find OWB
      OWB_list <- extract_OWB_state(m,OWB_filename)
      m$OWB_state <- OWB_list$OWB_state
      m_OWB <- OWB_list$m_OWB
      
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
    
# Add Bird_Island Data ---------------------------------------------------------------
    
    for (i in 1:length(Acc_files)) {
      m <- read_csv(Acc_files[i])
      m$datetime <- as.POSIXct(m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
      if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
        print("Data continuity check failed.")
        break
      }
      
      birdname_trip <- str_sub(Acc_files[i],1,-23)
      birdname <- str_sub(Acc_files[i],1,-25)
      
      m$HMM_2S_state <- HMMstate(birdname_trip,states=2)
      m$HMM_3S_state <- HMMstate(birdname_trip,states=3)
      
      OWB_filename <- paste0(birdname,"_Acc_L4_OWB.csv")
      GLS_filename <- paste0(birdname,"_GLS_L1.csv")
      ECG_filename <- paste0(birdname,"_ECG_L1.csv")
      
      # Find OWB
      OWB_list <- extract_OWB_state(m,OWB_filename)
      m$OWB_state <- OWB_list$OWB_state
      m_OWB <- OWB_list$m_OWB
      
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
        
        # find the seconds passed after the first GPS measurement for all GLS==wet start and stop times
        Immersion_data_wet$GPSsec_start <- as.numeric(difftime(Immersion_data_wet$starttime,m$datetime[1],units='secs'))
        Immersion_data_wet$GPSsec_end <- as.numeric(difftime(Immersion_data_wet$endtime,m$datetime[1],units='secs'))
        
        breaks <- seq(0,600*nrow(m),by=600)
        Immersion_data_wet$start <- cut(Immersion_data_wet$GPSsec_start,breaks,include.lowest=TRUE,right=FALSE,labels=FALSE)
        Immersion_data_wet$end <- cut(Immersion_data_wet$GPSsec_end,breaks,include.lowest=TRUE,right=FALSE,labels=FALSE)
        Immersion_data_wet$indexes_covered <- pmap(list(Immersion_data_wet$start,Immersion_data_wet$end,nrow(m)),generate_sequence)
        # Immersion_data_wet$indexes_covered[Immersion_data_wet$indexes_covered == "NA"] <- NA 
        for (z in 1:nrow(Immersion_data_wet)) {
          m$GLS_state[Immersion_data_wet$indexes_covered[[z]]] <- "wet"
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
          
          # find the seconds passed after the first GPS measurement for all HeartBeats
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
}
}
