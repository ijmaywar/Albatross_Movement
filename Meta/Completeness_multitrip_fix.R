################################################################################
#
# Ammend completeness for multi-trip birds
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)

# Set Environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
completeness_dir <- paste0(GD_dir,"/metadata/Tag_completeness_allignment/")

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))
incomplete_birds <- fullmeta %>% filter(Full_dur==0)

setwd(completeness_dir)
completeness_files <- list.files(pattern='*.csv')

# Create compiled file
for (i in 1:length(completeness_files)) {
  mi <- read_csv(completeness_files[i])
  if (i==1) {
    m <- mi
  } else {
    m <- rbind(m,mi)
  }
}

# GPS finishes all trips but acc does not --------------------------------------
m_Acc_incomplete <- m %>% filter(ntrips>1 & finish_on_colony==TRUE & stop_diff_mins<0)

# Find start and stop times for individual trips
for (i in 1:nrow(m_Acc_incomplete)) {
  birdname <- m_Acc_incomplete$bird[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
  GPS_filename <- paste0(GD_dir,"L1/",birdmeta$Location,"/Tag_Data/GPS/GPS_",
                         birdmeta$Pos_TagType,"/",birdmeta$Field_Season,"/2_buffer2km/",birdname,"_GPS_L1_2.csv")
  GPS_data <- read_csv(GPS_filename)
  GPS_data$datetime <- as.POSIXct(GPS_data$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  trip_vector <- unique(GPS_data$tripID)
  trip_dt_df <- data.frame(tripID = trip_vector,
                           start = numeric(length(trip_vector)),
                           stop = numeric(length(trip_vector)),
                           acc_trip_complete = logical(length(trip_vector)))
  for (j in 1:nrow(trip_dt_df)) {
    trip_GPS_data <- GPS_data %>% filter(tripID == trip_dt_df$tripID[j])
    trip_dt_df$start[j] <- trip_GPS_data$datetime[1]
    trip_dt_df$stop[j] <- trip_GPS_data$datetime[nrow(trip_GPS_data)]
    if (m_Acc_incomplete$acc_stop[i]<trip_dt_df$stop[j]) {
      trip_dt_df$acc_trip_complete[j] <- FALSE
    } else {
      trip_dt_df$acc_trip_complete[j] <- TRUE
    }
  }
  
  # convert DTs from numeric to POSIXct
  trip_dt_df$start <- as.POSIXct(trip_dt_df$start,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  trip_dt_df$stop <- as.POSIXct(trip_dt_df$stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  if (i==1) {
    Acc_incomplete_df <- trip_dt_df
  } else {
    Acc_incomplete_df <- rbind(Acc_incomplete_df,trip_dt_df)
  }
  
}

# Mark these trips in fullmeta (Trips_Full_dur) as complete 
Acc_incomplete_df %>% filter(acc_trip_complete==TRUE)




# Both Acc and GPS stop recording before all trips ends ------------------------
m_Both_incomplete <- m %>% filter(ntrips>1 & finish_on_colony==FALSE)

# Find start and stop times for individual trips
for (i in 1:nrow(m_Both_incomplete)) {
  birdname <- m_Both_incomplete$bird[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
  GPS_filename <- paste0(GD_dir,"L1/",birdmeta$Location,"/Tag_Data/GPS/GPS_",
                         birdmeta$Pos_TagType,"/",birdmeta$Field_Season,"/2_buffer2km/",birdname,"_GPS_L1_2.csv")
  GPS_data <- read_csv(GPS_filename)
  GPS_data$datetime <- as.POSIXct(GPS_data$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  trip_vector <- unique(GPS_data$tripID)
  trip_dt_df <- data.frame(tripID = trip_vector,
                           start = numeric(length(trip_vector)),
                           stop = numeric(length(trip_vector)),
                           acc_trip_complete = logical(length(trip_vector)),
                           GPS_trip_complete = logical(length(trip_vector)))
  for (j in 1:nrow(trip_dt_df)) {
    trip_GPS_data <- GPS_data %>% filter(tripID == trip_dt_df$tripID[j])
    trip_dt_df$start[j] <- trip_GPS_data$datetime[1]
    trip_dt_df$stop[j] <- trip_GPS_data$datetime[nrow(trip_GPS_data)]
    if (m_Both_incomplete$acc_stop[i]<trip_dt_df$stop[j]) {
      trip_dt_df$acc_trip_complete[j] <- FALSE
    } else {
      trip_dt_df$acc_trip_complete[j] <- TRUE
    }
    if (m_Both_incomplete$GPS_stop[i]<trip_dt_df$stop[j]) {
      trip_dt_df$GPS_trip_complete[j] <- FALSE
    } else {
      trip_dt_df$GPS_trip_complete[j] <- TRUE
    }
  }
  
  # convert DTs from numeric to POSIXct
  trip_dt_df$start <- as.POSIXct(trip_dt_df$start,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  trip_dt_df$stop <- as.POSIXct(trip_dt_df$stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  if (i==1) {
    Both_incomplete_df <- trip_dt_df
  } else {
    Both_incomplete_df <- rbind(Both_incomplete_df,trip_dt_df)
  }
  
}

# Mark these trips in fullmeta (Trips_Full_dur) as complete 
Both_incomplete_df %>% filter(acc_trip_complete==TRUE & GPS_trip_complete==TRUE)





