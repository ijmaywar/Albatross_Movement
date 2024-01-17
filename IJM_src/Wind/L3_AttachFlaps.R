################################################################################
#
# Wind data: attach flap data and HMM state to 10 min intervals
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2022_2023"
interp = "600s"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)


# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
HMM_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/",interp,"/3_states/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/",szn,"/",interp,"/")
wind_L3_dir <- paste0(GD_dir,"L3/",location,"/Wind_Data/",szn,"/",interp,"/")

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

setwd(wind_L2_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_bwa.csv$","",files)

if (interp == "600s") {
  interval <- 600
}


# Add flaps ---------------------------------------------------------------

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != interval*(nrow(m)-1)) {
    print("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(files[i],1,-9)
  birdname <- str_sub(files[i],1,-11)
  birdspp <- str_sub(birdname,1,4)
  
  # Attach number of flaps
  acc_filename <- paste0(birdname,"_Acc_L2.csv")
  if (sum(acc_files==acc_filename)>=1) {
    flap_data <- read.csv(paste0(Acc_dir,acc_filename))
  
    # find the seconds passed after the first GPS measurement for all flaps
    flap_data$GPSsec <- as.numeric(difftime(flap_data$DateTime,m$datetime[1],units='secs'))
    
    breaks <- seq(0,interval*nrow(m),by=interval)
    categories <- cut(flap_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
    frequency_table <- table(categories)
    data <- as.data.frame(frequency_table)
    m$flaps <- data$Freq
    
    # Attach HMM state
    HMM_filename <- paste0(birdname,"_GPS_L3_600s.csv")
    HMM_data <- read.csv(paste0(HMM_dir,birdspp,"/",HMM_filename))
    HMM_data <- HMM_data %>% filter(trip_ID==birdname_trip)
    m$HMM_state <- HMM_data$state
    
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(wind_L3_dir,birdname_trip,"_wind_and_flaps.csv"), row.names=FALSE)
  
  }
}
