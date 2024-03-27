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

library(dplyr)
library(stringr)
library(readxl)
library(foreach)


# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/ERA5_SingleLevels_10m/",szn,"/")
Acc_L3_dir <- paste0(GD_dir,"L3/",location,"/Tag_Data/Acc/",szn,"/")

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

setwd(wind_L2_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_bwa.csv$","",files)

# Add flaps ---------------------------------------------------------------

for (i in 1:length(files)) {
  m <- read.csv(files[i])
 
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != 600*(nrow(m)-1)) {
    print("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(files[i],1,-9)
  birdname <- str_sub(files[i],1,-11)
  birdspp <- str_sub(birdname,1,4)
  
  # Attach number of flaps
  acc_filename <- paste0(birdname,"_Acc_L2.csv")
  if (sum(acc_files==acc_filename)==1) {
    flap_data <- read.csv(paste0(Acc_dir,acc_filename))
  
    # find the seconds passed after the first GPS measurement for all flaps
    flap_data$GPSsec <- as.numeric(difftime(flap_data$DateTime,m$datetime[1],units='secs'))
    
    breaks <- seq(0,600*nrow(m),by=600)
    categories <- cut(flap_data$GPSsec,breaks,include.lowest=TRUE,right=FALSE)
    frequency_table <- table(categories)
    data <- as.data.frame(frequency_table)
    m$flaps <- data$Freq
    
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(Acc_L3_dir,birdname_trip,"_Acc_L3_wind_10min.csv"), row.names=FALSE)
  
  }
}
