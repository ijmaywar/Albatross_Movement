################################################################################
#
# Wind data: attach flap data to 10 min intervals
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
interp = "600s"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/",szn,"/",interp,"/")
wind_L3_dir <- paste0(GD_dir,"L3/",location,"/Wind_Data/",szn,"/",interp,"/")

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

setwd(wind_L2_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_bwa.csv$","",files)

if (interp=="600s") {
  interval <- 600
}

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  if (difftime(m$datetime[nrow(m)],m$datetime[1],units="secs") != interval*(nrow(m)-1)) {
    disp("GPS continuity check failed.")
    break
  }
  
  birdname_trip <- str_sub(files[i],1,-9)
  birdname <- str_sub(files[i],1,-11)
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
    
    m$datetime <- as.character(format(m$datetime)) # safer for writing csv in character format  
    write.csv(m, file=paste0(wind_L3_dir,birdname_trip,"_wind_and_flaps.csv"), row.names=FALSE)
  
    }
}
