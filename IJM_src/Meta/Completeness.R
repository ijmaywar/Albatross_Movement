################################################################################
#
# Find the completeness of Acc and GPS data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
szn = "2018_2019"
interp = "600s"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(doParallel)


# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
L1_Acc_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/Acc/Acc_Technosmart/",szn,"/")
L1_wind_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/",szn,"/",interp,"/")
L1_GPS_summary_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/GPS_Summaries/")

fullmeta <- read_excel("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Full_Metadata.xlsx")

setwd(L1_GPS_summary_dir)
L1_GPS_summary <- list.files(pattern='*.csv')
L1_GPS_summary <- read_csv(L1_GPS_summary)

setwd(L1_Acc_dir)
acc_files <- list.files(pattern='*.csv')


# Parallelize to load Acc info --------------------------------------------

# Setup parallel backend
my.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1, 
  type = "FORK"
)

doParallel::registerDoParallel(cl = my.cluster)

# Load Acc files and extract start and stop times
Acc_times <- foreach(
  i = 1:length(acc_files), 
  .combine = 'rbind'
) %dopar% {
  setwd(L1_Acc_dir)
  m <- read_csv(acc_files[i])
  birdname <- sub("_Acc_L1.csv$","",acc_files[i])
  start <- m$DateTime[1]
  stop <- m$DateTime[nrow(m)]
  out <- data.frame(birdname,start,stop)
  return(out)
}


# Extract GPS start and stop ----------------------------------------------

setwd(L1_wind_dir)
GPS_files <- list.files(pattern='*.csv')
m <- read_csv(GPS_files[1])
all_trips <- unique(m$tripID)

GPS_times <- data.frame(matrix(ncol=3,nrow=length(all_trips)))
colnames(GPS_times) <- c("tripID", "start", "stop")
for (i in 1:length(all_trips)) {
  mi <- filter(m,tripID==all_trips[i])
  start <- mi$datetime[1]
  stop <- mi$datetime[nrow(mi)]
  GPS_times[i,] <- c(all_trips[i],as.character(format(start)),as.character(format(stop)))
}

GPS_times$birdID <- str_sub(GPS_times$tripID,1,-3)

# trim out trips less than 2 hours
# GPS_times$trip_dur <- as.numeric(difftime(GPS_times$stop,GPS_times$start,units="hours"))
# GPS_times <- GPS_times %>% filter(trip_dur>=2)


# Pass thru acc files -----------------------------------------------------
allignment <- Acc_times
colnames(allignment) <- c("bird","acc_start","acc_stop")

allignment$GPS_start <- NA
allignment$GPS_stop <- NA
allignment$start_diff <- NA
allignment$stop_diff <- NA

allignment$meta_recap <- NA
allignment$ntrips <- NA
allignment$finaltripcomplete <- NA
allignment$last_loc_km_from_col <- NA


for (i in 1:nrow(Acc_times)) {
  current_bird <- Acc_times$birdname[i]
  birdmeta <- filter(fullmeta,Deployment_ID == current_bird)
  birdGPSsummary <- filter(L1_GPS_summary,bird == current_bird)
  birdGPS <- filter(GPS_times,birdID==current_bird)
  
  allignment$GPS_start[i] <- birdGPS$start[1]
  allignment$GPS_stop[i] <- birdGPS$stop[nrow(birdGPS)]
  allignment$meta_recap[i] <- paste(birdmeta$Recapture_Date_yyyymmdd, birdmeta$Recapture_Time_hhmm)
  allignment$ntrips[i] <- birdGPSsummary$ntrips 
  allignment$finaltripcomplete[i] <- birdGPSsummary$tripcomplete
  allignment$last_loc_km_from_col[i] <- birdGPSsummary$last_loc_km_from_col
}






setwd(wind_L2_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_bwa.csv$","",files)

if (interp == "600s") {
  interval <- 600
}






