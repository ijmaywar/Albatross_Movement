################################################################################
#
# Find the completeness of Acc and GPS data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2021_2022"
Acc_Type = "NRL"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(doParallel)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
L1_Acc_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/Acc/","Acc_",Acc_Type,"/",szn,"/")
L1_wind_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")
L1_GPS_summary_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/GPS_Summaries/")

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(L1_GPS_summary_dir)
L1_GPS_summary <- list.files(pattern='*.csv')
L1_GPS_summary <- read.csv(L1_GPS_summary)

setwd(L1_Acc_dir)
acc_files <- list.files(pattern='*.csv')

# Parallelize to load Acc info --------------------------------------------

# Setup parallel backend
my.cluster <- parallel::makeCluster(
  6,
  type = "FORK"
)
# Memory overload with 9 cores. Use 6

doParallel::registerDoParallel(cl = my.cluster)

# Load Acc files and extract start and stop times
Acc_times <- foreach(
  i = 1:length(acc_files),
  .combine = 'rbind'
) %dopar% {
  setwd(L1_Acc_dir)
  m <- read.csv(acc_files[i])
  birdname <- sub("_Acc_L1.csv$","",acc_files[i])
  start <- m$DateTime[1]
  stop <- m$DateTime[nrow(m)]
  out <- data.frame(birdname,start,stop)
  return(out)
}

parallel::stopCluster(cl = my.cluster)



# Alternatively, extract acc data sequentially if parallelization isn't working for some reason

# setwd(L1_Acc_dir)
# Acc_times <- data.frame(matrix(ncol=3,nrow=length(acc_files)))
# colnames(Acc_times) <- c("birdname","start","stop")
# for (i in 1:length(acc_files)) {
#   m <- read.csv(acc_files[i])
#   birdname <- sub("_Acc_L1.csv$","",acc_files[i])
#   start <- m$DateTime[1]
#   stop <- m$DateTime[nrow(m)]
#   Acc_times$birdname[i] <- birdname
#   Acc_times$start[i] <- start
#   Acc_times$stop[i] <- stop
# }


# Extract GPS start and stop ----------------------------------------------

setwd(L1_wind_dir)
GPS_files <- list.files(pattern='*.csv')
m <- read.csv(GPS_files[1])
all_trips <- unique(m$tripID)

GPS_times <- data.frame(tripID = character(length(all_trips)),
                        start = character(length(all_trips)),
                        stop = character(length(all_trips)))
colnames(GPS_times) <- c("tripID", "start", "stop")
for (i in 1:length(all_trips)) {
  mi <- filter(m,tripID==all_trips[i])
  start <- mi$datetime[1]
  stop <- mi$datetime[nrow(mi)]
  GPS_times$tripID[i] <- all_trips[i]
  GPS_times$start[i] <- start
  GPS_times$stop[i] <- stop
}

GPS_times$birdID <- str_sub(GPS_times$tripID,1,-3)

# Pass thru acc files -----------------------------------------------------
allignment <- Acc_times
colnames(allignment) <- c("bird","acc_start","acc_stop")

for (i in 1:nrow(Acc_times)) {
  current_bird <- Acc_times$birdname[i]
  birdmeta <- filter(fullmeta,Deployment_ID == current_bird)
  birdGPSsummary <- filter(L1_GPS_summary,bird == current_bird)
  birdGPS <- filter(GPS_times,birdID==current_bird)
  allignment$GPS_start[i] <-birdGPS$start[1]
  allignment$GPS_stop[i] <- birdGPS$stop[nrow(birdGPS)]
  allignment$meta_recap[i] <- paste(birdmeta$Recapture_Date_yyyymmdd, birdmeta$Recapture_Time_hhmm)
  allignment$ntrips[i] <- birdGPSsummary$ntrips 
  allignment$finish_on_colony[i] <- birdGPSsummary$tripcomplete
  allignment$last_loc_km_from_col[i] <- birdGPSsummary$last_loc_km_from_col
}

# format datetime columns for duration calculations
allignment$acc_start <- as.POSIXct(allignment$acc_start,format="%Y-%m-%d %H:%M:%S",tz="GMT")
allignment$acc_stop <- as.POSIXct(allignment$acc_stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
allignment$GPS_start <- as.POSIXct(allignment$GPS_start,format="%Y-%m-%d %H:%M:%S",tz="GMT")
allignment$GPS_stop <- as.POSIXct(allignment$GPS_stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
allignment$meta_recap <- as.POSIXct(allignment$meta_recap,format="%Y%m%d %H%M",tz="GMT")

allignment <- allignment %>% mutate(acc_dur_days = as.numeric(difftime(acc_stop,acc_start,units="days")),
                                    GPS_dur_days = as.numeric(difftime(GPS_stop,GPS_start,units="days")),
                                    start_diff_mins = as.numeric(difftime(acc_start,GPS_start,units="mins")),
                                    stop_diff_mins = as.numeric(difftime(acc_stop,GPS_stop,units="mins")),
                                    meta_acc_diff_mins = as.numeric(difftime(meta_recap,acc_stop,units="mins")),
                                    meta_GPS_diff_mins = as.numeric(difftime(meta_recap,GPS_stop,units="mins")))

# Save file
allignment$acc_start <- as.character(format(allignment$acc_start)) # safer for writing csv in character format
allignment$acc_stop <- as.character(format(allignment$acc_stop)) # safer for writing csv in character format
allignment$GPS_start <- as.character(format(allignment$GPS_start)) # safer for writing csv in character format
allignment$GPS_stop <- as.character(format(allignment$GPS_stop)) # safer for writing csv in character format
allignment$meta_recap <- as.character(format(allignment$meta_recap)) # safer for writing csv in character format

# Make sure you're not overwriting something !!!!! -----------------------------
write.csv(allignment,file=paste0(GD_dir,"/metadata/Tag_completeness_allignment/", location,"_",szn,"_",Acc_Type,"_allignment.csv"),row.names = FALSE)


# fix meta_acc_diff_mins and meta_GPS_diff_mins --------------------------------
#
# # read allignment file
# szn = "2020_2021"
# location = "Bird_Island"
# Acc_Type = "Technosmart"
# 
# allignment <- read.csv(paste0(GD_dir,"/metadata/Tag_completeness_allignment/", location,"_",szn,"_",Acc_Type,"_allignment.csv"))
# allignment$acc_stop <- as.POSIXct(allignment$acc_stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# allignment$GPS_stop <- as.POSIXct(allignment$GPS_stop,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# # allignment$meta_recap <- as.POSIXct(allignment$meta_recap,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# allignment$meta_recap <- as.POSIXct(allignment$meta_recap,format="%Y%m%d %H%M",tz="GMT")
# 
# allignment <- allignment %>% mutate(meta_acc_diff_mins = as.numeric(difftime(meta_recap,acc_stop,units="mins")),
#                                     meta_GPS_diff_mins = as.numeric(difftime(meta_recap,GPS_stop,units="mins")))
# 
# write.csv(allignment,
#           file=paste0(GD_dir,"/metadata/Tag_completeness_allignment/", location,"_",szn,"_allignment.csv"),
#           row.names = FALSE)
