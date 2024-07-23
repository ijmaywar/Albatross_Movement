################################################################################
#
# Find the start and stop times of L1 acc data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(doParallel)
library(readr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
file_list <- list.files(path = paste0(GD_dir,"L1/",location,"/Tag_Data/Acc/Acc_Technosmart/"), full.names = TRUE, recursive = TRUE, pattern='*.csv')

# Parallelize to load Acc info --------------------------------------------

# Setup parallel backend
my.cluster <- parallel::makeCluster(
  6, 
  type = "FORK"
)
# Memory overload with 9 cores. Trying 6... 

doParallel::registerDoParallel(cl = my.cluster)

# Load Acc files and extract start and stop times
Acc_times <- foreach(
  i = 1:length(file_list), 
  .combine = 'rbind'
) %dopar% {
  m <- readr::read_csv(file_list[i])
  if (nrow(m)>0) {
    m$DateTime <- as.POSIXct(m$DateTime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
    birdname <- str_sub(file_list[i],-29,-12)
    start <- m$DateTime[1]
    stop <- m$DateTime[nrow(m)]
    out <- data.frame(birdname,start,stop)
    return(out)
  } else {
    return(NULL)
  }
}

# If there are Acc data from NRL, read them as well
# if (location == "Bird_Island" & (szn == "2019_2020" | szn == "2021_2022")) {
#   Acc_NRL_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/Acc/Acc_NRL/",szn,"/")
#   setwd(Acc_NRL_dir)
#   NRL_acc_files <- list.files(pattern='*.csv')
#   
#   # Load NRL Acc files and extract start and stop times
#   NRL_Acc_times <- foreach(
#     i = 1:length(NRL_acc_files), 
#     .combine = 'rbind'
#   ) %dopar% {
#     m <- readr::read_csv(paste0(Acc_NRL_dir,NRL_acc_files[i]))
#     if (nrow(m)>0) {
#       m$DateTime <- as.POSIXct(m$DateTime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
#       birdname <- sub("_Acc_L1.csv$","",NRL_acc_files[i])
#       start <- m$DateTime[1]
#       stop <- m$DateTime[nrow(m)]
#       out <- data.frame(birdname,start,stop)
#       return(out)
#     } else {
#       return(NULL)
#     }
#   }
#   
#   Acc_times <- rbind(Acc_times,NRL_Acc_times)
# }

parallel::stopCluster(cl = my.cluster)

# Write file
Acc_times$start <- as.character(format(Acc_times$start)) # safer for writing csv in character format  
Acc_times$stop <- as.character(format(Acc_times$stop)) # safer for writing csv in character format  
write.csv(Acc_times, file="/Users/ian/Desktop/Acc_times_2.csv", row.names=FALSE)

# Alternatively, extract acc data sequentially if parallel processing isn't working

# setwd(Acc_Technosmart_dir)
# Acc_times <- data.frame(matrix(ncol=3,nrow=length(acc_files)))
# colnames(Acc_times) <- c("birdname","start","stop")
# for (i in 1:length(acc_files)) {
#   m <- read_csv(acc_files[i])
#   m$DateTime <- as.POSIXct(m$DateTime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
#   birdname <- sub("_Acc_L1.csv$","",acc_files[i])
#   start <- m$DateTime[1]
#   stop <- m$DateTime[nrow(m)]
#   Acc_times$birdname[i] <- birdname
#   Acc_times$start[i] <- start
#   Acc_times$stop[i] <- stop
# }


