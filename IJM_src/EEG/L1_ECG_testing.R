# Align HRL start times to ECG data


################################################################################
# USER INPUTED VALUES

szn = '2019_2020'
sheet = 'HRL'
location = 'Bird_Island' # Options: 'Bird_Island', 'Midway', 'Wandering'
computer = 'ThinkPad' # Options: 'MBP', 'ThinkPad'

################################################################################
# Set Environment

library(tidyverse)
library(readxl)
library(lubridate)

if (computer == "MBP") {
  HRL_L0_dir <- paste0("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/HRL/L0_1_Raw_Decompressed_txt/2_EEG/")
  Sensor_L0_dir <- paste0("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/HRL/L0_1_Raw_Decompressed_txt/1_SensorData/")
# L1_dir <- paste0("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/")
# meta_dir <- paste0(L1_dir,"GPS_Summaries/")
} else if (computer == "ThinkPad"){
  HRL_L0_dir <- paste0("G:/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/HRL/L0_1_Raw_Decompressed_txt/2_EEG/")
  Sensor_L0_dir <- paste0("G:/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/HRL/L0_1_Raw_Decompressed_txt/1_SensorData/")
} else {
  print("Computer not found.")
}

# Import metadata: ------------------------------------------------------------
fullmeta <- read_excel("/Volumes/LaCie/Full_metadata.xlsx", sheet = sheet)
fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

sample_freq <- 600
sample_freq_exact <- 599.88

################################################################################
# Load files
setwd(Sensor_L0_dir)
sensor_files <- list.files(pattern='*.txt')

setwd(HRL_L0_dir)
hrl_files<-list.files(pattern='*.txt')
nfiles <- length(hrl_files)
timing_df <- data.frame(dep_ID=vector("character",length=nfiles),timing=vector("character",length=nfiles))

for (i in 1:nfiles) {
  namesplit <- strsplit(hrl_files[i],"_")[[1]]
  
  Acc_ID <- paste(namesplit[1],namesplit[2],namesplit[3],sep="_")
  birdmeta <- fullmeta %>% filter(Acc_OG_ID == Acc_ID)
  if (nrow(birdmeta)==0){
    print("Cannot find bird in metadata.")
    break
  }
  
  Sensor_ID <- paste(namesplit[1],namesplit[2],namesplit[3],namesplit[4],namesplit[5],"SensorData.txt",sep="_")
  setwd(Sensor_L0_dir)
  sensor_data <- read.table(Sensor_ID, header=TRUE, sep = ",")
  
  setwd(HRL_L0_dir)
  hrl_data <- read.table(hrl_files[i],header = TRUE, sep = "\t")
  
  # Build datetime column
  start_dt <- as.POSIXct(paste(birdmeta$AxyON_date_yyyymmdd,birdmeta$AxyON_time_hhmmss), format = "%Y%m%d %H%M", tz = "GMT")
  dt_col <- seq(from = start_dt, by = 1/sample_freq, length.out = length(raw$EEG))
  dt_col_exact <- seq(from = start_dt, by = 1/sample_freq_exact, length.out = length(raw$EEG))
  
  final_dt <- tail(dt_col, n = 1)
  final_dt_exact <- tail(dt_col_exact, n = 1)
  RecaptureDateTime <- as.POSIXct(paste(birdmeta$Recapture_Date_yyyymmdd,birdmeta$Recapture_Time_hhmm), format = "%Y%m%d %H%M", tz = "GMT")
  time_short <- RecaptureDateTime - final_dt
  time_short_exact <- RecaptureDateTime - final_dt_exact
  
  timing_df$dep_ID[i] <- birdmeta$Deployment_ID
  timing_df$timing[i] <- time_short
  }
