################################################################################
#
# Wind data: Take 600s wind data with gps, HMM, num. flaps and turn it into hourly intervals 
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2021_2022"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(lubridate)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/NEW_STRUCTURE/"
read_dir <- paste0(GD_dir, "Projects/Maywar/Flaps_ImmersionState_600s/GLS/",location,"/",szn,"/")
write_dir <- paste0(GD_dir, "Projects/Maywar/Flaps_Hourly_AnalysisReady/GLS/",location,"/",szn,"/")
meta_dir <- paste0(GD_dir, "Projects/Maywar/Flaps_Hourly_AnalysisReady/GLS/",location,"/Meta_tbls/")

setwd(read_dir)
files <- list.files(pattern='*.csv')

# Set up a meta tbl to keep track of how much data is being removed after filtering for dry states
meta_tbl <- data.frame(
  ID = character(),
  samples_1 = numeric(),
  samples_2 = numeric(),
  samples_3 = numeric()
)

# Loop thru and process  ---------------------------------------------------------------

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  birdname_trip <- str_sub(files[i],1,-20)
  birdname <- str_sub(files[i],1,-22)
  birdspp <- str_sub(birdname,1,4)
  
  m$datetime <- as.POSIXct(m$datetime, format="%Y-%m-%d %H:%M:%S")
  m$rounded_hour <- round_date(m$datetime, unit = "hour")
  m$timediff <- abs(as.numeric(difftime(m$datetime,m$rounded_hour)))
  
  m_hourly <- m %>% filter(timediff==min(timediff))
  m_hourly$trim <- 0
  
  # Remove all rows that contain GLS state: "wet"
  # Also, remove all rows that have less than 6 measurements within that given hour
  for (j in 1:nrow(m_hourly)) {
    current_hour <- m_hourly$rounded_hour[j]
    current_m <- m %>% filter(rounded_hour == current_hour)
    m_hourly$flaps[j] <- sum(current_m$flaps)
    if ( (nrow(current_m) != 6) || (any(current_m$GLS_state == "dry")) ) {
      m_hourly$trim[j] <- 1
    }
  }
  
  m_hr_trim <- m_hourly %>% filter(trim == 0)
  m_hr_trim <- m_hr_trim %>% dplyr::select(-timediff,-trim,-GLS_state,-rounded_hour)
  
  if (nrow(m_hr_trim>0)) {
    # If the trimmed file isn't empty, save it.
    m_hr_trim$datetime <- as.character(format(m_hr_trim$datetime)) # safer for writing csv in character format  
    write.csv(m_hr_trim, file=paste0(write_dir,birdname_trip,"_Flaps_GLS_Hourly_AnalysisReady.csv"), row.names=FALSE)
    }
  
  # Record how many samples remained after trimming
  meta_tbl <- rbind(meta_tbl,c(birdname_trip,nrow(m),nrow(m_hourly),nrow(m_hr_trim)))
}

# Write meta_tbl
names(meta_tbl) <- c("trip_ID","samples_600s","samples_Hourly","samples_Hourly_dry")
meta_tbl$samples_600s <- as.numeric(meta_tbl$samples_600s)
meta_tbl$samples_Hourly <- as.numeric(meta_tbl$samples_Hourly)
meta_tbl$samples_Hourly_dry <- as.numeric(meta_tbl$samples_Hourly_dry)
meta_tbl$samples_Hourly_removed <- meta_tbl$samples_Hourly - meta_tbl$samples_Hourly_dry
write.csv(meta_tbl, file=paste0(meta_dir,szn,"_SamplesRemoved.csv"), row.names=FALSE)

hist(meta_tbl$samples_Hourly_dry,breaks=50)

