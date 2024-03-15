################################################################################
#
# Wind data: Take 600s wind data with gps, HMM, num. flaps and turn it into hourly intervals 
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
library(lubridate)


# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
wind_L3_dir <- paste0(GD_dir,"L3/",location,"/Wind_Data/",szn,"/")
wind_L4_dir <- paste0(GD_dir,"L4/",location,"/Wind_Data/",szn,"/")
meta_dir <- paste0(GD_dir,"L4/",location,"/Wind_Data/meta_tbls/")

setwd(wind_L3_dir)
wind_files <- list.files(pattern='*.csv')

meta_tbl <- data.frame(
    ID = character(),
    samples = numeric()
  )

# Loop thru and process  ---------------------------------------------------------------

for (i in 1:length(wind_files)) {
  m <- read.csv(wind_files[i])
  bird_trip_name <- str_sub(wind_files[i],1,-20)
  
  m$datetime <- as.POSIXct(m$datetime, format="%Y-%m-%d %H:%M:%S")
  m$rounded_hour <- round_date(m$datetime, unit = "hour")
  m$timediff <- abs(as.numeric(difftime(m$datetime,m$rounded_hour)))
  
  m_hourly <- m %>% filter(timediff==min(timediff))
  m_hourly$trim <- 0
  # Remove all rows that contain HMM states 1 and 2
  # Also, remove all rows that have less than 6 measurements within that given hour
  for (j in 1:nrow(m_hourly)) {
    current_hour <- m_hourly$rounded_hour[j]
    current_m <- m %>% filter(rounded_hour == current_hour)
    m_hourly$flaps[j] <- sum(current_m$flaps)
    if ( (nrow(current_m) != 6) || (any(current_m$HMM_state != 3)) ) {
      m_hourly$trim[j] <- 1
    }
  }
  
  m_hr_trim <- m_hourly %>% filter(trim == 0)
  m_hr_trim <- m_hr_trim %>% dplyr::select(-timediff,-trim,-HMM_state,-rounded_hour)
  
  if (nrow(m_hr_trim>0)) {
    # If the trimmed file isn't emmpty, save it.
    m_hr_trim$datetime <- as.character(format(m_hr_trim$datetime)) # safer for writing csv in character format  
    write.csv(m_hr_trim, file=paste0(wind_L4_dir,bird_trip_name,"_L4_hourly.csv"), row.names=FALSE)
  }
  
  # Record how many samples remained after trimming
  meta_tbl <- rbind(meta_tbl,c(bird_trip_name,nrow(m_hr_trim)))
}

names(meta_tbl) <- c("ID","num.samples")
meta_tbl$num.samples <- as.numeric(meta_tbl$num.samples)
write.csv(meta_tbl, file=paste0(meta_dir,szn,"_meta.csv"), row.names=FALSE)

hist(meta_tbl$num.samples,breaks=50)

