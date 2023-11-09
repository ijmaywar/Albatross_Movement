################################################################################
#
# Add GPS info to flaps
#
################################################################################

# Clear envrionment -------------------------------------------------------

rm(list = ls())
# rm(list = ls(all=TRUE))

# User Inputed Values -----------------------------------------------------

szn <- '2019_2020'
location <- 'Bird_Island' # Options: 'Bird_Island', 'Midway'
interval <- '600s'

# Libraries ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# User functions ----------------------------------------------------------

# Set Environment ---------------------------------------------------------
GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
Acc_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/Acc/",szn,"/")
GPS_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/",interval,"/")

# Import metdata
fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))
fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

# Find Acc files
setwd(Acc_dir)
AccFiles<-list.files(pattern='*.csv')

# Loop thru and process ---------------------------------------------------

for (i in 1:length(AccFiles)) {
  
  # Find dep_ID
  dep_ID <- str_sub(AccFiles[i],1,-12)
  Spp <- substr(dep_ID,1,4)
  
  # Load Acc data
  AccData <- read.csv(AccFiles[i])
  
  # Find matching L3_GPS file
  GPS_FileName <- paste0(GPS_dir,Spp,"/",dep_ID,"_GPS_L3_",interval,".csv")
  GPSData <- read.csv(GPS_FileName) 
  
    
  
  
  
  
  df$datetime <- as.character(format(bird_commuting_df$datetime)) # safer for writing csv in character format  
  
  
  
}






