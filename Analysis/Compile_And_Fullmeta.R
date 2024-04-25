################################################################################
#
# Compile all Hourly summaries and add metadata from fullmeta. 
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

min_peak_prob = 0 # What was the min_peak_prob used to create summary data?

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"

if (min_peak_prob == 0) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG/p_0/")
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_0/")
} else if (min_peak_prob == 0.85) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG/p_085/")
  write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_085/")
}

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

file_list <- list.files(path = read_dir, full.names = TRUE, recursive = TRUE, pattern='*.csv')

# Compile all files...this will take awhile...
compiled_m <- do.call(rbind, lapply(file_list, read_csv))

compiled_m$datetime <- as.POSIXct(compiled_m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")

birds <- unique(str_sub(basename(file_list),1,-32))
# Add fullmeta Data ---------------------------------------------------------------

compiled_m$Location <- NA
compiled_m$Species <- NA
compiled_m$Field_Season <- NA
compiled_m$Trip_Type <- NA
compiled_m$Aux_TagType <- NA
compiled_m$Body_Mass_Kg <- NA

for (i in 1:length(birds)) {
  current_bird <- birds[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  
  # Add select metadata
  current_bird_rows <- which(compiled_m$id==current_bird)
  if (length(current_bird_rows)==0) {
    # If there is a labeling issue with the Deployment_ID. This has occured with 2 WAAL birds...
    current_bird_rows <- which(str_sub(compiled_m$id,1,-2)==str_sub(current_bird,1,-2))
    # Amend id and tripID columns
    compiled_m$id[current_bird_rows] <- current_bird
    compiled_m$tripID[current_bird_rows] <- paste0(current_bird,str_sub(compiled_m$tripID[current_bird_rows],start=-2))
  }
  compiled_m$Location[current_bird_rows] <- birdmeta$Location
  compiled_m$Species[current_bird_rows] <- birdmeta$Species
  compiled_m$Field_Season[current_bird_rows] <- birdmeta$Field_Season
  compiled_m$Trip_Type[current_bird_rows] <- birdmeta$Trip_Type
  compiled_m$Aux_TagType[current_bird_rows] <- birdmeta$Aux_TagType
  compiled_m$Body_Mass_Kg[current_bird_rows] <- birdmeta$Body_Mass_Kg
}

# Save m
compiled_m$datetime <- as.character(format(compiled_m$datetime)) # safer for writing csv in character format  
write.csv(compiled_m, file=paste0(write_dir,"Compiled_Flaps_HMM_GLS_ECG_Hourly.csv"), row.names=FALSE)

# Filter for only ECG birds ----------------------------------------------------

ECG_L1_dir <- paste0(GD_dir, "L1/Bird_Island/Tag_Data/ECG/ECG_NRL/")
setwd(ECG_L1_dir)
ECG_files <- list.files(pattern='*.csv',recursive = TRUE,full.names = FALSE)
ECG_birds <- str_sub(ECG_files,11,-12)

compiled_ECG_m <- compiled_m %>% filter(id %in% ECG_birds)
write.csv(compiled_ECG_m, file=paste0(write_dir,"ECGonly_Compiled_Flaps_HMM_GLS_ECG_Hourly.csv"), row.names=FALSE)

