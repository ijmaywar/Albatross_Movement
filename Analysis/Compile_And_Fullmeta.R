################################################################################
#
# Compile all Hourly summaries and add metadata from fullmeta. 
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG/")
write_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/")

# Load fullmeta
fullmeta <- read_xlsx(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

file_list <- list.files(path = read_dir, full.names = TRUE, recursive = TRUE, pattern='*.csv')

# Compile all files...this will take awhile...
compiled_m <- do.call(rbind, lapply(file_list, read_csv))

compiled_m$datetime <- as.POSIXct(compiled_m$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
birds <- unique(compiled_m$id)

# Add fullmeta Data ---------------------------------------------------------------

compiled_m$Location <- NA
compiled_m$Species <- NA
compiled_m$Field_Season <- NA
compiled_m$Trip_Type <- NA
compiled_m$Aux_TagType <- NA

for (i in 1:length(birds)) {
  current_bird <- birds[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  
  # Add select metadata
  current_bird_rows <- which(compiled_m$id==current_bird)
  compiled_m$Location[current_bird_rows] <- birdmeta$Location
  compiled_m$Species[current_bird_rows] <- birdmeta$Species
  compiled_m$Field_Season[current_bird_rows] <- birdmeta$Field_Season
  compiled_m$Trip_Type[current_bird_rows] <- birdmeta$Trip_Type
  compiled_m$Aux_TagType[current_bird_rows] <- birdmeta$Aux_TagType
}

# Save m
compiled_m$datetime <- as.character(format(compiled_m$datetime)) # safer for writing csv in character format  
write.csv(compiled_m, file=paste0(write_dir,"Compiled_Flaps_HMM_GLS_ECG_Hourly.csv"), row.names=FALSE)
