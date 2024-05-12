# Remove trips less than 2 hrs and trips where the GPS doesn't record data for the entire trip
# Also add TripType data.

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

loc = 'Midway'

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))
if (loc == "Bird_Island") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2019_2022/compiled_by_spp/")
  write_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2019_2022/compiled_complete/")
} else if (loc == "Midway") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2018_2023/compiled_by_spp/")
  write_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2018_2023/compiled_complete/")
}

setwd(GPS_dir)
files <- list.files(pattern='*.csv')

for (i in 1:length(files)) {
  m <- read_csv(files[i])
  m$datetime <- as.POSIXlt(m$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  if (i==1) {
    all_data <- m
  } else {
    all_data <- rbind(all_data,m)
  }
}

alltrips <- unique(all_data$tripID)

# if trips are less than two hours, remove them
for (i in 1:length(alltrips)) {
  if (nrow(all_data %>% dplyr::filter(tripID==alltrips[i])) < 12) {
    all_data <- all_data %>% filter(tripID!=alltrips[i])
  }
}

allbirds <- unique(all_data$id)

# If metadata says that GPS data is incomplete, remove the bird
# Also, add metadata for trip type
all_data$TripType <- NA
for (i in 1:length(allbirds)) {
  current_bird <- allbirds[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  if (nrow(birdmeta)==0) {
    # Catching birds that have been historically misnamed 
    if (current_bird=="WAAL_20220129_WB25") {
      birdmeta <- fullmeta %>% filter(Deployment_ID == "WAAL_20220129_WB24")
    }
    else if (current_bird == "WAAL_20220310_BP45") {
      birdmeta <- fullmeta %>% filter(Deployment_ID == "WAAL_20220310_BP43")
    }
  }
  if (is.na(birdmeta$Pos_complete)) {
    all_data <- all_data %>% filter(id!=allbirds[i])
  } else if (birdmeta$Pos_complete==FALSE) {
    all_data <- all_data %>% filter(id!=allbirds[i])
  } else {
    all_data <- all_data %>% mutate(TripType = if_else(id==allbirds[i],birdmeta$Trip_Type,TripType))
  }
}

# update alltrips and allbirds
alltrips <- unique(all_data$tripID)
allbirds <- unique(all_data$id)

# Reclassify E_pip as BG
all_data <- all_data %>% mutate(TripType = if_else(TripType %in% c("E_pip","2BEP","Ep"),"BG",TripType))

# remove any na values
all_data <- na.omit(all_data)

# write file
write_csv(all_data,paste0(write_dir,loc,"_compiled_complete.csv"))

