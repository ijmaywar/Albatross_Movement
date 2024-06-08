################################################################################
#
# This script compiles all GPS files from the same species 
# (across all field seasons)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
species <- "WAAL"

locations = c("Bird_Island","Midway")

# Set Environment ---------------------------------------------------------

library(dplyr)
library(ggplot2)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"

# Loop thru locations ----------------------------------------------------------

for (location in locations) {
szn_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/")

if (location == "Bird_Island") {
  species = c("BBAL", "GHAL", "WAAL")
  compile_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/compiled_2019_2022/compiled_by_spp/")
} else if (location == "Midway") {
  species = c("BFAL", "LAAL")
  compile_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/compiled_2018_2023/compiled_by_spp/")
} else {
  print("Can't find location")
}

setwd(szn_dir)
seasons <- list.files()[1:length(list.files())-1] # Remove the last folder because that's for the compiled data

# Write Files ------------------------------------------------

for (i in 1:length(species)) {
  
  spp = species[i]
  
  for (j in 1:length(seasons)) {
    
    # 600s files
    setwd(paste0(szn_dir,seasons[j],"/"))
    gpsfiles<-list.files(pattern='*.csv')
    for (k in 1:length(gpsfiles)) {
      if (substr(gpsfiles[k],1,4)==spp) {
        current_file <- read_csv(gpsfiles[k])
        current_file$datetime <- as.POSIXct(current_file$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        if (!exists("compiled600s")){
          compiled600s <- current_file
        } else {
          compiled600s <- bind_rows(compiled600s, current_file)
        }
      }
    }
  }
  
  compiled600s$datetime <- as.character(format(compiled600s$datetime)) # safer for writing csv in character format
  
  write.csv(compiled600s, file=paste0(compile_dir,spp,"_Compiled_GPS_L2_600s.csv"), row.names=FALSE)
  
  rm(compiled600s)
  
}
}
