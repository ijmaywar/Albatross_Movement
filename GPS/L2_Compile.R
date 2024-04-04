################################################################################
#
# This script compiles all GPS files from the same species 
# (across all field seasons)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'

# Set Environment ---------------------------------------------------------

library(dplyr)
library(ggplot2)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/"
szn_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/",location,"/Tag_Data/GPS/")
compile_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/",location,"/Tag_Data/GPS/compiled/")

setwd(szn_dir)
seasons <- list.files()[1:length(list.files())-1] # Remove the last folder because that's for the compiled data

# Find species ------------------------------------------------

if (location == "Bird_Island") {
  species = c("BBAL", "GHAL", "WAAL")
} else if (location == "Midway") {
  species = c("BFAL", "LAAL")
} else {
  print("Can't find location")
}

# Write Files ------------------------------------------------

for (i in 1:length(species)) {
  
  spp = species[i]
  
  for (j in 1:length(seasons)) {
    
    # 300s files
    setwd(paste0(szn_dir,seasons[j],"/300s/"))
    gpsfiles<-list.files(pattern='*.csv')
    
    for (k in 1:length(gpsfiles)) {
      if (substr(gpsfiles[k],1,4)==spp) {
        current_file <- read.csv(gpsfiles[k])
        if (!exists("compiled300s")) {
          compiled300s <- current_file
        } else {
          compiled300s <- bind_rows(compiled300s, current_file)
        }
      }
    }
    
    # 600s files
    setwd(paste0(szn_dir,seasons[j],"/600s/"))
    gpsfiles<-list.files(pattern='*.csv')
    for (k in 1:length(gpsfiles)) {
      if (substr(gpsfiles[k],1,4)==spp) {
        current_file <- read.csv(gpsfiles[k])
        if (!exists("compiled600s")){
          compiled600s <- current_file
        } else {
          compiled600s <- bind_rows(compiled600s, current_file)
        }
      }
    }
  }
  
  compiled300s$datetime <- as.character(format(compiled300s$datetime)) # safer for writing csv in character format
  compiled600s$datetime <- as.character(format(compiled600s$datetime)) # safer for writing csv in character format
  
  write.csv(compiled300s, file=paste0(compile_dir,"300s/",spp,"interp_300s.csv"), row.names=FALSE)
  write.csv(compiled600s, file=paste0(compile_dir,"600s/",spp,"interp_600s.csv"), row.names=FALSE)
  
  rm(compiled300s,compiled600s)
  
}

