# compare previously processed files to newly processed files

################################################################################
# PACKAGES

library(tidyverse)
library(arsenal)
library(readxl)

################################################################################
# USER INPUTED VALUES

szn = '2018_2019'
location = 'Midway' # Options: 'Bird_Island', 'Midway', 'Wandering'
computer = 'MBP' # Options: 'MBP', 'ThinkPad'

buffer_dist <- 2 # KM spatial buffer for counting trips

################################################################################
# Set up Environment
new_L1_1_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Midway/Tag_Data/GPS/GPS_Catlog/2018_2019/1_onbird/"
new_L1_2_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Midway/Tag_Data/GPS/GPS_Catlog/2018_2019/2_buffer2km/"

old_L1_1_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/data/L1/Midway/GPS_L1_1_onbird/"
old_L1_2_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/data/L1/Midway/GPS_L1_2_buffer2km/"

# Import times metadata: ------------------------------------------------------------
fullmeta <- read_excel("/Volumes/LaCie/Full_metadata.xlsx", sheet = location)
fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

################################################################################
# Get file lists

setwd(new_L1_1_dir)
new_L1_1_gpsfiles<-list.files(pattern='*.csv')

setwd(new_L1_2_dir)
new_L1_2_gpsfiles<-list.files(pattern='*.csv')

setwd(old_L1_1_dir)
old_L1_1_gpsfiles<-list.files(pattern='*.csv')

setwd(old_L1_2_dir)
old_L1_2_gpsfiles<-list.files(pattern='*.csv')


################################################################################
# Compare L1_1 files
for (i in 16:length(new_L1_1_gpsfiles)) {
  setwd(new_L1_1_dir)
  new_file <- read_csv(new_L1_1_gpsfiles[i])
  namesplit <- strsplit(new_L1_1_gpsfiles[i],"_")[[1]]
  current_bird <- paste0(namesplit[1],"_",namesplit[2],"_",namesplit[3])
  
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  
  old_name <- substr(birdmeta$GPS_OG_ID,1,nchar(birdmeta$GPS_OG_ID)-5)
  old_filename <- paste0(old_name,"_L1_1_onbird.csv")
  setwd(old_L1_1_dir)
  old_file <- read_csv(old_filename)
  
  new_file_comp <- select(new_file, -id, -lon, -tripID)
  old_file_comp <- select(old_file, -id, -lon, -onLand, -tripID)
  unequal_rows <- which(!identical(new_file_comp,old_file_comp))
  
  if (length(unequal_rows)>0){
    old_rm_last <- filter(old_file_comp,row_number() <= n()-1)
    unequal_rm_last <- which(!identical(new_file_comp,old_rm_last))
    
    old_rm_first <- filter(old_file_comp,row_number() > 1)
    unequal_rm_first <- which(!identical(new_file_comp,old_rm_first))
    
    if(length(unequal_rm_last)>0 & length(unequal_rm_first)>0) {
      print(paste(current_bird, "is not the same."))
      summary(comparedf(new_file_comp,old_file_comp))
      break     
    }
  }
}

################################################################################
# Compare L1_2 files
for (i in 1:length(new_L1_2_gpsfiles)) {
  setwd(new_L1_2_dir)
  new_file <- read_csv(new_L1_2_gpsfiles[i])
  namesplit <- strsplit(new_L1_2_gpsfiles[i],"_")[[1]]
  current_bird <- paste0(namesplit[1],"_",namesplit[2],"_",namesplit[3])
  
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  
  old_name <- substr(birdmeta$GPS_OG_ID,1,nchar(birdmeta$GPS_OG_ID)-5)
  old_filename <- paste0(old_name,"_L1_2_buffer2km.csv")
  setwd(old_L1_2_dir)
  old_file <- read_csv(old_filename)
  
  new_file_comp <- select(new_file, -id, -lon, -tripID)
  old_file_comp <- select(old_file, -id, -lon, -onLand, -tripID)
  unequal_rows <- which(!identical(new_file_comp,old_file_comp))
  
  if (length(unequal_rows)>0){
    old_rm_last <- filter(old_file_comp,row_number() <= n()-1)
    unequal_rm_last <- which(!identical(new_file_comp,old_rm_last))
    
    old_rm_first <- filter(old_file_comp,row_number() > 1)
    unequal_rm_first <- which(!identical(new_file_comp,old_rm_first))
    
    if(length(unequal_rm_last)>0 & length(unequal_rm_first)>0) {
      print(paste(current_bird, "is not the same."))
      summary(comparedf(new_file_comp,old_file_comp))
      break     
    }
  }
}


