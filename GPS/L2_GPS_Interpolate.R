################################################################################
# Ian's GPS L1 to L2 code. Interpolates GPS data to 10 min (600s) intervals
################################################################################


# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

szn = '2022_2023'
location = 'Midway' # Options: 'Bird_Island', 'Midway'

# Set Environment ---------------------------------------------------------

library(adehabitatLT)
library(lubridate)
library(rlang)
library(dplyr)
library(ggplot2)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
L1_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/")
L2_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/",szn,"/")

# Find GPS files
setwd(L1_dir)
gpsfiles<-list.files(pattern='*.csv')

# User Functions ----------------------------------------------------------

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

# Loop thru birds ---------------------------------------------------------

for (i in 1:length(gpsfiles)) {
  
  mi<-read.csv(gpsfiles[i])
  mi$datetime <- as.POSIXct(mi$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  # ---------------------------------------------------------------------
  # Interpolate Tracks
  # ---------------------------------------------------------------------
  newm<-mi
  newm$lat<-as.numeric(mi$lat)
  newm$lon<-as.numeric(wrap360(mi$lon))
  coordinates(newm) = c("lon","lat")
  proj4string(newm) <- CRS("+proj=longlat +ellps=WGS84")
  xy<-cbind(newm$lon,newm$lat)
  ptime <- mi$datetime
  
  tripscheck<-which(!is.na(mi$tripID))
  if (is_empty(tripscheck)) {
    # dont do nothin
  }else{
    # convert to ltraj - this is sda filtered and colony-buffered trip (need to keep tripID)
    traj_m<-as.ltraj(xy, date=ptime, id=mi$tripID, typeII=TRUE, proj4string = CRS("+proj=longlat +ellps=WGS84"))
    if (length(which(is.na(traj_m[[1]])))>0){
      traj_m<-as.ltraj(xy[1:nrow(xy)-1,], date=ptime[1:length(ptime)-1], id=mi$tripID[1:nrow(mi)-1], typeII=TRUE, proj4string = CRS("+proj=longlat +ellps=WGS84"))
    }
    
    traj_600s<-redisltraj(traj_m,600,type="time") 
    
    # Convert to Dataframes
    df600<-ld(traj_600s) 
    df600_keep <- df600 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df600_keep) <- c("id","datetime","lon","lat","tripID")
    
    # And Save
    df600_keep$datetime <- as.character(format(df600_keep$datetime)) # safer for writing csv in character format
    write.csv(df600_keep, file=paste0(L2_dir, mi$id[1], "_GPS_L2_600s.csv"), row.names=FALSE)
    
  }
  
  rm(list=ls()[! ls() %in% c("wrap360", "buffer_dist", "GD_dir", "gpsfiles", "L1_dir", "L2_dir", "location", "szn", "i")])  
  
}
