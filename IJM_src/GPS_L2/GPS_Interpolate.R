################################################################################
# Ian's GPS L1 to L2 code
################################################################################


# Clear envrionment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

szn = '2019_2020'
location = 'Bird_Island' # Options: 'Bird_Island', 'Midway', 'Wandering'
buffer_dist <- 2 # KM spatial buffer for counting trips

# Set Environment ---------------------------------------------------------

library(adehabitatLT)
library(lubridate)
library(rlang)
library(dplyr)
library(ggplot2)
library(patchwork)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/"
L1_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/")
L2_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/",location,"/Tag_Data/GPS/",szn,"/")

# Import metdata
# fullmeta <- read_excel("/Volumes/LaCie/Full_metadata.xlsx", sheet = location)
# fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

# Find GPS files
setwd(L1_dir)
gpsfiles<-list.files(pattern='*.csv')

# Create Write Directories
dir.create(paste0(L2_dir,'300s/'))
dir.create(paste0(L2_dir,'600s/'))

# User Functions ----------------------------------------------------------

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

# Loop thru birds ---------------------------------------------------------

for (i in 1:length(gpsfiles)) {
  
  mi<-read.csv(gpsfiles[i])
  # ---------------------------------------------------------------------
  # Interpolate Tracks
  # ---------------------------------------------------------------------
  newm<-mi
  newm$lat<-as.numeric(mi$lat)
  newm$lon<-as.numeric(wrap360(mi$lon))
  coordinates(newm) = c("lon","lat")
  proj4string(newm) <- CRS("+proj=longlat +ellps=WGS84")
  xy<-cbind(newm$lon,newm$lat)
  ptime <- as.POSIXct(mi$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  
  tripscheck<-which(!is.na(mi$tripID))
  if (is_empty(tripscheck)) {
    # dont do nothin
  }else{
    # convert to ltraj - this is sda filtered and colony-buffered trip (need to keep tripID)
    traj_m<-as.ltraj(xy, date=ptime, id=mi$tripID, typeII=TRUE, proj4string = CRS("+proj=longlat +ellps=WGS84"))
    traj_300s<-redisltraj(traj_m,300,type="time") 
    traj_600s<-redisltraj(traj_m,600,type="time") 
    
    # Convert to Dataframes
    df300<-ld(traj_300s) 
    df300_keep <- df300 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df300_keep) <- c("id","datetime","lon","lat","tripID")
    
    df600<-ld(traj_600s) 
    df600_keep <- df600 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df600_keep) <- c("id","datetime","lon","lat","tripID")
    
    # And Save
    write.csv(df300_keep, file=paste0(L2_dir,'300s/', mi$id[1], "_L1_3_interp300s.csv"), row.names=FALSE)
    write.csv(df600_keep, file=paste0(L2_dir,'600s/', mi$id[1], "_L1_3_interp600s.csv"), row.names=FALSE)
    
  }
  
}





##################


p1domain = 1:100

p1 <- ggplot(mi[p1domain,], aes(x=lon,y=lat)) + 
  geom_point(size=1) +
  geom_point(x=wrap360(-38.0658417),y=-54.0101833,color="blue")

p2domain <- 1:24

p2 <- ggplot(df300_keep[p2domain,], aes(x=lon,y=lat)) + 
  geom_point(size=1) +
  geom_point(x=wrap360(-38.0658417),y=-54.0101833,color="blue")

p1+p2

