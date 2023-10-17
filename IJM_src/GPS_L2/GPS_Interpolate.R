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

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/"
L1_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/")
L2_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/",location,"/Tag_Data/GPS/",szn,"/")
meta_dir <- paste0(L2_dir,"intrp_summary/")

# Import metdata
# fullmeta <- read_excel("/Volumes/LaCie/Full_metadata.xlsx", sheet = location)
# fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

# Find GPS files
setwd(L1_dir)
gpsfiles<-list.files(pattern='*.csv')

# Create Write Directories
dir.create(paste0(L2_dir,'quant/'))

dir600<-'/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Midway/2018-2019/Tag_Data/L1_Tag_Data/GPS_L1_3_interpolated/600s/'
dir3600<-'/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Midway/2018-2019/Tag_Data/L1_Tag_Data/GPS_L1_3_interpolated/3600s/'

# User Functions ----------------------------------------------------------

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}




for (i in 1:length(files)) {
  
  mi<-read.csv(files[i])
  # ---------------------------------------------------------------------
  # Interpolate Tracks
  # ---------------------------------------------------------------------
  newm<-mi
  newm$lat<-as.numeric(mi$lat)
  newm$lon<-as.numeric(wrap360(mi$lon))
  coordinates(newm) = c("lon","lat")
  proj4string(newm) <- CRS("+proj=longlat +ellps=WGS84")
  xy<-cbind(newm$lon,newm$lat)
  ptime <- as.POSIXct(mi$datetime, tz="UTC")
  
  tripscheck<-which(!is.na(mi$tripID))
  if (is_empty(tripscheck)) {
    # dont do nothin
  }else{
    # convert to ltraj - this is sda filtered and colony-buffered trip (need to keep tripID)
    traj_m<-as.ltraj(xy, date=ptime, id=mi$tripID, typeII=TRUE, proj4string = CRS("+proj=longlat +ellps=WGS84"))
    traj_3600s<-redisltraj(traj_m,3600,type="time") #3600 = 1 hour (60 min) 
    traj_600s<-redisltraj(traj_m,600,type="time") #600s = 10min
    traj_30s<-redisltraj(traj_m,30,type="time") #30s
    
    # Convert to Dataframes
    df3600<-ld(traj_3600s) 
    df3600_keep <- df3600 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df3600_keep) <- c("id","datetime","lon","lat","tripID")
    
    df600<-ld(traj_600s) 
    df600_keep <- df600 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df3600_keep) <- c("id","datetime","lon","lat","tripID")
    
    df30<-ld(traj_30s) 
    df30_keep <- df30 %>% 
      mutate(birdid=mi$id[1]) %>% 
      dplyr::select(birdid, date, x, y, id)
    colnames(df30_keep) <- c("id","datetime","lon","lat","tripID")
    
    # And Save
    write.csv(df3600_keep, file=paste0(dir3600, mi$id[1], "_L1_3_interp3600s.csv"), row.names=FALSE)
    write.csv(df600_keep, file=paste0(dir600, mi$id[1], "_L1_3_interp600s.csv"), row.names=FALSE)
    write.csv(df30_keep, file=paste0(dir30, mi$id[1], "_L1_3_interp30s.csv"), row.names=FALSE)
    
    
    rm(list=ls()[! ls() %in% c("wrap360", "files", "dir30", "dir600", "dir3600", "i")])
    
  }
  
}