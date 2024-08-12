################################################################################
# Ian's GPS L0 to L1 code
# ASSUMES THAT THE DATETIMES RECORDED IN METADATA ARE LOCAL TIMES
# AND TAG DATETIMES ARE RECORDED IN GMT
#
# Sometimes I get an error saying that select() cannot find lon, lat. 
# If I restart Rstudio it gets rid of this error. 
################################################################################


# Clear envrionment -------------------------------------------------------

rm(list = ls())
# rm(list = ls(all=TRUE))

# User Inputed Values -----------------------------------------------------

szn = '2019_2020'
location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'

# Set Environment ---------------------------------------------------------

library(geosphere)
library(tidyverse)
library(lubridate)
library(SDLfilter)
library(trip)
library(tibbletime)
library(anomalize)
library(logr)
library(readxl)
library(tools)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
L0_dir <- paste0(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Pos/Catlog/")
L1_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/")
meta_dir <- paste0(L1_dir,"GPS_Summaries/")

# Import metdata
fullmeta <- read_excel(paste0(GD_dir,"THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Full_Metadata.xlsx"))
fullmeta <- fullmeta %>% filter(Field_season  == szn, Location == location)

# Find GPS files
setwd(L0_dir)
gpsfiles<-list.files(pattern='*.csv')


# User Functions ----------------------------------------------------------

Mode <- function(x) {
  uni <- unique(x)
  uni[which.max(tabulate(match(x, uni)))]
}

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

calculate_speed <- function(df) {
  # Calculate distance using the Haversine formula from 'geosphere'
  distances <- distHaversine(df %>% select(lon,lat))/1000
  dts <- as.numeric(diff(df$ptime))/60
  # Calculate speed (distance / time) and convert to desired units (e.g., km/h)
  speed <- distances / dts
  
  # Add the speed values to the data frame
  df$distances <- c(NA,distances)
  df$dts <- c(NA,dts)
  df$myspeed <- c(NA, speed)
  
  return(df)
}


# Set Global Parameters ---------------------------------------------------

if (location == "Midway") {
  colony_coords <- c(-177.3813,28.19989)
  loc_tz = "Pacific/Midway"
} else if (location == "Bird_Island") {
  colony_coords <- c(-38.0658417,-54.0101833)
  loc_tz = "GMT" # Bird_Island uses GMT, not UTC-2:00.
} else {
  print("Location not found.")
  break
}

buffer_dist <- 2 # KM spatial buffer for counting trips

# initialize meta dataframe
df<-as.data.frame(matrix(NA,nrow=length(gpsfiles),ncol=12))
colnames(df)<-c('bird','tagid','interval_minutes','int_sd','int_max','duration_tag','duration_bird','ntrips','tripcomplete','last_loc_km_from_col','tag_recorded_until_removal','num_locs_filteredOut')

################################################################################
# Loop Through Birds

for (i in 1:length(gpsfiles)) {
  
  namesplit <- strsplit(gpsfiles[i],"_")[[1]]
  dep_ID <- paste0(namesplit[1],"_",namesplit[2],"_",namesplit[3])
  
  # Find matching row in meta
  birdmeta <- fullmeta %>% filter(Deployment_ID == dep_ID)
  if (nrow(birdmeta)==0) {
    print(paste0(dep_ID, " was not found in fullmeta."))
    break
  }
  

# Import data and clean column names --------------------------------------

  if (location=="Bird_Island" && szn=="2020_2021") { 
    m<-read.csv(gpsfiles[i]) # 2020_2021 data starts on first row for bird island gps files. skip=6 for all other szns
  } else {
    m<-read.csv(gpsfiles[i], skip=6)
  }
  
  # Remove rows where lat and lon are NA
  m<-m[!is.na(m$Latitude),]
  
  # Create GPS datetime columns (GMT column and local TZ column)
  if (substr(m$Date[1],3,3) == "/" || substr(m$Date[1],2,2) == "/") {
    m$ptime <- as.POSIXct(paste(m$Date,m$Time), format='%m/%d/%Y %H:%M:%S', tz="GMT")
  } else {
    m$ptime <- as.POSIXct(paste(m$Date,m$Time), format='%Y-%m-%d %H:%M:%S', tz="GMT")
  }
  
  # sometimes ptime writes the year like this 0021 (instead of 2021). Fix this.
  if (year(m$ptime[1])==21) {
    year(m$ptime) <- 2021
  }
  
  m$ptime_loc <- with_tz(m$ptime, loc_tz)

  # Clean up GPS column names
  if (length(which(grepl( "Temp" , colnames(m) )))!=1) {
    m$Temperature_C <- NA
  }else{
    colnames(m)[which(grepl( "Temp" , colnames(m) ))]<-"Temperature_C"
  }
  if (length(which(grepl( "Speed" , colnames(m) )))!=1) {
    m$Speed_kmhr <- NA
  }else{
    colnames(m)[which(grepl( "Speed" , colnames(m) ))]<-"Speed_kmhr"
  }
  if (length(which(grepl( "TTFF" , colnames(m) )))!=1) {
    m$TTFF <- NA
  }else{
    colnames(m)[which(grepl( "TTFF" , colnames(m) ))]<-"TTFF"
  }
  if (length(which(grepl( "atellites" , colnames(m) )))!=1) {
    m$satellites <- NA
  }else{
    colnames(m)[which(grepl( "atellites" , colnames(m) ))]<-"satellites"
  }
  if (length(which(grepl( "ltitude" , colnames(m) )))!=1) {
    m$altitude <- NA
  }else{
    colnames(m)[which(grepl( "ltitude" , colnames(m) ))]<-"altitude"
  }
  

# Check Random Weirdness --------------------------------------------------
  # I've found a few tracks with some completely nonsensical times.
  # This script is to catch those, and remove those instances
  
  # using anomolize to identify extreme outliers in the ntime column.
  mt<-as_tibble(m)
  mt$ntime<-as.numeric(m$ptime)
  af<-anomalize(mt,ntime,alpha=0.05,verbose = FALSE) # lower alpha more conservative
  nix<-as.numeric(which(af$anomaly=="Yes"))
  
  # Remove and Print to Catch and Save - should be rare - maybe just in BBAL 101; if more, need to check and potentially adjust alpha
  if (!is_empty(nix)) {
    log_print(gpsfiles[i])
    log_print(nix)
    log_print(as.character(m$ptime[nix]))
    m<-m[-nix,]
  } else {
    print("No anomalies found in datetimes")
  }
  
  

# L0 -> L1_1 --------------------------------------------------------------
  # Trim GPS file to when tag is on bird.
  # The point of it is to truncate times that the GPS was on and being carried 
  # around in the field, or, traveling in car to field site, etc. 
  
  # Use times from meta (capturetime_local and releasetime_local) to match closest time in GPS datafile. 
  # Set Start of When Tag Was On Bird:
  CaptureDateTime = as.POSIXct(paste(birdmeta$Capture_Date_yyyymmdd,birdmeta$Capture_Time_hhmm), format = "%Y%m%d %H%M", tz = loc_tz)
  RecaptureDateTime = as.POSIXct(paste(birdmeta$Recapture_Date_yyyymmdd,birdmeta$Recapture_Time_hhmm), format = "%Y%m%d %H%M", tz = loc_tz)

  if (is.na(RecaptureDateTime)) {
    m_bird <- m %>% filter(CaptureDateTime <= ptime_loc)
  } else {
    m_bird <- m %>% filter(CaptureDateTime <= ptime_loc & ptime_loc <= RecaptureDateTime)
  }
  
  # Speed and Lat, Lon filters for ridiculous speeds and locations.
  # Add required columns for function: "id", "DateTime","lat","lon","qi"
  m_bird$id       <- dep_ID
  m_bird$DateTime <- m_bird$ptime
  m_bird <- rename(m_bird, lat = Latitude)
  m_bird <- rename(m_bird, lon = Longitude)
  
  # Negative timestep filter
  neg_TS_idx <- which(diff(m_bird$DateTime)<0)+1
  n_TS_rmv <- length(neg_TS_idx)
  if (n_TS_rmv > 0) {
    m_bird <- m_bird[-neg_TS_idx,]
  }
  
  # Lat, Lon filter (outside of geographic bounds)
  m_latlon_filter <- m_bird %>% filter((lon >= -180 & lon <= 180) & (lat >=-90 & lat <= 90))
  n_latlon_rmv <- nrow(m_bird) - nrow(m_latlon_filter)
  print(paste("Lat Lon filter removed",n_latlon_rmv,"of",nrow(m_bird),"locations"))
  m_bird <- m_latlon_filter
  rm(m_latlon_filter)
  
  # Manually calculate minimum speed by using lat, lon and datetimes.
  # If the min speed is >= 200 kmh throw a warning. No way birds are flying this fast.
  speed_thresh <- 200
  n_speed_rmv <- 0
  m_speed <- calculate_speed(m_bird)
  if (max(na.omit(m_speed$myspeed))>=speed_thresh) {
    while (max(na.omit(m_speed$myspeed))>=speed_thresh) {
      print(paste0(dep_ID, " is speeding."))
      fast_idx <- first(which(m_speed$myspeed>=speed_thresh))
      m_bird <- m_bird[-fast_idx,]
      m_speed <- calculate_speed(m_bird)
      n_speed_rmv <- n_speed_rmv + 1
    }
  }
  
  m_bird<-m_bird %>% select(id,ptime,lon,lat,altitude,Speed_kmhr,Temperature_C,satellites,HDOP,PDOP,TTFF)
  colnames(m_bird) <- c("id","datetime","lon","lat","altitude","Speed_kmhr","Temp_C","satellites","HDOP","PDOP","TTFF")
  
  # Fill out Metadata Table 
  df$num_locs_filteredOut[i] <- n_TS_rmv + n_latlon_rmv + n_speed_rmv
  int<-diff(m_bird$datetime) # Interval Metadata 
  units(int)<-"mins"
  
  df$bird[i]                <- dep_ID
  df$tagid[i]               <- paste0(birdmeta$Pos_TagType,"_",birdmeta$Pos_TagNumber)
  df$interval_minutes[i]    <- round(Mode(as.numeric(int)),1)
  df$int_sd[i]              <- round(sd(as.numeric(int), na.rm=TRUE),2)
  df$int_max[i]             <- round(max(as.numeric(int),na.rm=TRUE),1)
  df$duration_tag[i]        <- round(as.numeric(difftime(max(m_bird$datetime),min(m_bird$datetime), units = "days")), 2)
  df$duration_bird[i]       <- round(as.numeric(abs(difftime(CaptureDateTime,RecaptureDateTime, units = "days"))), 2)
  

# L1_1 -> L1_2 ------------------------------------------------------------
  # Trim GPS data to only include samples taken when bird is off-colony
  
  # First calculate distance from colony, for every location
  distcol <- cbind(m_bird$lon, m_bird$lat)
  dist_from_col <- matrix(NA,nrow=length(m_bird$id),ncol=1)
  
  for (k in 1:nrow(m_bird)) {
    dist_from_col[k]<-distHaversine(colony_coords,distcol[k,])/1000 # calculate the distance
  }
  rm(k)        
  
  # Plot distance from colony and save
  png(paste0(meta_dir,'distcolfigs/',dep_ID,"_distcol_plot.png"), width = 650, height = 650)
  plot(dist_from_col)
  title(dep_ID)
  dev.off()
  
  # SPATIAL BUFFER AND TRIP METADATA
  # use rle function to count repeats in sequence: http://masterr.org/r/how-to-find-consecutive-repeats-in-r/
  rr <- as.numeric(dist_from_col)
  rv <- rle(rr > buffer_dist)
  # a run is quantified as a bird 2 km away from the colony for 10 or more timesteps
  myruns <- which(rv$values == TRUE & rv$lengths >= 10)
  
  # Add TripID
  if (is_empty(myruns)) { #For any birds that never left
    
    m_bird$tripID <-NA 
    SeaIx  <- NA
    
    # wrap lon for plotting
    m_bird$lon = wrap360(m_bird$lon)
    # Plot trips in different colors:     
    ggplot(data=m_bird, aes(x=lon, y=lat)) + 
      geom_point(size = 2) +
      geom_path() +
      ggtitle(paste(m_bird$id[1]))
    ggsave(paste0(meta_dir,'gpsfigs_multitrips/',dep_ID,'_trips.png'))
    
    
  } else {
    # Find end index of TRUE run
    runs.lengths.cumsum <- cumsum(rv$lengths)
    ends <- runs.lengths.cumsum[myruns]
    
    # Find start index of TRUE run
    newindex = ifelse(myruns>1, myruns-1, 0)
    starts = runs.lengths.cumsum[newindex] + 1
    if (0 %in% newindex) starts = c(1,starts)
    
    m_bird$tripID<-NA
    for (k in 1:length(starts)) {
      m_bird$tripID[starts[k]:ends[k]]<-paste(m_bird$id[1],"_",as.character(k),sep="")
    }
    
    # Find Indices of On Land Chunks
    ok_atsea <- rv$values == 1 & rv$lengths > 3
    ok_onland <- rv$values == 0 & rv$lengths > 60/df$interval_minutes[i] # Needs to be at sea for at least an hour to be counted
    
    ends <- cumsum(rv$lengths)
    starts <- ends - rv$lengths + 1
    landIx <- data.frame(starts, ends)[ok_onland, ]
    SeaIx  <- data.frame(starts, ends)[ok_atsea, ]
    
    m_bird$lon = wrap360(m_bird$lon) # !! Only Relevant for Midway...but might as well run it for everything...
    # Plot trips in different colors:     
    ggplot(data=m_bird, aes(x=lon, y=lat)) + 
      geom_point(aes(colour = factor(tripID)), size = 2) +
      geom_path() +
      ggtitle(paste(m_bird$id[1]))
    ggsave(paste0(meta_dir,'gpsfigs_multitrips/',dep_ID,'_trips.png'))
    
  }
  
  # If there are no at-sea trips, add the following metadata:
  if (is.null(nrow(SeaIx))) {
    df$ntrips[i]=0
    df$tripcomplete[i]=NA
    df$last_loc_km_from_col[i]=NA
    df$tag_recorded_until_removal[i]=NA
    m_bird$tripID=NA
    m_bird$datetime<-as.character(format(m_bird$datetime)) # safer for writing csv in character format
    m_buff<-m_bird
    
    # Otherwise calculate metadata for each trip:  
  } else {
    
    df$ntrips[i] <- length(unique(m_bird$tripID))-1 # minus one to get rid of "NA"
    
    #if ((SeaIx$ends[nrow(SeaIx)] == nrow(dist_from_col)) & (dist_from_col[nrow(dist_from_col)]>2)) {
    if (dist_from_col[nrow(dist_from_col)]>2) {
      df$tripcomplete[i] <- FALSE
    } else {
      df$tripcomplete[i] <- TRUE
    }
    
    df$last_loc_km_from_col[i] <- round(dist_from_col[nrow(dist_from_col)], digits= 2)# last distance from colony
    
    tagendlocal<-m$ptime_loc[nrow(m)]
    
    if (is.na(RecaptureDateTime)) {
      df$tag_recorded_until_removal[i] <- NA
    }else{
      df$tag_recorded_until_removal[i] <- ifelse(as.numeric(difftime(RecaptureDateTime,tagendlocal,units="mins"))< 120,TRUE,FALSE)
    }
    
    m_bird$datetime<-as.character(format(m_bird$datetime)) # safer for writing csv in character format
    m_buff<-m_bird[!is.na(m_bird$tripID),]
    
  }
  

# Save L1_1 and L1_2 ------------------------------------------------------
  
  write.csv(m_bird,file=paste0(L1_dir,'1_onbird/',dep_ID,"_GPS_L1_1.csv"),row.names=FALSE)
  write.csv(m_buff,file=paste0(L1_dir,'2_buffer2km/',dep_ID,"_GPS_L1_2.csv"),row.names=FALSE)

  rm(list=ls()[! ls() %in% c("Mode", "calculate_speed", "wrap360",
                             "df", "fullmeta", "buffer_dist", "colony_coords", "GD_dir", "gpsfiles",
                             "L0_dir", "L1_dir", "loc_tz", "location", "meta_dir", "szn", "i")])  
} 
# End of Bird Loop



# Write metadata table ----------------------------------------------------

write.csv(df,file=paste0(meta_dir,szn,"_gpsCATLOG_trip-summary.csv"),row.names=FALSE)



