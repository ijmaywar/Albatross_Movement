#############################################################
#
# This function returns the state as determined by an HMM
# (declare which HMM you're using - either 2 or 3 states)
#
#############################################################

HMMstate <- function(m,birdname_trip,states) {
  dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/",states,"_states/")
  HMM_filename <- paste0(str_sub(birdname_trip,end=-3),"_GPS_L3_600s.csv")
  HMM_data <- read.csv(paste0(dir,str_sub(birdname_trip,1,4),"/",HMM_filename))
  HMM_data <- HMM_data %>% filter(trip_ID==birdname_trip)
  return(HMM_data$state)
}

