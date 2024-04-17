################################################################################
#
# L2 GPS -> L3 GPS data
# Use Theo Michelot's moveHMM to seperate flight behaviors:
#   1. Area-restricted foraging
#   2. Commuting
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Midway' # Options: 'Bird_Island', 'Midway'
spp = "LAAL"
numstates = "3_states" # "2_states"
  
# Set Environment ---------------------------------------------------------
  
library(moveHMM)
library(dplyr)
library(ggplot2)
library(stringr)

# User functions ----------------------------------------------------------

wrapCor = function(cor) {corWrap<-ifelse(cor>180,cor-360,cor);return(corWrap)}

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}


# Loop thru and process ---------------------------------------------------
  
GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"

if (location == "Bird_Island") {
  compile_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/compiled_2019_2022/compiled_by_spp/")
} else if (location == "Midway") {
  compile_dir <- paste0(GD_dir, "L2/",location,"/Tag_Data/GPS/compiled_2018_2023/compiled_by_spp/")
}
L3_dir <- paste0(GD_dir, "L3/",location,"/Tag_Data/GPS/compiled_all_yrs/",numstates,"/",spp,"/")

# Load data ---------------------------------------------------------------

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

setwd(compile_dir)
gpsfiles<-list.files(pattern='*.csv')

df = read.csv(gpsfiles[which(substr(gpsfiles,1,4)==spp)])

df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
df$lon <- wrapCor(df$lon)

names(df) <- c("bird", "date", "lon",  "lat", "ID" )

df$ID = factor(df$ID)

# filter out trips less than two hours
df <- df %>%
  group_by(ID) %>%
  filter(n() > 12) %>%
  ungroup()

hmmdata = prepData(
  df,
  type = "LL",
  coordNames = c("lon", "lat"),
  LLangle = TRUE
)

# Search for initial parameters -------------------------------------------

# Plot histogram of step lengths
hist(hmmdata$step, xlab = "step length", main = "")

# Plot histogram of turning angles
hist(hmmdata$angle, breaks = seq(-pi, pi, length = 15), xlab = "angle", main = "")

# 2-state model -----------------------------------------------------------

# # Set up initial params
# stepMean0 <- c(0.1,4) #(state1,state2)
# stepSD0 <- c(0.1,4) #(state1,state2)
# stepPar0 <- c(stepMean0,stepSD0)
# 
# angleMean0 <- c(pi,0)
# angleCon0 <- c(1,5)
# anglePar0 <- c(angleMean0,angleCon0)
# 
# # fit the HMM
# m <- fitHMM(data = hmmdata, 
#             nbStates = 2, 
#             stepPar0 = stepPar0, 
#             anglePar0 = anglePar0)


# 3-state model -----------------------------------------------------------

# The 3 proposed states are 1. on-water, 2. foraging, 3. commuting

stepMean0 <- c(0.1,1,4) #(state1,state2,state3)
stepSD0 <- c(0.1,1,4) #(state1,state2,state3)
stepPar0 <- c(stepMean0,stepSD0)

angleMean0 <- c(0,pi,0)
angleCon0 <- c(5,1,5)
anglePar0 <- c(angleMean0,angleCon0)

# fit the HMM
m <- fitHMM(data = hmmdata, 
            nbStates = 3, 
            stepPar0 = stepPar0, 
            anglePar0 = anglePar0)

# Plot the model -----------------------------------------------------------
plot(m, animals = 1, ask = TRUE)

# Get the states -----------------------------------------------------------
states <- viterbi(m)
state_probs <- stateProbs(m)

# Create a df with all of the important info -----------------------------------------------------------
HMMdf <- cbind(df,hmmdata$step,hmmdata$angle,states,state_probs)
names(HMMdf) <- c("dep_ID", "datetime", "lon",  "lat", "trip_ID",
                  "step_length", "angle", "state", "prob_1", "prob_2","prob_3")
HMMdf$state <- factor(HMMdf$state)
HMMdf$datetime <- as.character(format(HMMdf$datetime)) # safer for writing csv in character format

# Save plots and dataframes -----------------------------------------------
birdIDs = unique(str_sub(hmmdata$ID,1,-3))
tripIDs = unique(hmmdata$ID)
for (i in 1:length(birdIDs)) {
  current_bird = birdIDs[i]
  trips = tripIDs[str_sub(tripIDs,1,-3) == birdIDs[i]]
  
  bird_HMMdf <- HMMdf %>% filter(dep_ID == current_bird)
  write.csv(bird_HMMdf, file=paste0(L3_dir,"/",current_bird,"_GPS_L3_600s.csv"), row.names=FALSE)
  
  for (j in 1:length(trips)) {
    current_trip = trips[j]
    trip_df <- HMMdf %>% filter(trip_ID == current_trip)
    ggplot(data = trip_df, aes(x=wrap360(lon), y=lat)) + 
      geom_point(size = 1, aes(color = state)) +
      ggtitle(current_trip) + 
      geom_point(size = 3, x = colony_coords[1], y = colony_coords[2])
    ggsave(paste0(L3_dir,"/","Figures/",current_trip,"_600s_path_with_states.png"))
  }
  
}



# Plots time series showing for each position a binary estimate of whether they are in State 1 or 2 (dot plot, based on a threshold of the probability line graphs plotted below)
# Plots line graphs showing the continuous probability of being in state 1 or 2 (a threshold is used to turn into binary)
# plotStates(m)

###########################
### SAVE OUT TWO STATE MODEL
###########################

# # Add states to the data set #
# df$predictedState <- viterbi(m)
# df$probState1 <- stateProbs(m)[,1]
# df$probState2 <- stateProbs(m)[,2]
# 
# #md24 <- md
# save(df, file = "data-created/2stateHMM_24hr.Rdata")
# 
# migState = df[df$predictedState ==1,]
# resState = df[df$predictedState==2,]
# 
# 
# 
# 
# ##################################################
# ###
# ### Plot turn angle and step size with GPS track
# ###
# ##################################################
# 
# hmmdata$predictedstate = viterbi(m)
# 
# plotdomain = 500:1000
# 
# ggplot(hmmdata[plotdomain,], aes(x=x,y=y)) + 
#   geom_point(size=2,aes(color=abs(angle))) +
#   scale_color_gradient(low="yellow",high="black") +
#   geom_point(x=-38.0658417,y=-54.0101833,color="blue")
# 
# ggplot(hmmdata[plotdomain,], aes(x=x,y=y)) + 
#   geom_point(size=2,aes(color=factor(predictedstate))) +
#   geom_point(x=-38.0658417,y=-54.0101833,color="blue")
# 
# ggplot(hmmdata[plotdomain,], aes(x=x,y=y)) + 
#   geom_point(size=2,aes(color=abs(step))) +
#   scale_color_gradient(low="yellow",high="black") +
#   geom_point(x=-38.0658417,y=-54.0101833,color="blue")
# 







######################################################################################################
### PLOT on Pacific MAP, color code State 1 vs State 2 and overlay Clarion-Clipperton Zone boundaries
## plot selected IDs for Diva's paper with the residence periods shown
#######################################################################################################

## EXAMPLE PLOTTING CODE (cl45 and cczRGDAL were previously loaded geographic layers, but left this in so you can just see some example)
# p <- "+proj=robin +lon_0=-110 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# mapPlot(cl45, projection=p, col='gray88')
# ind = migState
# mapPoints(ind$lon, ind$lat, pch=19, cex=0.15, col=alpha("lightgrey",0.75))
# ind = resState
# mapPoints(ind$lon, ind$lat, pch=19, cex=0.15, col=alpha("darkblue",0.75))

# lines(cczRGDAL, col="red", lwd = 2)

#mapLines(ind$lon, ind$lat, pch=19, cex=0.25, col=alpha("darkblue"))



