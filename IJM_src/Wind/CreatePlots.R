################################################################################
#
# Wind data: create plots with wind data.
# This will need to be edited in order to account for times removed by on-water
# and foraging/resting (HMM)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
interp = "600s"

# Load Packages -----------------------------------------------------------

library(ggplot2)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
wind_L2_dir <- paste0(GD_dir,"L2/",location,"/Wind_Data/",szn,"/",interp,"/")
wind_L3_dir <- paste0(GD_dir,"L3/",location,"/Wind_Data/",szn,"/",interp,"/")

setwd(wind_L3_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_wind_and_flaps.csv$","",files)

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  if (i==1) {
    m_all <- m
  } else {
    m_all <- rbind(m_all,m)
  }
  
}

# Wind angle plots --------------------------------------------------------------

# set breaks
breaks <- seq(0,360,by=5)
categories <- cut(m_all$w_rel,breaks,include.lowest=TRUE,right=FALSE)
frequency_table <- table(categories)
data <- as.data.frame(frequency_table)
data$categories <- breaks[-length(breaks)]

polar_plot <- ggplot(data, aes(x = categories, y = Freq)) + #, group=3)) +  #, group = day, color = day)) +
  geom_line() +
  coord_polar(start=0) +
  scale_y_continuous(limits=c(0,1300))
             
polar_plot

polar_plot_bar <- ggplot(data, aes(x = categories, y = Freq)) +
  # geom_bar(stat = "identity", width = 1, fill = "skyblue") +
  geom_bar(stat="identity", alpha=0.5, fill="blue") + 
  coord_polar(start = 0)  # Adjust the starting angle if needed
  # scale_y_continuous(limits=c(0,1200))
  
polar_plot_bar



# Flap plots --------------------------------------------------------------
m_selectedWinds <- filter(m_all,bwa>60)
WS_breaks <- seq(0,max(m_selectedWinds$wind_vel))
WS_categories <- cut(m_selectedWinds$wind_vel,WS_breaks,include.lowest=TRUE,right=FALSE)
# count flaps within these categories!
WS_freqtable <- table(WS_categories)
WS_data <- as.data.frame(WS_freqtable)

windspeed_flap_fig <- ggplot(m_selectedWinds, aes(x=wind_vel,y=flaps)) +
  geom_line()
windspeed_flap_fig

# chunk wind vel and average flaps?

