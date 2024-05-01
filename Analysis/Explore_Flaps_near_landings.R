################################################################################
#
# Explore flaps during landing and takeoff relative to the avg flap rate
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

locations = c('Bird_Island','Midway')
min_peak_prob = 0 # What was the min_peak_prob used to create summary data?
int_dur_sec <- 30 # What is the amount of time we're looking at before and after GLS=="wet"?

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(readxl)
library(lme4)
library(stringr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(gridExtra)
library(patchwork)
library(gratia)
library(readr)

# Set up master data sheet ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"

if (min_peak_prob == 0) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_0/")
} else if (min_peak_prob == 0.85) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_085/")
}

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(read_dir)
files <- list.files(pattern = '*.csv')
m_all <- read_csv(files[1])

# Download flaps_near_landings data and summarize ------------------------------

FNL_dir <- paste0(GD_dir,"Analysis/Maywar/Flaps_near_landings/",as.character(int_dur_sec),"s/Bird_Island/")
FNL_list <- list.files(path = FNL_dir, full.names = TRUE, recursive = TRUE, pattern='*.csv')

n_GLS_birds <- length(FNL_list)
m <- data.frame(Deployment_ID = character(n_GLS_birds))
m$landing_flaps <- vector("list",length=nrow(m))
m$takeoff_flaps <- vector("list",length=nrow(m))
m$hourly_flaps <- vector("list",length=nrow(m))

# Loop thru each bird
for (i in 1:n_GLS_birds) {
  m_FNL <- read_csv(FNL_list[i])
  birdname <- str_sub(FNL_list[i],-46,-29)
  m_FNL$starttime <- as.POSIXct(m_FNL$starttime, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  m_FNL$endtime <- as.POSIXct(m_FNL$endtime, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  m_FNL$before_dt <- as.POSIXct(m_FNL$before_dt, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  m_FNL$after_dt <- as.POSIXct(m_FNL$after_dt, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  
  m$Deployment_ID[i] <- birdname
  m$landing_flaps[[i]] <- na.omit(m_FNL$flaps_before)
  m$takeoff_flaps[[i]] <- na.omit(m_FNL$flaps_after)
  
  current_hourly_df <- m_all %>% filter(id == birdname,GLS_state=="dry")
  m$hourly_flaps[[i]] <- na.omit(current_hourly_df$flaps)
}

# Run stats on m ---------------------------------------------------------------

m <- m %>% mutate(mean_Landing_flap_rate = sapply(m$landing_flaps,mean)/(int_dur_sec), # This is flaps/hour
                  mean_Takeoff_flap_rate = sapply(m$takeoff_flaps,mean)/(int_dur_sec), # flaps/hour
                  mean_Hourly_flap_rate = sapply(m$hourly_flaps,mean), # flaps/hour 
                  total_Landing_flaps = sapply(m$landing_flaps,sum),
                  total_Takeoff_flaps = sapply(m$takeoff_flaps,sum),
                  total_Hourly_flaps = sapply(m$hourly_flaps,sum),
                  num_Landings = sapply(m$landing_flaps,length),
                  num_Takeoffs = sapply(m$takeoff_flaps,length))
                  # ADD A METRIC FOR NUM LANDINGS / TOTAL DURATION OF DATA
                  # WHY NUM_LANDINGS != NUM_TAKEOFFS?

m$Species <- substr(m$Deployment_ID,1,4)
m$Species <- as.factor(m$Species)

# Stats across all landings across all individuals
grand_mean_Landing_flap_rate <- mean(unlist(m$landing_flaps))/int_dur_sec # This is flaps/hour -> change this to flaps in the 30 seconds
grand_mean_Takeoff_flap_rate <- mean(unlist(m$takeoff_flaps))/int_dur_sec # This is flaps/hour -> change this to flaps in the 30 seconds.
grand_mean_Hourly_flap_rate <- mean(unlist(m$hourly_flaps)) # This is flaps/hour
grand_prop_flaps_Landings <- sum(m$total_Landing_flaps)/sum(m$total_Hourly_flaps)
grand_prop_flaps_Takeoff <- sum(m$total_Takeoff_flaps)/sum(m$total_Hourly_flaps)
grand_prop_flaps_Either <- (sum(m$total_Landing_flaps)+sum(m$total_Takeoff_flaps))/sum(m$total_Hourly_flaps)
grand_mean_num_Landings <- mean(m$num_Landings)
grand_mean_num_Takeoffs <- mean(m$num_Takeoffs)

grand_mean_Landing_flap_rate
grand_mean_Takeoff_flap_rate 
grand_mean_Hourly_flap_rate
grand_prop_flaps_Landings
grand_prop_flaps_Takeoff
grand_prop_flaps_Either
grand_mean_num_Landings
grand_mean_num_Takeoffs

ggplot(m,aes(x=Species,y=mean_Landing_flap_rate)) +
  geom_boxplot()

ggplot(m,aes(x=Species,y=mean_Takeoff_flap_rate)) +
  geom_boxplot()
