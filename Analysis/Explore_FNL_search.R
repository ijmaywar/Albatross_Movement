################################################################################
#
# Work thru and visualize FNL_search data
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

locations = c('Bird_Island','Midway')
min_peak_prob = 0 # What was the min_peak_prob used to create summary data?

# User Defined Functions -----------------------------------------------------

convert_to_POSIXct <- function(x) {
  as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="GMT")
}

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

FNL_dir <- paste0(GD_dir,"Analysis/Maywar/FNL_search/")
FNL_list <- list.files(path = FNL_dir, full.names = TRUE, recursive = TRUE, pattern='*.csv')

bird_list <- list()

# Loop thru each bird
for (i in 1:length(FNL_list)) {
  
  birdname <- str_sub(FNL_list[i],-33,-16)
  # Remove NRL tags bc they are not as accurate time-wise
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
  if (birdmeta$Aux_TagCat == "NRL") {
    next
  }
  
  landing_flaps <- list()
  takeoff_flaps <- list()
  landing_flaps_NoOvr <- list()
  takeoff_flaps_NoOvr <- list()
  
  m_FNL <- read_csv(FNL_list[i])
  m_FNL$starttime <- as.POSIXct(m_FNL$starttime, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  m_FNL$endtime <- as.POSIXct(m_FNL$endtime, format="%Y-%d-%m %H:%M:%S", tz="GMT")
  
  m_FNL <- m_FNL %>% mutate(across(starts_with("before_dt_"), convert_to_POSIXct))
  
  m_FNL <- m_FNL %>% mutate(across(starts_with("after_dt_"), convert_to_POSIXct))
  
  # Remove 5 second landings
  m_FNL <- m_FNL %>% filter(duration_sec > 5)
    
  for (j in 1:60) {
    
    landing_flaps[[j]] <- na.omit(m_FNL[[paste0("flaps_before_",as.character(j))]])
    takeoff_flaps[[j]] <- na.omit(m_FNL[[paste0("flaps_after_",as.character(j))]])
    
    landing_flaps_NoOvr[[j]] <- na.omit(m_FNL[[paste0("flaps_before_",as.character(j),"_NoOvr")]])
    takeoff_flaps_NoOvr[[j]] <- na.omit(m_FNL[[paste0("flaps_after_",as.character(j),"_NoOvr")]])

  }
  
  bird_list[[sym(birdname)]]$landing_flaps <- landing_flaps
  bird_list[[sym(birdname)]]$takeoff_flaps <- takeoff_flaps
  bird_list[[sym(birdname)]]$landing_flaps_NoOvr <- landing_flaps_NoOvr
  bird_list[[sym(birdname)]]$takeoff_flaps_NoOvr <- takeoff_flaps_NoOvr
  
  current_hourly_df <- m_all %>% filter(id == birdname,GLS_state=="dry")
  bird_list[[sym(birdname)]]$hourly_flaps <- na.omit(current_hourly_df$flaps)
  
}

# Run stats on bird_list -------------------------------------------------------
m <- data.frame(landing_flaps = numeric(60),
                   takeoff_flaps = numeric(60),
                   landing_flaps_NoOvr = numeric(60),
                   takeoff_flaps_NoOvr = numeric(60),
                   num_landings = numeric(60),
                   num_takeoffs = numeric(60),
                   num_landings_NoOvr = numeric(60),
                   num_takeoffs_NoOvr = numeric(60))
fig_list <- list()

for (spp in c("BBAL","GHAL","WAAL")) {
# for (spp in c("BBAL","GHAL")) {
  for (k in which(substr(names(bird_list),1,4) == spp)) {
    for (l in 1:60) {
      m$landing_flaps[l] <- m$landing_flaps[l] + sum(bird_list[[k]]$landing_flaps[[l]])
      m$takeoff_flaps[l] <- m$takeoff_flaps[l] + sum(bird_list[[k]]$takeoff_flaps[[l]])
      m$landing_flaps_NoOvr[l] <- m$landing_flaps_NoOvr[l] + sum(bird_list[[k]]$landing_flaps_NoOvr[[l]])
      m$takeoff_flaps_NoOvr[l] <- m$takeoff_flaps_NoOvr[l] + sum(bird_list[[k]]$takeoff_flaps_NoOvr[[l]])
      m$num_landings[l] <- m$num_landings[l] + length(bird_list[[k]]$landing_flaps[[l]])
      m$num_takeoffs[l] <- m$num_takeoffs[l] + length(bird_list[[k]]$takeoff_flaps[[l]])
      m$num_landings_NoOvr[l] <- m$num_landings_NoOvr[l] + length(bird_list[[k]]$landing_flaps_NoOvr[[l]])
      m$num_takeoffs_NoOvr[l] <- m$num_takeoffs_NoOvr[l] + length(bird_list[[k]]$takeoff_flaps_NoOvr[[l]])
    }
  }
  fig_list[[spp]] <- m
}


# Plot some stuff---------------------------------------------------------------

for (spp in c("BBAL","GHAL","WAAL")) {
  fig_landings <- ggplot(fig_list[[spp]],aes(x=(1:60),y=(landing_flaps/num_landings)/(1:60))) +
    geom_point() +
    labs(y = "flaps per second", x = "window in seconds") + 
    ggtitle(paste(spp,"landings")) +
    ylim(0,0.6)
  
  fig_takeoffs <- ggplot(fig_list[[spp]],aes(x=(1:60),y=(takeoff_flaps/num_takeoffs)/(1:60))) +
    geom_point() +
    labs(y = "flaps per second", x = "window in seconds") + 
    ggtitle(paste(spp,"takeoffs")) +
    ylim(0,0.6)
  
  fig_landings_NoOvr <- ggplot(fig_list[[spp]],aes(x=(1:60),y=(landing_flaps_NoOvr/num_landings_NoOvr)/(1:60))) +
    geom_point() +
    labs(y = "flaps per second", x = "window in seconds") + 
    ggtitle(paste(spp,"landings (no overlap)")) +
    ylim(0,0.6)
  
  fig_takeoffs_NoOvr <- ggplot(fig_list[[spp]],aes(x=(1:60),y=(takeoff_flaps_NoOvr/num_takeoffs_NoOvr)/(1:60))) +
    geom_point() +
    labs(y = "flaps per second", x = "window in seconds") + 
    ggtitle(paste(spp,"takeoffs (no overlap")) +
    ylim(0,0.6)
  
  grid.arrange(fig_landings,fig_takeoffs,fig_landings_NoOvr,fig_takeoffs_NoOvr,nrow=2)
}


# Flaps don't seem to be on the order of 3 Hz as the literature suggests.
# I think this may be because we cannot assume such precision in our data.
# It's likely that the GLS isn't accurate to the second (which it's not,
# it's supposed to be at max accurate to 6 seconds). But even so, there is 
# drift which would cause the GLS to not allign with the Acc.

# At most, there's 0.45 flaps per second during takeoffs and 
# 0.275 flaps per second during landings

