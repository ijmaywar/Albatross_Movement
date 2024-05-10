################################################################################
#
# Create plots from L2 wind data that show the winds experienced by albatrosses
#
################################################################################
################################################################################
#
# Wind data: create plots with wind data.
# This will need to be edited in order to account for times removed by on-water
# and foraging/resting (HMM)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

locations = c('Bird_Island','Midway')
min_peak_prob = 0 # What was the min_peak_prob used to create summary data?

# Load Packages -----------------------------------------------------------

library(tidyverse)
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

# Set Environment ---------------------------------------------------------

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

# Classify 2BEP, E_pip, Ep as BG
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","BG")))

# Datetime stuff
m_all$datetime <- as.POSIXct(m_all$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# m_all$julian <- m_all$datetime$yday + 1 
# # plotday are the days since the beginning of the year (January 1 of the first
# # year is 1)
# m_all$plotday <- ifelse(m_all$julian > 200, m_all$julian, m_all$julian + 365)
# # adjust for leap years
# m_all$plotday <- ifelse(m_all$plotday > 365 & m_all$datetime$year+1900 == 2021, m_all$plotday+1, m_all$plotday)

# Remove unnecessary columns
# m_all <- m_all %>% dplyr::select(-lon,-lat,-u,-v,-datetime,-wind_dir,-bird_dir,-bird_vel)

# Categorize BWAs
m_all <- m_all %>% mutate(BWA_cat = case_when(bwa<=45 ~ "tail",
                                              bwa>45 & bwa<135 ~ "cross",
                                              bwa>=135 ~ "head"))

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$Field_Season <- as.factor(m_all$Field_Season)
m_all$Location <- as.factor(m_all$Location)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$Species <- as.factor(m_all$Species)
m_all$BWA_cat <- as.factor(m_all$BWA_cat)
m_all$HMM_2S_state <- as.factor(m_all$HMM_2S_state)
m_all$HMM_3S_state <- as.factor(m_all$HMM_3S_state)

# Re-order Species groups
m_all$Species <- factor(m_all$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Split data between species
m_BBAL <- m_all %>% filter(Species=="BBAL")
m_GHAL <- m_all %>% filter(Species=="GHAL")
m_WAAL <- m_all %>% filter(Species=="WAAL")
m_LAAL <- m_all %>% filter(Species=="LAAL")
m_BFAL <- m_all %>% filter(Species=="BFAL")

m_all_nonaflaps <- m_all %>% drop_na(flaps)
m_BBAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BBAL")
m_GHAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="GHAL")
m_WAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="WAAL")
m_BFAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BFAL")
m_LAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="LAAL")

# Downsampling Bird_Island to Midway --------------------------------------

downsampled_ids <- c(sample(unique(m_BBAL_nonaflaps$id),
                            size=length(unique(m_BFAL_nonaflaps$id)),
                            replace=FALSE),
                     sample(unique(m_GHAL_nonaflaps$id),
                            size=length(unique(m_BFAL_nonaflaps$id)),
                            replace=FALSE),
                     sample(unique(m_WAAL_nonaflaps$id),
                            size=length(unique(m_BFAL_nonaflaps$id)),
                            replace=FALSE),
                     sample(unique(m_LAAL_nonaflaps$id),
                            size=length(unique(m_BFAL_nonaflaps$id)),
                            replace=FALSE),
                     unique(m_BFAL_nonaflaps$id))
ds_m_all_nonaflaps <- m_all_nonaflaps %>% filter(id %in% downsampled_ids)

# Box plots -------------------------------------------------------------

m_all_nonaflaps |>
  ggplot(aes(Species,wind_vel)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,30) +
  labs(y="Wind velocity (m/s)",x="Species")

m_all_nonaflaps |>
  ggplot(aes(Species,bwa)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,180) +
  labs(y="Bird-wind angle (degrees)",x="Species")

m_all_nonaflaps %>% filter(HMM_3S_state!=1) |>
  ggplot(aes(Species,flaps)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,2000) +
  labs(y="Flaps/hour",x="Species")


# Downsampled plots

ds_m_all_nonaflaps |>
  ggplot(aes(Species,wind_vel)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,30) +
  labs(y="Wind velocity (m/s)",x="Species")

ds_m_all_nonaflaps |>
  ggplot(aes(Species,bwa)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,180) +
  labs(y="Bird-wind angle (degrees)",x="Species")

ds_m_all_nonaflaps %>% filter(HMM_3S_state!=1) |>
  ggplot(aes(Species,flaps)) +
  geom_violin() + 
  # theme_minimal() +
  ylim(0,2000) +
  labs(y="Flaps/hour",x="Species")

ds_m_all_nonaflaps |>
  ggplot(aes(Species,wind_vel)) +
  geom_boxplot() + 
  theme_minimal()

ds_m_all_nonaflaps |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot() +
  ylim(0,1000) +
  theme_minimal()

ds_m_all_nonaflaps |>
  ggplot(aes(Species,flaps)) +
  geom_violin(draw_quantiles = TRUE) +
  ylim(0,1000) +
  theme_minimal()


# Wind angle plots -------------------------------------------------------------

# set breaks
breaks <- seq(0,360,by=5)
categories <- cut(m_all_nonaflaps$w_rel,breaks,include.lowest=TRUE,right=FALSE)
frequency_table <- table(categories)
data <- as.data.frame(frequency_table)
data$categories <- breaks[-length(breaks)]

polar_plot <- ggplot(data, aes(x = categories, y = Freq)) + #, group=3)) +  #, group = day, color = day)) +
  geom_line() +
  coord_polar(start=0) 
# + scale_y_continuous(limits=c(0,75))

polar_plot

polar_plot_bar <- ggplot(data, aes(x = categories, y = Freq)) +
  # geom_bar(stat = "identity", width = 1, fill = "skyblue") +
  geom_bar(stat="identity", alpha=1, fill="black") + 
  labs(main="Polar plot of wind angle relative to bird heading",y="Frequency",x="") +
  coord_polar(start = 0)  # Adjust the starting angle if needed
# scale_y_continuous(limits=c(0,1200))

polar_plot_bar

