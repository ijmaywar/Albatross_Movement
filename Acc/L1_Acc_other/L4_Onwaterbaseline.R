################################################################################
#
# Testing onwaterbaseline
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)
library(readr)
library(zoo)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
Acc_dir <- paste0(GD_dir, "L1/",location,"/Tag_Data/Acc/Acc_Technosmart/",szn,"/")
GLS_dir <- paste0(GD_dir,"L1/",location,"/Tag_Data/Immersion/GLS/",szn,"/")
write_dir <- paste0(GD_dir,"L4/",location,"/Tag_Data/Acc/")

setwd(Acc_dir)
acc_files <- list.files(pattern='*.csv')

# I use this times 2 in MATLAB code.
onwaterBaseline <- 2.5e-04

for (i in 1:length(acc_files)) {
  m <- read_csv(acc_files[i])
  
  # Find variance using 2 min moving window
  Mvar_Az <- rollapply(m$Az, width = 2*(60*25), FUN = var, na.pad = TRUE)
  
  
  
}


