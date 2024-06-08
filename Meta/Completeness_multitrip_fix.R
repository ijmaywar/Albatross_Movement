################################################################################
#
# Ammend completeness for multi-trip birds
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(foreach)

# Set Envrionment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
completeness_dir <- paste0(GD_dir,"/metadata/Tag_completeness_allignment/")

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))
incomplete_birds <- fullmeta %>% filter(Full_dur==0)

setwd(completeness_dir)
completeness_files <- list.files(pattern='*.csv')

# Create compiled file
for (i in 1:length(completeness_files)) {
  mi <- read_csv(completeness_files[i])
  if (i==1) {
    m <- mi
  } else {
    m <- rbind(m,mi)
  }
}
