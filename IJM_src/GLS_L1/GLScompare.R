################################################################################
#
# Comparing GLS on-water to HMM state 1
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
spp = "BBAL"
interp = "600s"

# Set Environment ---------------------------------------------------------

library(dplyr)
library(ggplot2)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/"
GLS_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GLS/", szn, "/")
HMM_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L3/Bird_Island/Tag_Data/GPS/", interp, "/", spp, "/")

setwd(GLS_dir)
allGLSfiles <- list.files(pattern='*.csv')
GLSfiles <- allGLSfiles[grep(paste0("^", spp), allGLSfiles)]

# User functions ----------------------------------------------------------


# Loop thru and process ---------------------------------------------------

for (i in 1:length(GLSfiles)) {
  m <- read.csv(GLSfiles[i])
  
}


