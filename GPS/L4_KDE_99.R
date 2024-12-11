# Ian's version of Dallas Jordan's Master KDE Script

# This script is most up to date and changes randomization procedure to permute cell values rather than the labels
# This script: 
# 1. Loads post-processed data, converts to SpatialPointsDataFrame
# 2. Creates KDEs using the adehabitatHR package for each individual, saves as "results"
# 3. Does not normalize these for the relevant group (there are 5 total, LAAL/BFAL for each island and combined) - 
#    we are dealing with probability densities, don't need to be normalized
# 4. Averages each group to output a single raster. 
# 5. Assigns these estUD objects to coded names for overlap comparisons, e.g. lm_averaged_estUD = LAALmidway
# 6. Makes estUDm objects for each comparison (combines estUD objects pairwise according to the comparison of interest)
# 7. Calculates overlap of 95 UD, 50 UD, 95 PHR, 50 PHR, 95 BA, and 50 BA for each comparison of original data
# 8. Randomization procedure to test for significance

####### IMPORTANT NOTES, in no particular order. Especially important are notes concerning bandwidth, 'h'. 

# no need for asymptote analysis because of Gutowsky paper that Melinda found

# Normalization not needed when dealing with probability density UDs

# Brownian bridge UDs are newer, but they make assumptions about staying at foraging spots that 
# aren't necessarily true here

# smoothing parameter 'h' is in the units of your relocations - here, I have projected into meters, so smoothing parameter should
# be in meters. The UD is estimated at the center of each pixel of a grid - adehabitatHR vignette. I set 150km 'h', because I am assuming that each kernel is smoothed from the center
# of a grid, so 150km in each direction = 300km. From the vignette - the size and resolution of the grid does not have a large effect on the estimates

# Concerning the value of h/bandwidth/search window - Low values of h give nearby locations the greatest influence on the shape of the kernel, revealing small-scale detail, while large 
# values allow more influence from distant locations, which reveals the outlying shape of the distribution

# LSCV only works when you have frequent and NONAUTOCORRELATED (Row, 2006) location data and there aren't many 'islands' of relocation. It
# is calculated by minimizing the error by comparing the prediction from all data points to the data minus each point.
# More about better smoothing parameters: https://vita.had.co.nz/papers/density-estimation.pdf

# href tends to estimate a larger home range than other methods (oversmoothing). However, we aren't interested
# in the size of the home range here; rather we are interested in the relative use. So to meet our needs, we just need 
# to pick a consistent bandwidth that will highlight different densities of use across space. We pick a consistent bandwidth
# because it will assume that space use is uniform (no area of the ocean gets a bigger search window to boost its density)

# href bandwidth for my dataset is about 690km - this is pretty big. Chapman pg. 44 - honestly, subjective choice can 
# be best. "A natural method for choosing the smoothing parameter is to plot out several curves and choose the estimate that is most
# in accordance with one's prior ideas about the density". 

# Furthermore on this point, "The reference bandwidth supposes that
# the UD is a bivariate normal distribution, which is disputable in most ecological
# studies. When the animals uses several centers of activity, the reference smoothing parameter is often too large, which results 
# into a strong oversmoothing of the data (the estimated UD predicts the frequent presence of the animal in areas
# which are not actually used)" - adehabitatHR vignette

# In general, home range analyses are rarely documented with enough detail to reproduce them or to allow for comparative studies. 
# At a minimum, h should be specified and it should be mentioned what method was used to calculate it. 


# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

loc = 'Bird_Island'

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(adehabitatHR)
library(sp)
library(readxl)
library(sf)
library(raster)
library(terra)


# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
write_dir <- paste0(GD_dir,"L4/",loc,"/Tag_Data/GPS/")
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))
if (loc == "Bird_Island") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2019_2022/compiled_complete/")
} else if (loc == "Midway") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2018_2023/compiled_complete/")
}

all_data <- read_csv(paste0(GPS_dir,loc,"_Compiled_600s_compiled_complete.csv"))
all_data$datetime <- as.POSIXlt(all_data$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# Split all_data into spp and Trip Types
if (loc == "Bird_Island") {
  BBAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="BBAL") & (TripType=="Inc"))
  BBAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="BBAL") & (TripType=="BG"))
  BBAL_all_data <- all_data %>% filter((substr(all_data$id,1,4)=="BBAL"))
  GHAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="GHAL") & (TripType=="Inc"))
  GHAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="GHAL") & (TripType=="BG"))
  GHAL_all_data <- all_data %>% filter((substr(all_data$id,1,4)=="GHAL"))
  WAAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="WAAL") & (TripType=="Inc"))
  WAAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="WAAL") & (TripType=="BG"))
  WAAL_all_data <- all_data %>% filter((substr(all_data$id,1,4)=="WAAL"))
} else if (loc == "Midway") {
  BFAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="BFAL") & (TripType=="Inc"))
  BFAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="BFAL") & (TripType=="BG"))
  BFAL_all_data <- all_data %>% filter((substr(all_data$id,1,4)=="BFAL"))
  LAAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="LAAL") & (TripType=="Inc"))
  LAAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="LAAL") & (TripType=="BG"))
  LAAL_all_data <- all_data %>% filter((substr(all_data$id,1,4)=="LAAL"))
}

# Set utmzone depending on location
if (loc=="Bird_Island") {
  utmzone <- "24"
} else if (loc == "Midway") {
  utmzone <- "1"
}

# Create estUDm objects --------------------------------------------------------

if (loc == "Bird_Island") {
  
  # Create BBAL Inc Href Kernel
  BBAL_Inc.sp <- BBAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(BBAL_Inc.sp) <- sp::CRS("epsg:4326")
  BBAL_Inc.sp <- spTransform(BBAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_Inc.Href.kernel <- kernelUD(BBAL_Inc.sp, h="href", same4all = TRUE, grid=250, extent=5)
  
  # Create BBAL BG Href Kernel
  BBAL_BG.sp <- BBAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_BG.sp) <- c("lon","lat")
  sp::proj4string(BBAL_BG.sp) <- sp::CRS("epsg:4326")
  BBAL_BG.sp <- spTransform(BBAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_BG.Href.kernel <- kernelUD(BBAL_BG.sp, h="href", same4all = TRUE, grid=250, extent=5)
  
  # Create BBAL all Href Kernel
  BBAL_all.sp <- BBAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_all.sp) <- c("lon","lat")
  sp::proj4string(BBAL_all.sp) <- sp::CRS("epsg:4326")
  BBAL_all.sp <- spTransform(BBAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_all.Href.kernel <- kernelUD(BBAL_all.sp, h="href", same4all = TRUE, grid=250, extent=5)
  
  # Create GHAL Inc Href Kernel
  GHAL_Inc.sp <- GHAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(GHAL_Inc.sp) <- sp::CRS("epsg:4326")
  GHAL_Inc.sp <- spTransform(GHAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_Inc.Href.kernel <- kernelUD(GHAL_Inc.sp, h="href", same4all = TRUE, grid=500)
  
  # Create GHAL BG Href Kernel
  GHAL_BG.sp <- GHAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_BG.sp) <- c("lon","lat")
  sp::proj4string(GHAL_BG.sp) <- sp::CRS("epsg:4326")
  GHAL_BG.sp <- spTransform(GHAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_BG.Href.kernel <- kernelUD(GHAL_BG.sp, h="href", same4all = TRUE, grid=500)
  
  # Create GHAL all Href Kernel
  GHAL_all.sp <- GHAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_all.sp) <- c("lon","lat")
  sp::proj4string(GHAL_all.sp) <- sp::CRS("epsg:4326")
  GHAL_all.sp <- spTransform(GHAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_all.Href.kernel <- kernelUD(GHAL_all.sp, h="href", same4all = TRUE, grid=500)
  
  # Create WAAL Inc Href Kernel
  WAAL_Inc.sp <- WAAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(WAAL_Inc.sp) <- sp::CRS("epsg:4326")
  WAAL_Inc.sp <- spTransform(WAAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_Inc.Href.kernel <- kernelUD(WAAL_Inc.sp, h="href", same4all = TRUE, grid=500)
  
  # Create WAAL BG Href Kernel
  WAAL_BG.sp <- WAAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_BG.sp) <- c("lon","lat")
  sp::proj4string(WAAL_BG.sp) <- sp::CRS("epsg:4326")
  WAAL_BG.sp <- spTransform(WAAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_BG.Href.kernel <- kernelUD(WAAL_BG.sp, h="href", same4all = TRUE, grid=500)
  
  # Create WAAL all Href Kernel
  WAAL_all.sp <- WAAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_all.sp) <- c("lon","lat")
  sp::proj4string(WAAL_all.sp) <- sp::CRS("epsg:4326")
  WAAL_all.sp <- spTransform(WAAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_all.Href.kernel <- kernelUD(WAAL_all.sp, h="href", same4all = TRUE, grid=500)
  
} else if (loc == "Midway") {
  
  # Create BFAL Inc Href Kernel
  BFAL_Inc.sp <- BFAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(BFAL_Inc.sp) <- sp::CRS("epsg:4326")
  BFAL_Inc.sp <- spTransform(BFAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_Inc.Href.kernel <- kernelUD(BFAL_Inc.sp, h="href", same4all = TRUE, grid=500)
  
  # Create BFAL BG Href Kernel
  BFAL_BG.sp <- BFAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_BG.sp) <- c("lon","lat")
  sp::proj4string(BFAL_BG.sp) <- sp::CRS("epsg:4326")
  BFAL_BG.sp <- spTransform(BFAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_BG.Href.kernel <- kernelUD(BFAL_BG.sp, h="href", same4all = TRUE, grid=500)
  
  # Create BFAL all Href Kernel
  BFAL_all.sp <- BFAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_all.sp) <- c("lon","lat")
  sp::proj4string(BFAL_all.sp) <- sp::CRS("epsg:4326")
  BFAL_all.sp <- spTransform(BFAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_all.Href.kernel <- kernelUD(BFAL_all.sp, h="href", same4all = TRUE, grid=500)
  
  # Create LAAL Inc Href Kernel
  LAAL_Inc.sp <- LAAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(LAAL_Inc.sp) <- sp::CRS("epsg:4326")
  LAAL_Inc.sp <- spTransform(LAAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_Inc.Href.kernel <- kernelUD(LAAL_Inc.sp, h="href", same4all = TRUE, grid=500)
  
  # Create LAAL BG Href Kernel
  LAAL_BG.sp <- LAAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_BG.sp) <- c("lon","lat")
  sp::proj4string(LAAL_BG.sp) <- sp::CRS("epsg:4326")
  LAAL_BG.sp <- spTransform(LAAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_BG.Href.kernel <- kernelUD(LAAL_BG.sp, h="href", same4all = TRUE, grid=500)
  
  # Create LAAL all Href Kernel
  LAAL_all.sp <- LAAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_all.sp) <- c("lon","lat")
  sp::proj4string(LAAL_all.sp) <- sp::CRS("epsg:4326")
  LAAL_all.sp <- spTransform(LAAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_all.Href.kernel <- kernelUD(LAAL_all.sp, h="href", same4all = TRUE, grid=500)
}

# Average KDE ------------------------------------------------------------------
# average KDE of individuals within a species at a study site#

if (loc=="Bird_Island") {
  
  # Average KDE of BBAL_Inc
  BBAL_Inc_holder <- numeric(length = nrow(BBAL_Inc.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BBAL_Inc_data$tripID))) {
    if (sum(is.na(BBAL_Inc.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BBAL_Inc.Href.kernel[[i]]@data$ud[is.na(BBAL_Inc.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BBAL_Inc.Href.kernel[[i]]@data$ud
    BBAL_Inc_holder <- BBAL_Inc_holder+add
  }
  BBAL_Inc_holder <- BBAL_Inc_holder/length(BBAL_Inc.Href.kernel)
  
  BBAL_Inc_avg_estUD <- BBAL_Inc.Href.kernel[[1]]
  BBAL_Inc_avg_estUD@data$ud <- BBAL_Inc_holder
  
  if (sum(is.na(BBAL_Inc_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BBAL_Inc_avg_estUD@data$ud[is.na(BBAL_Inc_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of BBAL_BG
  BBAL_BG_holder <- numeric(length = nrow(BBAL_BG.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BBAL_BG_data$tripID))) {
    if (sum(is.na(BBAL_BG.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BBAL_BG.Href.kernel[[i]]@data$ud[is.na(BBAL_BG.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BBAL_BG.Href.kernel[[i]]@data$ud
    BBAL_BG_holder <- BBAL_BG_holder+add
  }
  BBAL_BG_holder <- BBAL_BG_holder/length(BBAL_BG.Href.kernel)
  
  BBAL_BG_avg_estUD <- BBAL_BG.Href.kernel[[1]]
  BBAL_BG_avg_estUD@data$ud <- BBAL_BG_holder
  
  if (sum(is.na(BBAL_BG_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BBAL_BG_avg_estUD@data$ud[is.na(BBAL_BG_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of BBAL_all
  BBAL_all_holder <- numeric(length = nrow(BBAL_all.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BBAL_all_data$tripID))) {
    if (sum(is.na(BBAL_all.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BBAL_all.Href.kernel[[i]]@data$ud[is.na(BBAL_all.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BBAL_all.Href.kernel[[i]]@data$ud
    BBAL_all_holder <- BBAL_all_holder+add
  }
  BBAL_all_holder <- BBAL_all_holder/length(BBAL_all.Href.kernel)
  
  BBAL_all_avg_estUD <- BBAL_all.Href.kernel[[1]]
  BBAL_all_avg_estUD@data$ud <- BBAL_all_holder
  
  if (sum(is.na(BBAL_all_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BBAL_all_avg_estUD@data$ud[is.na(BBAL_all_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of GHAL_Inc
  GHAL_Inc_holder <- numeric(length = nrow(GHAL_Inc.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(GHAL_Inc_data$tripID))) {
    if (sum(is.na(GHAL_Inc.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      GHAL_Inc.Href.kernel[[i]]@data$ud[is.na(GHAL_Inc.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- GHAL_Inc.Href.kernel[[i]]@data$ud
    GHAL_Inc_holder <- GHAL_Inc_holder+add
  }
  GHAL_Inc_holder <- GHAL_Inc_holder/length(GHAL_Inc.Href.kernel)
  
  GHAL_Inc_avg_estUD <- GHAL_Inc.Href.kernel[[1]]
  GHAL_Inc_avg_estUD@data$ud <- GHAL_Inc_holder
  
  if (sum(is.na(GHAL_Inc_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    GHAL_Inc_avg_estUD@data$ud[is.na(GHAL_Inc_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of GHAL_BG
  GHAL_BG_holder <- numeric(length = nrow(GHAL_BG.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(GHAL_BG_data$tripID))) {
    if (sum(is.na(GHAL_BG.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      GHAL_BG.Href.kernel[[i]]@data$ud[is.na(GHAL_BG.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- GHAL_BG.Href.kernel[[i]]@data$ud
    GHAL_BG_holder <- GHAL_BG_holder+add
  }
  GHAL_BG_holder <- GHAL_BG_holder/length(GHAL_BG.Href.kernel)
  
  GHAL_BG_avg_estUD <- GHAL_BG.Href.kernel[[1]]
  GHAL_BG_avg_estUD@data$ud <- GHAL_BG_holder
  
  if (sum(is.na(GHAL_BG_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    GHAL_BG_avg_estUD@data$ud[is.na(GHAL_BG_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of GHAL_all
  GHAL_all_holder <- numeric(length = nrow(GHAL_all.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(GHAL_all_data$tripID))) {
    if (sum(is.na(GHAL_all.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      GHAL_all.Href.kernel[[i]]@data$ud[is.na(GHAL_all.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- GHAL_all.Href.kernel[[i]]@data$ud
    GHAL_all_holder <- GHAL_all_holder+add
  }
  GHAL_all_holder <- GHAL_all_holder/length(GHAL_all.Href.kernel)
  
  GHAL_all_avg_estUD <- GHAL_all.Href.kernel[[1]]
  GHAL_all_avg_estUD@data$ud <- GHAL_all_holder
  
  if (sum(is.na(GHAL_all_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    GHAL_all_avg_estUD@data$ud[is.na(GHAL_all_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of WAAL_Inc
  WAAL_Inc_holder <- numeric(length = nrow(WAAL_Inc.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(WAAL_Inc_data$tripID))) {
    if (sum(is.na(WAAL_Inc.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      WAAL_Inc.Href.kernel[[i]]@data$ud[is.na(WAAL_Inc.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- WAAL_Inc.Href.kernel[[i]]@data$ud
    WAAL_Inc_holder <- WAAL_Inc_holder+add
  }
  WAAL_Inc_holder <- WAAL_Inc_holder/length(WAAL_Inc.Href.kernel)
  
  WAAL_Inc_avg_estUD <- WAAL_Inc.Href.kernel[[1]]
  WAAL_Inc_avg_estUD@data$ud <- WAAL_Inc_holder
  
  if (sum(is.na(WAAL_Inc_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    WAAL_Inc_avg_estUD@data$ud[is.na(WAAL_Inc_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of WAAL_BG
  WAAL_BG_holder <- numeric(length = nrow(WAAL_BG.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(WAAL_BG_data$tripID))) {
    if (sum(is.na(WAAL_BG.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      WAAL_BG.Href.kernel[[i]]@data$ud[is.na(WAAL_BG.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- WAAL_BG.Href.kernel[[i]]@data$ud
    WAAL_BG_holder <- WAAL_BG_holder+add
  }
  WAAL_BG_holder <- WAAL_BG_holder/length(WAAL_BG.Href.kernel)
  
  WAAL_BG_avg_estUD <- WAAL_BG.Href.kernel[[1]]
  WAAL_BG_avg_estUD@data$ud <- WAAL_BG_holder
  
  if (sum(is.na(WAAL_BG_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    WAAL_BG_avg_estUD@data$ud[is.na(WAAL_BG_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of WAAL_all
  WAAL_all_holder <- numeric(length = nrow(WAAL_all.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(WAAL_all_data$tripID))) {
    if (sum(is.na(WAAL_all.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      WAAL_all.Href.kernel[[i]]@data$ud[is.na(WAAL_all.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- WAAL_all.Href.kernel[[i]]@data$ud
    WAAL_all_holder <- WAAL_all_holder+add
  }
  WAAL_all_holder <- WAAL_all_holder/length(WAAL_all.Href.kernel)
  
  WAAL_all_avg_estUD <- WAAL_all.Href.kernel[[1]]
  WAAL_all_avg_estUD@data$ud <- WAAL_all_holder
  
  if (sum(is.na(WAAL_all_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    WAAL_all_avg_estUD@data$ud[is.na(WAAL_all_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
} else if (loc=="Midway") {
  
  # Average KDE of BFAL_Inc
  BFAL_Inc_holder <- numeric(length = nrow(BFAL_Inc.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BFAL_Inc_data$tripID))) {
    if (sum(is.na(BFAL_Inc.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BFAL_Inc.Href.kernel[[i]]@data$ud[is.na(BFAL_Inc.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BFAL_Inc.Href.kernel[[i]]@data$ud
    BFAL_Inc_holder <- BFAL_Inc_holder+add
  }
  BFAL_Inc_holder <- BFAL_Inc_holder/length(BFAL_Inc.Href.kernel)
  
  BFAL_Inc_avg_estUD <- BFAL_Inc.Href.kernel[[1]]
  BFAL_Inc_avg_estUD@data$ud <- BFAL_Inc_holder
  
  if (sum(is.na(BFAL_Inc_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BFAL_Inc_avg_estUD@data$ud[is.na(BFAL_Inc_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of BFAL_BG
  BFAL_BG_holder <- numeric(length = nrow(BFAL_BG.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BFAL_BG_data$tripID))) {
    if (sum(is.na(BFAL_BG.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BFAL_BG.Href.kernel[[i]]@data$ud[is.na(BFAL_BG.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BFAL_BG.Href.kernel[[i]]@data$ud
    BFAL_BG_holder <- BFAL_BG_holder+add
  }
  BFAL_BG_holder <- BFAL_BG_holder/length(BFAL_BG.Href.kernel)
  
  BFAL_BG_avg_estUD <- BFAL_BG.Href.kernel[[1]]
  BFAL_BG_avg_estUD@data$ud <- BFAL_BG_holder
  
  if (sum(is.na(BFAL_BG_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BFAL_BG_avg_estUD@data$ud[is.na(BFAL_BG_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of BFAL_all
  BFAL_all_holder <- numeric(length = nrow(BFAL_all.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(BFAL_all_data$tripID))) {
    if (sum(is.na(BFAL_all.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      BFAL_all.Href.kernel[[i]]@data$ud[is.na(BFAL_all.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- BFAL_all.Href.kernel[[i]]@data$ud
    BFAL_all_holder <- BFAL_all_holder+add
  }
  BFAL_all_holder <- BFAL_all_holder/length(BFAL_all.Href.kernel)
  
  BFAL_all_avg_estUD <- BFAL_all.Href.kernel[[1]]
  BFAL_all_avg_estUD@data$ud <- BFAL_all_holder
  
  if (sum(is.na(BFAL_all_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    BFAL_all_avg_estUD@data$ud[is.na(BFAL_all_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of LAAL_Inc
  LAAL_Inc_holder <- numeric(length = nrow(LAAL_Inc.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(LAAL_Inc_data$tripID))) {
    if (sum(is.na(LAAL_Inc.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      LAAL_Inc.Href.kernel[[i]]@data$ud[is.na(LAAL_Inc.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- LAAL_Inc.Href.kernel[[i]]@data$ud
    LAAL_Inc_holder <- LAAL_Inc_holder+add
  }
  LAAL_Inc_holder <- LAAL_Inc_holder/length(LAAL_Inc.Href.kernel)
  
  LAAL_Inc_avg_estUD <- LAAL_Inc.Href.kernel[[1]]
  LAAL_Inc_avg_estUD@data$ud <- LAAL_Inc_holder
  
  if (sum(is.na(LAAL_Inc_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    LAAL_Inc_avg_estUD@data$ud[is.na(LAAL_Inc_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of LAAL_BG
  LAAL_BG_holder <- numeric(length = nrow(LAAL_BG.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(LAAL_BG_data$tripID))) {
    if (sum(is.na(LAAL_BG.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      LAAL_BG.Href.kernel[[i]]@data$ud[is.na(LAAL_BG.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- LAAL_BG.Href.kernel[[i]]@data$ud
    LAAL_BG_holder <- LAAL_BG_holder+add
  }
  LAAL_BG_holder <- LAAL_BG_holder/length(LAAL_BG.Href.kernel)
  
  LAAL_BG_avg_estUD <- LAAL_BG.Href.kernel[[1]]
  LAAL_BG_avg_estUD@data$ud <- LAAL_BG_holder
  
  if (sum(is.na(LAAL_BG_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    LAAL_BG_avg_estUD@data$ud[is.na(LAAL_BG_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
  # Average KDE of LAAL_all
  LAAL_all_holder <- numeric(length = nrow(LAAL_all.Href.kernel[[1]])) #length = number of rows on the gridcell
  for (i in 1:length(unique(LAAL_all_data$tripID))) {
    if (sum(is.na(LAAL_all.Href.kernel[[i]]@data$ud)) > 0) {
      print("there na values!")
      LAAL_all.Href.kernel[[i]]@data$ud[is.na(LAAL_all.Href.kernel[[i]]@data$ud)] <- 0
    }
    add <- LAAL_all.Href.kernel[[i]]@data$ud
    LAAL_all_holder <- LAAL_all_holder+add
  }
  LAAL_all_holder <- LAAL_all_holder/length(LAAL_all.Href.kernel)
  
  LAAL_all_avg_estUD <- LAAL_all.Href.kernel[[1]]
  LAAL_all_avg_estUD@data$ud <- LAAL_all_holder
  
  if (sum(is.na(LAAL_all_avg_estUD@data$ud)) > 0) {
    print("there na values!")
    LAAL_all_avg_estUD@data$ud[is.na(LAAL_all_avg_estUD@data$ud)] <- 0 # ok if it sums to >1!
  }
  
}


if (loc == "Bird_Island") {
  ################################################################################
  # THESE ARE FOR BIRD_ISLAND
  
  # Plot BBAL KDEs and save polygons ---------------------------------------------
  
  # BBAL Inc
  BBAL_Inc_KDE_99 <- getverticeshr(BBAL_Inc_avg_estUD,percent=99)
  plot(BBAL_Inc_KDE_99)
  BBAL_Inc_KDE_99_vect <- vect(BBAL_Inc_KDE_99)
  crs(BBAL_Inc_KDE_99_vect, proj=TRUE)
  BBAL_Inc_KDE_99_longlat_vect <- terra::project(BBAL_Inc_KDE_99_vect, "epsg:4326")
  writeVector(BBAL_Inc_KDE_99_longlat_vect,paste0(write_dir,"BBAL_Inc_KDE_99.gpkg"),overwrite=TRUE)
  
  # BBAL BG
  BBAL_BG_KDE_99 <- getverticeshr(BBAL_BG_avg_estUD,percent=99)
  plot(BBAL_BG_KDE_99)
  BBAL_BG_KDE_99_vect <- vect(BBAL_BG_KDE_99)
  crs(BBAL_BG_KDE_99_vect, proj=TRUE)
  BBAL_BG_KDE_99_longlat_vect <- terra::project(BBAL_BG_KDE_99_vect, "epsg:4326")
  writeVector(BBAL_BG_KDE_99_longlat_vect,paste0(write_dir,"BBAL_BG_KDE_99.gpkg"),overwrite=TRUE)
  
  # BBAL all
  BBAL_all_KDE_99 <- getverticeshr(BBAL_all_avg_estUD,percent=99)
  plot(BBAL_all_KDE_99)
  BBAL_all_KDE_99_vect <- vect(BBAL_all_KDE_99)
  crs(BBAL_all_KDE_99_vect, proj=TRUE)
  BBAL_all_KDE_99_longlat_vect <- terra::project(BBAL_all_KDE_99_vect, "epsg:4326")
  writeVector(BBAL_all_KDE_99_longlat_vect,paste0(write_dir,"BBAL_all_KDE_99.gpkg"),overwrite=TRUE)
  
  
  # Plot GHAL KDEs and save polygons ---------------------------------------------
  
  # GHAL Inc
  GHAL_Inc_KDE_99 <- getverticeshr(GHAL_Inc_avg_estUD,percent=99)
  plot(GHAL_Inc_KDE_99)
  GHAL_Inc_KDE_99_vect <- vect(GHAL_Inc_KDE_99)
  crs(GHAL_Inc_KDE_99_vect, proj=TRUE)
  GHAL_Inc_KDE_99_longlat_vect <- terra::project(GHAL_Inc_KDE_99_vect, "epsg:4326")
  writeVector(GHAL_Inc_KDE_99_longlat_vect,paste0(write_dir,"GHAL_Inc_KDE_99.gpkg"),overwrite=TRUE)
  
  # GHAL BG
  GHAL_BG_KDE_99 <- getverticeshr(GHAL_BG_avg_estUD,percent=99)
  plot(GHAL_BG_KDE_99)
  GHAL_BG_KDE_99_vect <- vect(GHAL_BG_KDE_99)
  crs(GHAL_BG_KDE_99_vect, proj=TRUE)
  GHAL_BG_KDE_99_longlat_vect <- terra::project(GHAL_BG_KDE_99_vect, "epsg:4326")
  writeVector(GHAL_BG_KDE_99_longlat_vect,paste0(write_dir,"GHAL_BG_KDE_99.gpkg"),overwrite=TRUE)
  
  # GHAL all
  GHAL_all_KDE_99 <- getverticeshr(GHAL_all_avg_estUD,percent=99)
  plot(GHAL_all_KDE_99)
  GHAL_all_KDE_99_vect <- vect(GHAL_all_KDE_99)
  crs(GHAL_all_KDE_99_vect, proj=TRUE)
  GHAL_all_KDE_99_longlat_vect <- terra::project(GHAL_all_KDE_99_vect, "epsg:4326")
  writeVector(GHAL_all_KDE_99_longlat_vect,paste0(write_dir,"GHAL_all_KDE_99.gpkg"),overwrite=TRUE)
  
  # Plot WAAL KDEs and save polygons ---------------------------------------------
  
  # WAAL Inc
  WAAL_Inc_KDE_99 <- getverticeshr(WAAL_Inc_avg_estUD,percent=99)
  plot(WAAL_Inc_KDE_99)
  WAAL_Inc_KDE_99_vect <- vect(WAAL_Inc_KDE_99)
  crs(WAAL_Inc_KDE_99_vect, proj=TRUE)
  WAAL_Inc_KDE_99_longlat_vect <- terra::project(WAAL_Inc_KDE_99_vect, "epsg:4326")
  writeVector(WAAL_Inc_KDE_99_longlat_vect,paste0(write_dir,"WAAL_Inc_KDE_99.gpkg"),overwrite=TRUE)
  
  # WAAL BG
  WAAL_BG_KDE_99 <- getverticeshr(WAAL_BG_avg_estUD,percent=99)
  plot(WAAL_BG_KDE_99)
  WAAL_BG_KDE_99_vect <- vect(WAAL_BG_KDE_99)
  crs(WAAL_BG_KDE_99_vect, proj=TRUE)
  WAAL_BG_KDE_99_longlat_vect <- terra::project(WAAL_BG_KDE_99_vect, "epsg:4326")
  writeVector(WAAL_BG_KDE_99_longlat_vect,paste0(write_dir,"WAAL_BG_KDE_99.gpkg"),overwrite=TRUE)
  
  # WAAL all
  WAAL_all_KDE_99 <- getverticeshr(WAAL_all_avg_estUD,percent=99)
  plot(WAAL_all_KDE_99)
  WAAL_all_KDE_99_vect <- vect(WAAL_all_KDE_99)
  crs(WAAL_all_KDE_99_vect, proj=TRUE)
  WAAL_all_KDE_99_longlat_vect <- terra::project(WAAL_all_KDE_99_vect, "epsg:4326")
  writeVector(WAAL_all_KDE_99_longlat_vect,paste0(write_dir,"WAAL_all_KDE_99.gpkg"),overwrite=TRUE)
  
} else if (loc == "Midway") {
  
  
  # Plot BFAL KDEs and save polygons ---------------------------------------------
  
  # BFAL Inc
  BFAL_Inc_KDE_99 <- getverticeshr(BFAL_Inc_avg_estUD,percent=99)
  plot(BFAL_Inc_KDE_99)
  BFAL_Inc_KDE_99_vect <- vect(BFAL_Inc_KDE_99)
  crs(BFAL_Inc_KDE_99_vect, proj=TRUE)
  BFAL_Inc_KDE_99_longlat_vect <- terra::project(BFAL_Inc_KDE_99_vect, "epsg:4326")
  writeVector(BFAL_Inc_KDE_99_longlat_vect,paste0(write_dir,"BFAL_Inc_KDE_99.gpkg"),overwrite=TRUE)
  
  # BFAL BG
  BFAL_BG_KDE_99 <- getverticeshr(BFAL_BG_avg_estUD,percent=99)
  plot(BFAL_BG_KDE_99)
  BFAL_BG_KDE_99_vect <- vect(BFAL_BG_KDE_99)
  crs(BFAL_BG_KDE_99_vect, proj=TRUE)
  BFAL_BG_KDE_99_longlat_vect <- terra::project(BFAL_BG_KDE_99_vect, "epsg:4326")
  writeVector(BFAL_BG_KDE_99_longlat_vect,paste0(write_dir,"BFAL_BG_KDE_99.gpkg"),overwrite=TRUE)
  
  # BFAL all
  BFAL_all_KDE_99 <- getverticeshr(BFAL_all_avg_estUD,percent=99)
  plot(BFAL_all_KDE_99)
  BFAL_all_KDE_99_vect <- vect(BFAL_all_KDE_99)
  crs(BFAL_all_KDE_99_vect, proj=TRUE)
  BFAL_all_KDE_99_longlat_vect <- terra::project(BFAL_all_KDE_99_vect, "epsg:4326")
  writeVector(BFAL_all_KDE_99_longlat_vect,paste0(write_dir,"BFAL_all_KDE_99.gpkg"),overwrite=TRUE)
  
  # Plot LAAL KDEs and save polygons ---------------------------------------------
  
  # LAAL Inc
  LAAL_Inc_KDE_99 <- getverticeshr(LAAL_Inc_avg_estUD,percent=99)
  plot(LAAL_Inc_KDE_99)
  LAAL_Inc_KDE_99_vect <- vect(LAAL_Inc_KDE_99)
  crs(LAAL_Inc_KDE_99_vect, proj=TRUE)
  # LAAL_Inc_KDE_99_longlat_vect <- terra::project(LAAL_Inc_KDE_99_vect, "epsg:4326")
  LAAL_Inc_KDE_99_longlat_vect <- terra::project(LAAL_Inc_KDE_99_vect, "epsg:4326")
  writeVector(LAAL_Inc_KDE_99_longlat_vect,paste0(write_dir,"LAAL_Inc_KDE_99.gpkg"),overwrite=TRUE)
  
  # LAAL BG
  LAAL_BG_KDE_99 <- getverticeshr(LAAL_BG_avg_estUD,percent=99)
  plot(LAAL_BG_KDE_99)
  LAAL_BG_KDE_99_vect <- vect(LAAL_BG_KDE_99)
  crs(LAAL_BG_KDE_99_vect, proj=TRUE)
  LAAL_BG_KDE_99_longlat_vect <- terra::project(LAAL_BG_KDE_99_vect, "epsg:4326")
  writeVector(LAAL_BG_KDE_99_longlat_vect,paste0(write_dir,"LAAL_BG_KDE_99.gpkg"),overwrite=TRUE)
  
  # LAAL all
  LAAL_all_KDE_99 <- getverticeshr(LAAL_all_avg_estUD,percent=99)
  plot(LAAL_all_KDE_99)
  LAAL_all_KDE_99_vect <- vect(LAAL_all_KDE_99)
  crs(LAAL_all_KDE_99_vect, proj=TRUE)
  LAAL_all_KDE_99_longlat_vect <- terra::project(LAAL_all_KDE_99_vect, "epsg:4326")
  writeVector(LAAL_all_KDE_99_longlat_vect,paste0(write_dir,"LAAL_all_KDE_99.gpkg"),overwrite=TRUE)
  
}

