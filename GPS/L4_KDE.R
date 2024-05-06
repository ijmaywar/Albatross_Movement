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
# library(maptools)
# library(rgdal) # probably not needed, this is outdated by stars, terra, and sf
# just kidding, it is needed for spTransform. You should migrate to sf and stop
# using sp and functions that rely on sp
# library(GeoLocTools)
# setupGeolocation()
# Not available for R>=4.0, though you can get around that if you need
# library(mapdata)
# library(rgeos) # This package is outdated. R says "consider using 'sf' or 'terra' intsead
# library(raster)
library(adehabitatHR)
library(sp)
library(readxl)
library(sf)
library(raster)
# library(ggplot2)
# library(leaflet)


# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";

write_dir <- paste0("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L4/",loc,"/Tag_Data/GPS/")

fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))

GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2019_2022/compiled_by_spp/")
setwd(GPS_dir)
files <- list.files(pattern='*.csv')

for (i in 1:length(files)) {
  m <- read_csv(files[i])
  m$datetime <- as.POSIXlt(m$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  if (i==1) {
    all_data <- m
  } else {
    all_data <- rbind(all_data,m)
  }
}

alltrips <- unique(all_data$tripID)

# if trips are less than two hours, remove them
for (i in 1:length(alltrips)) {
  if (nrow(all_data %>% dplyr::filter(tripID==alltrips[i])) < 12) {
    all_data <- all_data %>% filter(tripID!=alltrips[i])
  }
}

allbirds <- unique(all_data$id)

# If metadata says that GPS data is incomplete, remove the bird
# Also, add metadata for trip type
all_data$TripType <- NA
for (i in 1:length(allbirds)) {
  current_bird <- allbirds[i]
  birdmeta <- fullmeta %>% filter(Deployment_ID == current_bird)
  
  if (is.na(birdmeta$Pos_complete)) {
    all_data <- all_data %>% filter(id!=allbirds[i])
  } else if (birdmeta$Pos_complete==FALSE) {
    all_data <- all_data %>% filter(id!=allbirds[i])
  } else {
    all_data <- all_data %>% mutate(TripType = if_else(id==allbirds[i],birdmeta$Trip_Type,TripType))
  }
}

# update alltrips and allbirds
alltrips <- unique(all_data$tripID)
allbirds <- unique(all_data$id)

# Reclassify E_pip as BG
all_data <- all_data %>% mutate(TripType = if_else(TripType %in% c("E_pip","2BEP","Ep"),"BG",TripType))

# remove any na values
all_data <- na.omit(all_data)

# Split all_data into spp and Trip Types
if (loc == "Bird_Island") {
  BBAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="BBAL") & (TripType=="Inc"))
  BBAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="BBAL") & (TripType=="BG"))
  GHAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="GHAL") & (TripType=="Inc"))
  GHAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="GHAL") & (TripType=="BG"))
  WAAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="WAAL") & (TripType=="Inc"))
  WAAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="WAAL") & (TripType=="BG"))
} else if (loc == "Midway") {
  BFAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="BFAL") & (TripType=="Inc"))
  BFAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="BFAL") & (TripType=="BG"))
  LAAL_Inc_data <- all_data %>% filter((substr(all_data$id,1,4)=="LAAL") & (TripType=="Inc"))
  LAAL_BG_data <- all_data %>% filter((substr(all_data$id,1,4)=="LAAL") & (TripType=="BG"))
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
  sp::proj4string(BBAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BBAL_Inc.sp <- spTransform(BBAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_Inc.Href.kernel <- kernelUD(BBAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create BBAL BG Href Kernel
  BBAL_BG.sp <- BBAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_BG.sp) <- c("lon","lat")
  sp::proj4string(BBAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BBAL_BG.sp <- spTransform(BBAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_BG.Href.kernel <- kernelUD(BBAL_BG.sp, h="href", same4all = TRUE, grid=250)
  
  # Create GHAL Inc Href Kernel
  GHAL_Inc.sp <- GHAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(GHAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  GHAL_Inc.sp <- spTransform(GHAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_Inc.Href.kernel <- kernelUD(GHAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create GHAL BG Href Kernel
  GHAL_BG.sp <- GHAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_BG.sp) <- c("lon","lat")
  sp::proj4string(GHAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  GHAL_BG.sp <- spTransform(GHAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_BG.Href.kernel <- kernelUD(GHAL_BG.sp, h="href", same4all = TRUE, grid=250)
  
  # Create WAAL Inc Href Kernel
  WAAL_Inc.sp <- WAAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(WAAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  WAAL_Inc.sp <- spTransform(WAAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_Inc.Href.kernel <- kernelUD(WAAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create WAAL BG Href Kernel
  WAAL_BG.sp <- WAAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_BG.sp) <- c("lon","lat")
  sp::proj4string(WAAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  WAAL_BG.sp <- spTransform(WAAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_BG.Href.kernel <- kernelUD(WAAL_BG.sp, h="href", same4all = TRUE, grid=250)
  
} else if (loc == "Midway") {
  
  # Create BFAL Inc Href Kernel
  BFAL_Inc.sp <- BFAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(BFAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BFAL_Inc.sp <- spTransform(BFAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_Inc.Href.kernel <- kernelUD(BFAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create BFAL BG Href Kernel
  BFAL_BG.sp <- BFAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_BG.sp) <- c("lon","lat")
  sp::proj4string(BFAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BFAL_BG.sp <- spTransform(BFAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_BG.Href.kernel <- kernelUD(BFAL_BG.sp, h="href", same4all = TRUE, grid=250)
  
  # Create LAAL Inc Href Kernel
  LAAL_Inc.sp <- LAAL_Inc_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_Inc.sp) <- c("lon","lat")
  sp::proj4string(LAAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  LAAL_Inc.sp <- spTransform(LAAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_Inc.Href.kernel <- kernelUD(LAAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create LAAL BG Href Kernel
  LAAL_BG.sp <- LAAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_BG.sp) <- c("lon","lat")
  sp::proj4string(LAAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  LAAL_BG.sp <- spTransform(LAAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_BG.Href.kernel <- kernelUD(LAAL_BG.sp, h="href", same4all = TRUE, grid=250)
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
  
}


################################################################################
# THESE ARE FOR BIRD_ISLAND

# Plot BBAL Inc KDEs and extract coordinates of polygons -----------------------

# BBAL Inc
BBAL_Inc_KDE_95 <- getverticeshr(BBAL_Inc_avg_estUD,percent=95)
plot(BBAL_Inc_KDE_95)
summary(BBAL_Inc_KDE_95)

# Extract pixel values
BBAL_Inc_KDE_95_longlat <- spTransform(BBAL_Inc_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BBAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons)
BBAL_Inc_poly_1 <- BBAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
BBAL_Inc_poly_2 <- BBAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
BBAL_Inc_poly_3 <- BBAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[3]]

# Combine coordinates into a single df
BBAL_Inc_df_coords <- data.frame(c(rep(1,nrow(BBAL_Inc_poly_1@coords)),rep(2,nrow(BBAL_Inc_poly_2@coords)),
                          rep(3,nrow(BBAL_Inc_poly_3@coords))),
           rbind(BBAL_Inc_poly_1@coords,BBAL_Inc_poly_2@coords,
                 BBAL_Inc_poly_3@coords))
colnames(BBAL_Inc_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BBAL_Inc_df_coords, file=paste0(write_dir,"BBAL_Inc_KDE_95_polygons.csv"), row.names=FALSE)



# Plot BBAL BG KDEs and extract coordinates of polygons -----------------------

# BBAL BG
BBAL_BG_KDE_95 <- getverticeshr(BBAL_BG_avg_estUD,percent=95)
plot(BBAL_BG_KDE_95)
summary(BBAL_BG_KDE_95)

# Extract pixel values
BBAL_BG_KDE_95_longlat <- spTransform(BBAL_BG_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BBAL_BG_KDE_95_longlat@polygons[[1]]@Polygons)
BBAL_BG_poly_1 <- BBAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[1]]

# Combine coordinates into a single df
BBAL_BG_df_coords <- data.frame(c(rep(1,nrow(BBAL_BG_poly_1@coords))),
                                 rbind(BBAL_BG_poly_1@coords))
colnames(BBAL_BG_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BBAL_BG_df_coords, file=paste0(write_dir,"BBAL_BG_KDE_95_polygons.csv"), row.names=FALSE)



# Plot GHAL Inc KDEs and extract coordinates of polygons -----------------------

# GHAL Inc
GHAL_Inc_KDE_95 <- getverticeshr(GHAL_Inc_avg_estUD,percent=95)
plot(GHAL_Inc_KDE_95)
summary(GHAL_Inc_KDE_95)

# Extract pixel values
GHAL_Inc_KDE_95_longlat <- spTransform(GHAL_Inc_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(GHAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons)
GHAL_Inc_poly_1 <- GHAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
GHAL_Inc_poly_2 <- GHAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
GHAL_Inc_poly_3 <- GHAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[3]]

# Combine coordinates into a single df
GHAL_Inc_df_coords <- data.frame(c(rep(1,nrow(GHAL_Inc_poly_1@coords)),rep(2,nrow(GHAL_Inc_poly_2@coords)),
                                   rep(3,nrow(GHAL_Inc_poly_3@coords))),
                                 rbind(GHAL_Inc_poly_1@coords,GHAL_Inc_poly_2@coords,
                                       GHAL_Inc_poly_3@coords))
colnames(GHAL_Inc_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(GHAL_Inc_df_coords, file=paste0(write_dir,"GHAL_Inc_KDE_95_polygons.csv"), row.names=FALSE)



# Plot GHAL BG KDEs and extract coordinates of polygons -----------------------

# GHAL BG
GHAL_BG_KDE_95 <- getverticeshr(GHAL_BG_avg_estUD,percent=95)
plot(GHAL_BG_KDE_95)
summary(GHAL_BG_KDE_95)

# Extract pixel values
GHAL_BG_KDE_95_longlat <- spTransform(GHAL_BG_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(GHAL_BG_KDE_95_longlat@polygons[[1]]@Polygons)
GHAL_BG_poly_1 <- GHAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
GHAL_BG_poly_2 <- GHAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[2]]

# Combine coordinates into a single df
GHAL_BG_df_coords <- data.frame(c(rep(1,nrow(GHAL_BG_poly_1@coords)),rep(2,nrow(GHAL_BG_poly_2@coords))),
                                rbind(GHAL_BG_poly_1@coords,GHAL_BG_poly_2@coords))
colnames(GHAL_BG_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(GHAL_BG_df_coords, file=paste0(write_dir,"GHAL_BG_KDE_95_polygons.csv"), row.names=FALSE)

################################################################################




################################################################################
# THESE ARE FOR MIDWAY

# Plot BFAL Inc KDEs and extract coordinates of polygons -----------------------

# BFAL Inc
BFAL_Inc_KDE_95 <- getverticeshr(BFAL_Inc_avg_estUD,percent=95)
plot(BFAL_Inc_KDE_95)
summary(BFAL_Inc_KDE_95)

# Extract pixel values
BFAL_Inc_KDE_95_longlat <- spTransform(BFAL_Inc_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BFAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons)
BFAL_Inc_poly_1 <- BFAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
BFAL_Inc_poly_2 <- BFAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
BFAL_Inc_poly_3 <- BFAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[3]]
BFAL_Inc_poly_4 <- BFAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[4]]

# Combine coordinates into a single df
BFAL_Inc_df_coords <- data.frame(c(rep(1,nrow(BFAL_Inc_poly_1@coords)),rep(2,nrow(BFAL_Inc_poly_2@coords)),
                                   rep(3,nrow(BFAL_Inc_poly_3@coords)),rep(4,nrow(BFAL_Inc_poly_4@coords))),
                                 rbind(BFAL_Inc_poly_1@coords,BFAL_Inc_poly_2@coords,
                                       BFAL_Inc_poly_3@coords,BFAL_Inc_poly_4@coords))
colnames(BFAL_Inc_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BFAL_Inc_df_coords, file=paste0(write_dir,"BFAL_Inc_KDE_95_polygons.csv"), row.names=FALSE)



# Plot BFAL BG KDEs and extract coordinates of polygons -----------------------

# BFAL BG
BFAL_BG_KDE_95 <- getverticeshr(BFAL_BG_avg_estUD,percent=95)
plot(BFAL_BG_KDE_95)
summary(BFAL_BG_KDE_95)

# Extract pixel values
BFAL_BG_KDE_95_longlat <- spTransform(BFAL_BG_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BFAL_BG_KDE_95_longlat@polygons[[1]]@Polygons)
BFAL_BG_poly_1 <- BFAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
BFAL_BG_poly_2 <- BFAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
BFAL_BG_poly_3 <- BFAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[3]]
BFAL_BG_poly_4 <- BFAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[4]]

# Combine coordinates into a single df
BFAL_BG_df_coords <- data.frame(c(rep(1,nrow(BFAL_BG_poly_1@coords)),rep(2,nrow(BFAL_BG_poly_2@coords)),
                                   rep(3,nrow(BFAL_BG_poly_3@coords)),rep(4,nrow(BFAL_BG_poly_4@coords))),
                                 rbind(BFAL_BG_poly_1@coords,BFAL_BG_poly_2@coords,
                                       BFAL_BG_poly_3@coords,BFAL_BG_poly_4@coords))
colnames(BFAL_BG_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BFAL_BG_df_coords, file=paste0(write_dir,"BFAL_BG_KDE_95_polygons.csv"), row.names=FALSE)



# Plot LAAL Inc KDEs and extract coordinates of polygons -----------------------

# LAAL Inc
LAAL_Inc_KDE_95 <- getverticeshr(LAAL_Inc_avg_estUD,percent=95)
plot(LAAL_Inc_KDE_95)
summary(LAAL_Inc_KDE_95)

# Extract pixel values
LAAL_Inc_KDE_95_longlat <- spTransform(LAAL_Inc_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(LAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons)
LAAL_Inc_poly_1 <- LAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[1]]

# Combine coordinates into a single df
LAAL_Inc_df_coords <- data.frame(c(rep(1,nrow(LAAL_Inc_poly_1@coords))),
                                 rbind(LAAL_Inc_poly_1@coords))
colnames(LAAL_Inc_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(LAAL_Inc_df_coords, file=paste0(write_dir,"LAAL_Inc_KDE_95_polygons.csv"), row.names=FALSE)



# Plot LAAL BG KDEs and extract coordinates of polygons -----------------------

# LAAL BG
LAAL_BG_KDE_95 <- getverticeshr(LAAL_BG_avg_estUD,percent=95)
plot(LAAL_BG_KDE_95)
summary(LAAL_BG_KDE_95)

# Extract pixel values
LAAL_BG_KDE_95_longlat <- spTransform(LAAL_BG_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(LAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons)
LAAL_BG_poly_1 <- LAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
LAAL_BG_poly_2 <- LAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
LAAL_BG_poly_3 <- LAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[3]]

# Combine coordinates into a single df
LAAL_BG_df_coords <- data.frame(c(rep(1,nrow(LAAL_BG_poly_1@coords)),rep(2,nrow(LAAL_BG_poly_2@coords)),
                                  rep(3,nrow(LAAL_BG_poly_3@coords))),
                                rbind(LAAL_BG_poly_1@coords,LAAL_BG_poly_2@coords,
                                      LAAL_BG_poly_3@coords))
colnames(LAAL_BG_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(LAAL_BG_df_coords, file=paste0(write_dir,"LAAL_BG_KDE_95_polygons.csv"), row.names=FALSE)

################################################################################













# PLAYING AROUND WITH H
################################################################################

# save reference bandwiths into a vector
BBAL_BG_RefH <- c()
for (i in 1:length(BBAL_BG.Href.kernel)) {
  BBAL_BG_RefH[i] <- BBAL_BG.Href.kernel[[i]]@h[[1]]
}
hist(BBAL_BG_RefH, breaks=50)
mean(BBAL_BG_RefH)

mcp_obj <- mcp(BBAL_BG.sp,percent=100)

radii_df <- as.data.frame(unique(BBAL_BG_data$tripID))
colnames(radii_df) <- c("tripID")
radii_df["radius"] <- NA

# TO FIND MAX LENGTH. DIVIDE THIS BY 2 TO FIND RADIUS...
for (i in 1:length(mcp_obj)) {
  # Subset a single minimum convex polygon
  current_obj <- mcp_obj[mcp_obj$id==mcp_obj$id[i],]
  # convert sp object into sf object
  current_obj_sf <- as(current_obj,"sf")
  # Find max distance between two points in the mcp
  max_dist <- as.numeric(current_obj_sf %>% st_cast('MULTIPOINT') %>% st_cast('POINT') %>% st_distance %>% max())
  # Divide this distance by 2 to find the "radius" of the mcp
  radius <- max_dist/2
  # Store radius value
  radii_df[i,]$radius <- radius
}

# What's the mean mcp radius? 
mean_radius <- mean(radii_df$radius)
mean_radius

################################################################################
# plot one of the ranges
trip_no <- 2

which_trip <- alltrips[trip_no]
idxs <- which(BBAL.sp$tripID==which_trip)
# plot(mcp(BBAL.sp[idxs,],percent=100))
# plot(BBAL.sp[idxs,],pch=1,cex=0.025,add=TRUE)

BBAL.kernel.ref.1 <- kernelUD(BBAL.sp[idxs,],h="href",grid=100)
href <- BBAL.kernel.ref.1[[which_trip]]@h$h

par(mfrow=c(1,2),mar=c(1,1,1,1))

plot(getverticeshr(BBAL.kernel.ref.1,percent=95),main=paste0("href = ",href))
plot(getverticeshr(BBAL.kernel.ref.1,percent=50),col='red',add=TRUE)
plot(mcp(BBAL.sp[idxs,],percent=100),add=TRUE)
plot(BBAL.sp[idxs,],pch=1,cex=0.025,add=TRUE)

hcustom <- mean_radius
BBAL.kernel.custom.1 <- kernelUD(BBAL.sp[idxs,],h=hcustom,grid=100)

plot(getverticeshr(BBAL.kernel.custom.1,percent=95),main=paste0("custom = ",hcustom))
plot(getverticeshr(BBAL.kernel.custom.1,percent=50),col='red',add=TRUE)
plot(mcp(BBAL.sp[idxs,],percent=100),add=TRUE)
plot(BBAL.sp[idxs,],pch=1,cex=0.025,add=TRUE)

mtext(which_trip,side=3,line=-21,outer=TRUE)





# image(BBAL.kernel.ref.1)

BBAL.spdf <- sp:BBAL.spBBAL.spdf <- sp::SpatialPointsDataFrame(coords=BBAL_data %>% dplyr::select(lon,lat), data=BBAL_data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
BBAL.spdf <- sp::spTransform(BBAL.spdf, CRSobj = CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
BBAL. <- as.data.frame(BBAL.spdf) %>% dplyr::select(tripID,lon,lat)
sp::coordinates() <- c("coords.x1", "coords.x2")
mcp(BBAL.spdf,percent=100)
BBAL_homerange <- mcp(BBAL.spdf,percent=100)$area
BBAL_homerange
plot(mcp(BBAL.spdf,percent=100))
plot(BBAL.spdf,pch=1,cex=0.25,add=TRUE)


sp::coordinates(BBAL.sp) <- c("lon","lat")
sp::proj4string(BBAL.sp) <- sp::CRS(paste0("+proj=utm +zone=",utmzone," +datum=WGS84 +units=m +no_defs"))

# xy.spdf <- sp::SpatialPointsDataFrame(coords = xy, data = all_data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# xy.spdf <- sp::spTransform(xy.spdf, CRSobj = CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
# all_data_UTM <- as.data.frame(xy.spdf)
# all_data_1 <- all_data_UTM %>% dplyr::select(tripID,coords.x1,coords.x2) #selecting out animal id, UTM long, and UTM lat
# 
# sp::coordinates(all_data_1) <- c("coords.x1", "coords.x2") #coordinates function makes an sp object; sf has replaced sp right now, but adeHabitatHR is not compatible with sf
# sp::proj4string(all_data_1) <- sp::CRS(paste0("+proj=utm +zone=",utmzone," +datum=WGS84 +no_defs")) # assigning it the WGS84 projection

#xy <- all_data_1
#spTransform(nest.spdf, CRSobj = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#all_data_1 <- spTransform(all_data_1,lcea) #transform to lamberts only if you specify grid size as in the beginning. manually

# Effect of grid size, however, is negligible...
# http://r-sig-geo.2731867.n2.nabble.com/adehabitatHR-kerneloverlap-and-kernelUD-etc-td7586144.html

# https://trac.faunalia.it/animove/wiki/AnimoveHowto#Whatunitisthegridcellsizein
# some more good notes on parameters: 
# https://lists.faunalia.it/pipermail/animov/2006-May/000137.html


# Generate individual KDE -------------------------------------------------

# https://jamesepaterson.github.io/jamespatersonblog/04_trackingworkshop_kernels
# kernelUD output currently is probability density: absolute density (events per unit area) divided by total number of events
# results <- kernelUD(xy = all_data_1, grid=grid_input,same4all=T,extent=0.1,h=150000)# grid-input guarantees AT LEAST 300km x 300km - in actuality, it is very slightly more than 300km x 300km.

kernel.ref <- kernelUD(xy = tracks.sp, h = "href", same4all=TRUE, grid=100) #use href to figure out initial grid size and extent value
image(kernel.ref)

# save reference bandwiths into a vector
ref_h <- c()
for (i in 1:length(kernel.ref)) {
  ref_h[i] <- kernel.ref[[i]]@h[[1]]
}
hist(ref_h, breaks=50)

# kernel.lscv <- kernelUD(xy = tracks.sp[1:10000,], h = "LSCV", same4all = TRUE, grid=100) #didn't converge
# plotLSCV(kernel.lscv[[3]])
# image(kernel.lscv[[2]])

# we don't want to use the href bandwidth with fine scale data, as it tend to oversmooth (https://www.jstor.org/stable/1938423#metadata_info_tab_contents)
# to manually select the bandwidth, start with an initial high bandwidth and incrementally decrease until you start to see bimodality (https://royalsocietypublishing.org/doi/full/10.1098/rsos.200649)
# results3 <- kernelUD(xy = all_data_1, h = 1600, same4all = TRUE, grid = 100, extent = 0.8) #for YI, had to increase extent to calculate hr 

kernel_150 <- kernelUD(xy = tracks.sp[1:10000,], h = 1.5, same4all = TRUE, grid=100) #for MS
image(kernel_150)

kernel_temp <- kernelUD(xy = all_data_1[1:10000,], h = 250000, same4all = TRUE, grid=100) #for MS
image(kernel_temp) #try to futz with grid parameters here to make visualization better when manually estimating bandwidth

# Very very important: 
# https://meridian.allenpress.com/copeia/article/2006/4/797/260240
# Large H is ok; pick what is biologically relevant
# More discussion of h and grid parameter: 
# https://lists.faunalia.it/pipermail/animov/2006-May/000137.html
# GRID JUST CHANGES HOW THE RASTER LOOKS - THE SEARCH WINDOW/BANDWIDTH/K IS WHAT CONTROLS BIOLOGICAL RELEVANCE

names <- names(kernel.ref)
if (loc=="Bird_Island") {
  BBAL_kde <- kernel.ref[grep("BBAL",names)] #collate of all species from all sites into one object
  GHAL_kde <- kernel.ref[grep("GHAL",names)] #GBBG on MS
} else if (loc=="Midway") {
  BFAL_kde <- kernel.ref[grep("BFAL",names)] 
  LAAL_kde <- kernel.ref[grep("LAAL",names)]
}

#all_gbbg <- append(yig_kde, msg_kde)
#all_herg <- append(yih_kde, msh_kde)

# reminder on raster functions: not used because lines 176-242 are functionally equivalent
#probably densities are already normalized by nature as they range from 0-1



# Just try the href version -----------------------------------------------

kernel.ref.poly <- getverticeshr(kernel.ref, percent=95)
print(kernel.ref.poly)

color <- rep("white", nrow(tracks.sp@data))
  color[(tracks.sp@data$tripID == "BBAL_20191130_O596_1")] <- "red"
  color[(tracks.sp@data$tripID == "BBAL_20191202_O874_1")] <- "green"
  color[(tracks.sp@data$tripID == "BBAL_20191203_B221_1")] <- "blue"
  color[(tracks.sp@data$tripID == "BBAL_20191208_B282_1")] <- "cyan"
  color[(tracks.sp@data$tripID == "BBAL_20191208_B290_1")] <- "grey"
plot(kernel.ref.poly, col = kernel.ref.poly@data$tripID)
  plot(tracks.sp, add = TRUE, col = color, pch = 21)



# Average KDE -------------------------------------------------------------
# average KDE of individuals within a species at a study site#

BBAL_holder <- numeric(length = nrow(kernel.ref[[1]])) #length = number of rows on the gridcell
for (i in 1:length(BBAL_kde)) {
  BBAL_kde[[i]]@data$ud[is.na(BBAL_kde[[i]]@data$ud)] <- 0
  add <- BBAL_kde[[i]]@data$ud
  BBAL_holder <- BBAL_holder+add
}
BBAL_holder <- BBAL_holder/length(BBAL_kde)
BBAL_holder

##### modify existing estUD object with averaged values, then rename
BBAL_averaged_estUD <- BBAL_kde[[1]]
BBAL_averaged_estUD@data$ud <- BBAL_holder
BBAL_averaged_estUD@data$ud[is.na(BBAL_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
image(BBAL_kde[[1]])
image(BBAL_averaged_estUD) 







# average yi herg #
yih_holder <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
for (i in 1:length(yih_kde)) {
  yih_kde[[i]]@data$ud[is.na(yih_kde[[i]]@data$ud)] <- 0
  add <- yih_kde[[i]]@data$ud
  yih_holder <- yih_holder+add
}
yih_holder <- yih_holder/length(yih_kde)
yih_holder

##### modify existing estUD object with averaged values, then rename
yih_averaged_estUD <- yih_kde[[1]]
yih_averaged_estUD@data$ud <- yih_holder
yih_averaged_estUD@data$ud[is.na(yih_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
image(yih_kde[[1]])
image(yih_averaged_estUD) 

#         
# average ms gbbg #
msg_holder <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
for (i in 1:length(msg_kde)) {
  msg_kde[[i]]@data$ud[is.na(msg_kde[[i]]@data$ud)] <- 0
  add <- msg_kde[[i]]@data$ud
  msg_holder <- msg_holder+add
}
msg_holder <- msg_holder/length(msg_kde)
msg_holder

##### modify existing estUD object with averaged values, then rename
msg_averaged_estUD <- msg_kde[[1]]
msg_averaged_estUD@data$ud <- msg_holder
msg_averaged_estUD@data$ud[is.na(msg_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
image(msg_kde[[1]])
image(msg_averaged_estUD) 


# average ms herg #
msh_holder <- numeric(length = nrow(results3[[i]])) #length = number of rows on the gridcell
for (i in 1:length(msh_kde)) {
  msh_kde[[i]]@data$ud[is.na(msh_kde[[i]]@data$ud)] <- 0
  add <- msh_kde[[i]]@data$ud
  msh_holder <- msh_holder+add
}
msh_holder <- msh_holder/length(msh_kde)
msh_holder

##### modify existing estUD object with averaged values, then rename
msh_averaged_estUD <- msh_kde[[1]]
msh_averaged_estUD@data$ud <- msh_holder
msh_averaged_estUD@data$ud[is.na(msh_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
image(msh_kde[[1]])
image(msh_averaged_estUD) 


# average all_GBBG #
# all_gbbg_holder <- numeric(length = nrow(results3[[1]]))
# for (i in 1:length(all_gbbg)) {
#   all_gbbg[[i]]@data$ud[is.na(all_gbbg[[i]]@data$ud)] <- 0
#   add <- all_gbbg[[i]]@data$ud
#   all_gbbg_holder <- all_gbbg_holder+add
# }
# all_gbbg_holder <- all_gbbg_holder/length(all_gbbg)
# all_gbbg_holder
#     
#     ##### modify existing estUD object with averaged values, then rename
#     all_gbbg_averaged_estUD <- all_gbbg[[1]]
#     all_gbbg_averaged_estUD@data$ud <- all_gbbg_holder
#     all_gbbg_averaged_estUD@data$ud[is.na(all_gbbg_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
#     image(all_gbbg[[1]])
#     image(all_gbbg_averaged_estUD)
# 
# # average all_BFAL #
#     all_herg_holder <- numeric(length = nrow(results3[[1]]))
#     for (i in 1:length(all_herg)) {
#       all_herg[[i]]@data$ud[is.na(all_herg[[i]]@data$ud)] <- 0
#       add <- all_herg[[i]]@data$ud
#       all_herg_holder <- all_herg_holder+add
#     }
#     all_herg_holder <- all_herg_holder/length(all_herg)
#     all_herg_holder
#     
#     ##### modify existing estUD object with averaged values, then rename
#     all_herg_averaged_estUD <- all_herg[[1]]
#     all_herg_averaged_estUD@data$ud <- all_herg_holder
#     all_herg_averaged_estUD@data$ud[is.na(all_herg_averaged_estUD@data$ud)] <- 0 # ok if it sums to >1!
#     image(all_herg[[1]])
#     image(all_herg_averaged_estUD)



# Generate class KDE ------------------------------------------------------

# reminder for converting into a spatial pixel dataframe if needed: 
# udspdf <- estUDm2spixdf(lm_kde)

#allHERG <- all_herg_averaged_estUD #renaming KDE objects; one average KDE for one species together into one KDE
#save it at this point into one file
#allGBBG <- all_gbbg_averaged_estUD
yiGBBG <- yig_averaged_estUD
msGBBG <- msg_averaged_estUD
yiHERG <- yih_averaged_estUD
msHERG <- msh_averaged_estUD

# path <- paste0(getwd(),"/pre_defense/final_ud/")
# save(allLAAL, file=paste0(path,"allLAAL.Rdata"))
# save(allBFAL, file=paste0(path,"allBFAL.Rdata"))
# save(midLAAL, file=paste0(path,"midLAAL.Rdata"))
# save(midBFAL, file=paste0(path,"midBFAL.Rdata"))
# save(ternLAAL, file=paste0(path,"ternLAAL.Rdata"))
# save(ternBFAL, file=paste0(path,"ternBFAL.Rdata"))

### load in files ###

# setwd("/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/final_ud/")
# path <- getwd()
# file_list <- list.files(path=path)
# 
# for (i in 1:length(file_list)){
#   load(file_list[i])
# }

### done loading ###

# allHERG_v_allGBBG <- list(allHERG,allGBBG) #generating estUDM objects; we do this because we're interested in comparing UDs and we need this specific class object to do this
# #here, I would do this for herg and gbbg at YI 
# class(allHERG_v_allGBBG)<-"estUDm"
# names(allHERG_v_allGBBG)<-c("allHERG","allGBBG")


yiHERG_v_yiGBBG <- list(yiHERG,yiGBBG) #generating estUDM objects; we do this because we're interested in comparing UDs and we need this specific class object to do this
#here, I would do this for herg and gbbg at YI 
class(yiHERG_v_yiGBBG)<-"estUDm"
names(yiHERG_v_yiGBBG)<-c("yiHERG","yiGBBG")

msHERG_v_msGBBG <- list(msHERG,msGBBG) #generating estUDM objects; we do this because we're interested in comparing UDs and we need this specific class object to do this
#here, I would do this for herg and gbbg at YI 
class(msHERG_v_msGBBG)<-"estUDm"
names(msHERG_v_msGBBG)<-c("msHERG","msGBBG")

# save data classes; now you don't need to re-run all of the above
path <- "C://Users//klato//OneDrive//Documents//ThorneLab//gull_project//"
save(allHERG_v_allGBBG, file=paste0(path,"allHERG_v_allGBBG.Rdata"))
save(yiHERG_v_yiGBBG, file=paste0(path,"yiHERG_v_yiGBBG.Rdata"))
save(msHERG_v_msGBBG, file=paste0(path,"msHERG_v_msGBBG.Rdata"))


# Overlap calculations ----------------------------------------------------

### load in files ###

# setwd("/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/permutation_tests/data_to_load/")
# path <- getwd()
# file_list <- list.files(path=path)
# 
# for (i in 1:length(file_list)){
#   load(file_list[i])
# }

### done loading ###

# calculate UDOI, which is a measure of spatial overlap
ms_UDOI_95 <- kerneloverlaphr(msHERG_v_msGBBG, method="UDOI", percent=95, conditional=T) #missing 652b, two 645s, two 659s 
ms_UDOI_50 <- kerneloverlaphr(msHERG_v_msGBBG, method="UDOI", percent=50, conditional=T)
yi_UDOI_95 <- kerneloverlaphr(yiHERG_v_yiGBBG, method="UDOI", percent=95, conditional=T)
yi_UDOI_50 <- kerneloverlaphr(yiHERG_v_yiGBBG, method="UDOI", percent=50, conditional=T)

#plots to check
plot(getverticeshr(BBAL_averaged_estUD, percent = 50)) #ave this as an object and can plot in ggplot
plot(getverticeshr(BBAL_averaged_estUD, percent = 50), add = TRUE, lty = 2)

#plotting points over the vertices
image(BBAL_averaged_estUD)
plot(getverticeshr(BBAL_averaged_estUD, percent = 95), add = T)
points(tracks.sp[1:10000,], pch = 1, cex = 0.1)

#use plotverticeshr to get the actual area of the contours 
g <- getverticeshr(BBAL_averaged_estUD, percent = 95, unout = "km2")
h <- getverticeshr(msHERG, percent = 95, unout = "km2")
g5 <- getverticeshr(msGBBG, percent = 50, unout = "km2")
h5 <- getverticeshr(msHERG, percent = 50, unout = "km2")
getverticeshr(msGBBG, percent = 95, unout = "km2")

#calculate Bhattacharyya's affinity, which is a measure of similarity
ms_BA_95 <- kerneloverlaphr(msHERG_v_msGBBG, method="BA", percent=95, conditional=T) #missing 652b, two 645s, two 659s 
ms_BA_50 <- kerneloverlaphr(msHERG_v_msGBBG, method="BA", percent=50, conditional=T)
yi_BA_95 <- kerneloverlaphr(yiHERG_v_yiGBBG, method="BA", percent=95, conditional=T)
yi_BA_50 <- kerneloverlaphr(yiHERG_v_yiGBBG, method="BA", percent=50, conditional=T)




# doing some contour plotting with leaflet just to see if our contours make geographical sense
gg95_gbbg <- fortify(getverticeshr(yiGBBG))
gg <- gg95_gbbg[,c(1,2)] #extracting x and y values
names(gg) <- c("x", "y")
xy <- gg
finalTrips.spdf <- SpatialPointsDataFrame(coords = xy, data = gg, proj4string=CRS("+proj=utm +zone=18 +datum=WGS84"))
finalTrips_latlong <- spTransform(finalTrips.spdf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #leaflet only work with lat and longs
gf <- as.data.frame(finalTrips_latlong) #leaflet only works with data frames of lats and longs
leaflet(yi_gbbg_2019) %>% addTiles() %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 1, color = ~ID)
addPolygons(lat = ~y.1, lng = ~x.1)


#Perform a permutation test to determine the significance of overlap, following https://royalsocietypublishing.org/doi/full/10.1098/rsos.200649#d1e565
n_sim <- 1000 #number of simulations for the test
all_UDs <- c(yig_kde, yih_kde)
yi_UDOI_95_perm <- c() #generate empty vectors to put results in to
yi_UDOI_50_perm <- c()
yi_BA_95_perm <- c()
yi_BA_50_perm <- c()
yiHERG_v_yiGBBG_perm <- yiHERG_v_yiGBBG

for(i in 1:n_sim){
  yig_perm <- sample(all_UDs, size = length(yig_kde), replace = FALSE) #reassinging tagID labels to values of yigkde
  yih_perm <- sample(all_UDs, size = length(yih_kde), replace = FALSE)
  yig_holder_perm <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
  for (j in 1:length(yig_perm)) {
    yig_perm[[j]]@data$ud[is.na(yig_perm[[j]]@data$ud)] <- 0
    add <- yig_perm[[j]]@data$ud
    yig_holder_perm <- yig_holder_perm+add
  }
  yig_holder_perm <- yig_holder_perm/length(yig_perm)
  
  ##### modify existing estUD object with averaged values, then rename
  yig_averaged_estUD_perm <- yig_perm[[1]]
  yig_averaged_estUD_perm@data$ud <- yig_holder_perm
  yig_averaged_estUD_perm@data$ud[is.na(yig_averaged_estUD_perm@data$ud)] <- 0 # ok if it sums to >1!
  
  # average yi herg #
  yih_holder_perm <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
  for (j in 1:length(yih_perm)) {
    yih_perm[[j]]@data$ud[is.na(yih_perm[[j]]@data$ud)] <- 0
    add <- yih_perm[[j]]@data$ud
    yih_holder_perm <- yih_holder_perm+add
  }
  yih_holder_perm <- yih_holder_perm/length(yih_perm)
  
  ##### modify existing estUD object with averaged values, then rename
  yih_averaged_estUD_perm <- yih_perm[[1]]
  yih_averaged_estUD_perm@data$ud <- yih_holder_perm
  yih_averaged_estUD_perm@data$ud[is.na(yih_averaged_estUD_perm@data$ud)] <- 0 # ok if it sums to >1!
  
  yiGBBG_perm <- yig_averaged_estUD_perm
  yiHERG_perm <- yih_averaged_estUD_perm
  
  yiHERG_v_yiGBBG_perm[[i]] <- list(yiHERG_perm,yiGBBG_perm) #generating estUDM objects; we do this because we're interested in comparing UDs and we need this specific class object to do this
  #here, I would do this for herg and gbbg at YI 
  class(yiHERG_v_yiGBBG_perm[[i]])<-"estUDm"
  names(yiHERG_v_yiGBBG_perm[[i]])<-c("yiHERG_perm","yiGBBG_perm")
  
}

overlap <- matrix(ncol = 8, nrow = n_sim)
overlap <- as.data.frame(overlap)
names(overlap) <- c("perm_BA_95", "perm_BA_50", "perm_UDOI_95", "perm_UDOI_50", "real_BA_95", "real_BA_50", "real_UDOI_95", "real_UDOI_50")
for(i in 1:n_sim){
  overlap$perm_BA_95[i] <- kerneloverlaphr(yiHERG_v_yiGBBG_perm[[i]], method="BA", percent=95, conditional=T)
  overlap$perm_BA_50[i] <- kerneloverlaphr(yiHERG_v_yiGBBG_perm[[i]], method="BA", percent=50, conditional=T)
  overlap$perm_UDOI_95[i] <- kerneloverlaphr(yiHERG_v_yiGBBG_perm[[i]], method="UDOI", percent=95, conditional=T)
  overlap$perm_UDOI_50[i] <- kerneloverlaphr(yiHERG_v_yiGBBG_perm[[i]], method="UDOI", percent=50, conditional=T)
  
}

overlap$real_BA_95 <- yi_BA_95[1,2]
overlap$real_BA_50 <- yi_BA_50[1,2]
overlap$real_UDOI_50 <- yi_UDOI_50[1,2]
overlap$real_UDOI_95 <- yi_UDOI_95[1,2]
overlap$p_BA_95 <- ifelse(overlap$perm_BA_95[i] < overlap$real_BA_95[i], 1, 0)



n_sim <- 1000 #number of simulations for the test
all_UDs <- c(msg_kde, msh_kde)
ms_UDOI_95_perm <- c() #generate empty vectors to put results in to
ms_UDOI_50_perm <- c()
ms_BA_95_perm <- c()
ms_BA_50_perm <- c()
msHERG_v_msGBBG_perm <- msHERG_v_msGBBG

for(i in 1:n_sim){
  msg_perm <- sample(all_UDs, size = length(msg_kde), replace = FALSE) #reassinging tagID labels to values of yigkde
  msh_perm <- sample(all_UDs, size = length(msh_kde), replace = FALSE)
  msg_holder_perm <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
  for (j in 1:length(msg_perm)) {
    msg_perm[[j]]@data$ud[is.na(msg_perm[[j]]@data$ud)] <- 0
    add <- msg_perm[[j]]@data$ud
    msg_holder_perm <- msg_holder_perm+add
  }
  msg_holder_perm <- msg_holder_perm/length(msg_perm)
  
  ##### modify existing estUD object with averaged values, then rename
  msg_averaged_estUD_perm <- msg_perm[[1]]
  msg_averaged_estUD_perm@data$ud <- msg_holder_perm
  msg_averaged_estUD_perm@data$ud[is.na(msg_averaged_estUD_perm@data$ud)] <- 0 # ok if it sums to >1!
  
  # average yi herg #
  msh_holder_perm <- numeric(length = nrow(results3[[1]])) #length = number of rows on the gridcell
  for (j in 1:length(msh_perm)) {
    msh_perm[[j]]@data$ud[is.na(msh_perm[[j]]@data$ud)] <- 0
    add <- msh_perm[[j]]@data$ud
    msh_holder_perm <- msh_holder_perm+add
  }
  msh_holder_perm <- msh_holder_perm/length(msh_perm)
  
  ##### modify existing estUD object with averaged values, then rename
  msh_averaged_estUD_perm <- msh_perm[[1]]
  msh_averaged_estUD_perm@data$ud <- msh_holder_perm
  msh_averaged_estUD_perm@data$ud[is.na(msh_averaged_estUD_perm@data$ud)] <- 0 # ok if it sums to >1!
  
  msGBBG_perm <- msg_averaged_estUD_perm
  msHERG_perm <- msh_averaged_estUD_perm
  
  msHERG_v_msGBBG_perm[[i]] <- list(msHERG_perm,msGBBG_perm) #generating estUDM objects; we do this because we're interested in comparing UDs and we need this specific class object to do this
  #here, I would do this for herg and gbbg at YI 
  class(msHERG_v_msGBBG_perm[[i]])<-"estUDm"
  names(msHERG_v_msGBBG_perm[[i]])<-c("msHERG_perm","msGBBG_perm")
  
}

overlap <- matrix(ncol = 8, nrow = n_sim)
overlap <- as.data.frame(overlap)
names(overlap) <- c("perm_BA_95", "perm_BA_50", "perm_UDOI_95", "perm_UDOI_50", "real_BA_95", "real_BA_50", "real_UDOI_95", "real_UDOI_50")
for(i in 1:n_sim){
  overlap$perm_BA_95[i] <- kerneloverlaphr(msHERG_v_msGBBG_perm[[i]], method="BA", percent=95, conditional=T)
  overlap$perm_BA_50[i] <- kerneloverlaphr(msHERG_v_msGBBG_perm[[i]], method="BA", percent=50, conditional=T)
  overlap$perm_UDOI_95[i] <- kerneloverlaphr(msHERG_v_msGBBG_perm[[i]], method="UDOI", percent=95, conditional=T)
  overlap$perm_UDOI_50[i] <- kerneloverlaphr(msHERG_v_msGBBG_perm[[i]], method="UDOI", percent=50, conditional=T)
  
}

#permutation method here might not be working since its all coming up as signicant segregration......(i.e. real UDOI and BA 95% are always less than permuted values)
overlap$real_BA_95 <- ms_BA_95[1,2]
overlap$real_BA_50 <- ms_BA_50[1,2]
overlap$real_UDOI_50 <- ms_UDOI_50[1,2]
overlap$real_UDOI_95 <- ms_UDOI_95[1,2]
overlap$p_BA_95 <- ifelse(overlap$perm_BA_95[i] < overlap$real_BA_95[i], 1, 0)
sum(overlap$p_BA_95/1000)
overlap$p_BA_50 <- ifelse(overlap$perm_BA_50[i] < overlap$real_BA_50[i], 1, 0)
sum(overlap$p_BA_50/1000)
overlap$p_UDOI_95 <- ifelse(overlap$perm_UDOI_95[i] < overlap$real_UDOI_95[i], 1, 0)
sum(overlap$p_UDOI_95/1000)
















# for plotting UD raster in overallLAALBFAL_mapping: 
path <- "C:\\Users\\klato\\OneDrive\\Documents\\ThorneLab\\gull_project\\Tag_data\\msgbbg_95.Rdata"
save(g,file=path)
path <- "C:\\Users\\klato\\OneDrive\\Documents\\ThorneLab\\gull_project\\Tag_data\\msgbbg_50.Rdata"
save(g5,file=path)
path <- "C:\\Users\\klato\\OneDrive\\Documents\\ThorneLab\\gull_project\\Tag_data\\msherg_95.Rdata"
save(h,file=path)
path <- "C:\\Users\\klato\\OneDrive\\Documents\\ThorneLab\\gull_project\\Tag_data\\msherg_50.Rdata"
save(h5,file=path)

# path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/permutation_tests/test_stats/"
# setwd(path)
# save(list = ls(all.names = TRUE), file = "all_test_stats.RData")

# Rasters/Contours and GGplot visualization -------------------------------

### allLAAL v allBFAL ###

#95% contours and 100% UD RASTERS
# LAAL 
image(allLAAL)
plot(getverticeshr(allLAAL,percent=95), add=T)
vert95_allLAAL <- getverticeshr(allLAAL,percent=95) #this is what pulls out the specified contours
gg95_allLAAL <- fortify(vert95_allLAAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert95_allLAAL.Rdata"
save(vert95_allLAAL,file=path) #saving out just the contours, so that way you can reload in sf and make it look nice.
#95% contour is the area in which 95% of the data is encompassed



# BFAL 
image(allBFAL)
plot(getverticeshr(allBFAL,percent=95), add=T)
vert95_allBFAL <- getverticeshr(allBFAL,percent=95)
gg95_allBFAL <- fortify(vert95_allBFAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert95_allBFAL.Rdata"
save(vert95_allBFAL,file=path)

# for plotting UD raster in overallLAALBFAL_mapping: 
allBFAL.ud.vol <- getvolumeUD(allBFAL, standardize=TRUE)
plot(allBFAL.ud.vol)
allBFAL.ud.vol.raster <- raster(allBFAL.ud.vol)
plot(allBFAL.ud.vol.raster)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_rasters/allBFAL_ud_vol_rast.Rdata"
save(allBFAL.ud.vol.raster,file=path)


# compare contours at 95UD: 
ggplot()+
  geom_polygon(data=gg95_allLAAL,aes(x=long,y=lat,group=group), color="red",fill=NA)+
  geom_polygon(data=gg95_allBFAL,aes(x=long,y=lat,group=group), color="blue",fill=NA)+
  ggtitle("95UD allLAAL (red), 95UD allBFAL (blue) - \n averaged and not normalized") +
  annotate("label", x = 3800000, y = 6000000, label = "UDOI = 0.432, BA = 0.574", cex=2) +
  coord_equal()


#50
# LAAL 
image(allLAAL)
plot(getverticeshr(allLAAL,percent=50), add=T)
vert50_allLAAL <- getverticeshr(allLAAL,percent=50)
gg50_allLAAL <- fortify(vert50_allLAAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert50_allLAAL.Rdata"
save(vert50_allLAAL,file=path)

# BFAL
image(allBFAL)
plot(getverticeshr(allBFAL,percent=50), add=T)
vert50_allBFAL <- getverticeshr(allBFAL,percent=50)
gg50_allBFAL <- fortify(vert50_allBFAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert50_allBFAL.Rdata"
save(vert50_allBFAL,file=path)

# compare contours at 50UD
ggplot()+
  geom_polygon(data=gg50_allLAAL,aes(x=long,y=lat,group=group), color="red",fill=NA)+
  geom_polygon(data=gg50_allBFAL,aes(x=long,y=lat,group=group), color="blue",fill=NA)+
  ggtitle("50UD allLAAL (red), 50UD allBFAL (blue) - \n averaged and not normalized") +
  annotate("label", x = 3800000, y = 6000000, label = "UDOI = 0.0005, BA = 0.024", cex=2) +
  coord_equal()

#10
# LAAL 
image(allLAAL)
plot(getverticeshr(allLAAL,percent=10), add=T)
vert10_allLAAL <- getverticeshr(allLAAL,percent=10)
gg10_allLAAL <- fortify(vert10_allLAAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert10_allLAAL.Rdata"
save(vert10_allLAAL,file=path)

# BFAL
image(allBFAL)
plot(getverticeshr(allBFAL,percent=10), add=T)
vert10_allBFAL <- getverticeshr(allBFAL,percent=10)
gg10_allBFAL <- fortify(vert10_allBFAL)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/allLAAL_allBFAL/master_script_contours/vert10_allBFAL.Rdata"
save(vert10_allBFAL,file=path)

### midLAAL ###

#95-50-10% contours
image(midLAAL)
vert95_midLAAL <- getverticeshr(midLAAL,percent=95)
vert50_midLAAL <- getverticeshr(midLAAL,percent=50)
vert10_midLAAL <- getverticeshr(midLAAL,percent=10)
plot(getverticeshr(midLAAL,percent=95), add=T)
plot(getverticeshr(midLAAL,percent=50), add=T)
plot(getverticeshr(midLAAL,percent=10), add=T)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/midLAAL/master_script_contours/"
save(vert95_midLAAL,file=paste0(path,"vert95_midLAAL.Rdata"))
save(vert50_midLAAL,file=paste0(path,"vert50_midLAAL.Rdata"))
save(vert10_midLAAL,file=paste0(path,"vert10_midLAAL.Rdata"))

# for plotting UD raster in midwayLAAL_mapping: 
midLAAL.ud.vol <- getvolumeUD(midLAAL, standardize=TRUE)
midLAAL.ud.vol.raster <- raster(midLAAL.ud.vol)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/midLAAL/master_script_rasters/"
save(midLAAL.ud.vol.raster,file=paste0(path,"midLAAL_ud_vol_rast.Rdata"))

### midBFAL ###

#95-50-10% contours
image(midBFAL)
vert95_midBFAL <- getverticeshr(midBFAL,percent=95)
vert50_midBFAL <- getverticeshr(midBFAL,percent=50)
vert10_midBFAL <- getverticeshr(midBFAL,percent=10)
plot(getverticeshr(midBFAL,percent=95), add=T)
plot(getverticeshr(midBFAL,percent=50), add=T)
plot(getverticeshr(midBFAL,percent=10), add=T)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/midBFAL/master_script_contours/"
save(vert95_midBFAL,file=paste0(path,"vert95_midBFAL.Rdata"))
save(vert50_midBFAL,file=paste0(path,"vert50_midBFAL.Rdata"))
save(vert10_midBFAL,file=paste0(path,"vert10_midBFAL.Rdata"))

# for plotting UD raster in midwayLAAL_mapping: 
midBFAL.ud.vol <- getvolumeUD(midBFAL, standardize=TRUE)
midBFAL.ud.vol.raster <- raster(midBFAL.ud.vol)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/midBFAL/master_script_rasters/"
save(midBFAL.ud.vol.raster,file=paste0(path,"midBFAL_ud_vol_rast.Rdata"))

### ternLAAL ###

#95-50-10% contours
image(ternLAAL)
vert95_ternLAAL <- getverticeshr(ternLAAL,percent=95)
vert50_ternLAAL <- getverticeshr(ternLAAL,percent=50)
vert10_ternLAAL <- getverticeshr(ternLAAL,percent=10)
plot(getverticeshr(ternLAAL,percent=95), add=T)
plot(getverticeshr(ternLAAL,percent=50), add=T)
plot(getverticeshr(ternLAAL,percent=10), add=T)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/ternLAAL/master_script_contours/"
save(vert95_ternLAAL,file=paste0(path,"vert95_ternLAAL.Rdata"))
save(vert50_ternLAAL,file=paste0(path,"vert50_ternLAAL.Rdata"))
save(vert10_ternLAAL,file=paste0(path,"vert10_ternLAAL.Rdata"))

# for plotting UD raster in midwayLAAL_mapping: 
ternLAAL.ud.vol <- getvolumeUD(ternLAAL, standardize=TRUE)
ternLAAL.ud.vol.raster <- raster(ternLAAL.ud.vol)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/ternLAAL/master_script_rasters/"
save(ternLAAL.ud.vol.raster,file=paste0(path,"ternLAAL_ud_vol_rast.Rdata"))

### ternBFAL ###

#95-50-10% contours
image(ternBFAL)
vert95_ternBFAL <- getverticeshr(ternBFAL,percent=95)
vert50_ternBFAL <- getverticeshr(ternBFAL,percent=50)
vert10_ternBFAL <- getverticeshr(ternBFAL,percent=10)
plot(getverticeshr(ternBFAL,percent=95), add=T)
plot(getverticeshr(ternBFAL,percent=50), add=T)
plot(getverticeshr(ternBFAL,percent=10), add=T)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/ternBFAL/master_script_contours/"
save(vert95_ternBFAL,file=paste0(path,"vert95_ternBFAL.Rdata"))
save(vert50_ternBFAL,file=paste0(path,"vert50_ternBFAL.Rdata"))
save(vert10_ternBFAL,file=paste0(path,"vert10_ternBFAL.Rdata"))

# for plotting UD raster in midwayLAAL_mapping: 
ternBFAL.ud.vol <- getvolumeUD(ternBFAL, standardize=TRUE)
ternBFAL.ud.vol.raster <- raster(ternBFAL.ud.vol)
path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/figures/individual/ternBFAL/master_script_rasters/"
save(ternBFAL.ud.vol.raster,file=paste0(path,"ternBFAL_ud_vol_rast.Rdata"))

#>>> DONE WITH MAPPING CONTOURS AND RASTERS <<< 

##### Home range in raster mode
# GUIDE: https://ecosystems.psu.edu/research/labs/walter-lab/manual/home-range-estimation/link-to-pdf
# file:///Users/dallasjordan/Downloads/Chapter04_2016.pdf starting bottom of page 80
#the value of a pixel is equal to the percentage of the smallest home range containing this pixel:

volumeLAAL <- getvolumeUD(all_LAAL_averaged_estUD)
image(volumeLAAL)
plot(getverticeshr(allLAAL,percent=95), add=T)

volumeBFAL <- getvolumeUD(all_BFAL_averaged_estUD)
image(volumeBFAL)
plot(getverticeshr(allBFAL,percent=95), add=T)

allLAAL.ud.vol.raster <- raster(volumeLAAL)
plot.new()
breaks <- c(0, 50, 80, 90, 95)
plot(volumeLAAL, col=alpha(viridis(4, direction=-1),0.4), breaks=breaks,interpolate=TRUE,legend.shrink=0.80,legend.args=list(text="UD by
Volume (%)",side=4, font=2, line=2.5, cex=0.8))
plot(volumeBFAL, col=alpha(plasma(4),0.4), breaks=breaks,interpolate=TRUE,
     main="Kernel Density Estimation, Plug-in Bandwidth",xlab="Coords X",
     ylab="Coords Y", legend.shrink=0.80,legend.args=list(text="UD by
Volume (%)",side=4, font=2, line=2.5, cex=0.8))


allLAAL.50vol <- getverticeshr(allLAAL, percent = 50,ida = NULL, unin = "m",
                               unout = "ha", standardize=TRUE)
plot(allLAAL.50vol,add=T)

allBFAL.50vol <- getverticeshr(allBFAL, percent = 50,ida = NULL, unin = "m",
                               unout = "ha", standardize=TRUE)
plot(allBFAL.50vol,add=T)

### midLAAL v ternLAAL 

#95 
image(midLAAL)
plot(getverticeshr(midLAAL,percent=95), add=T)
vert95_midLAAL <- getverticeshr(midLAAL,percent=95)
gg95_midLAAL <- fortify(vert95_midLAAL)


image(ternLAAL)
plot(getverticeshr(ternLAAL,percent=95), add=T)
vert95_ternLAAL <- getverticeshr(ternLAAL,percent=95)
gg95_ternLAAL <- fortify(vert95_ternLAAL)
ggplot()+
  geom_polygon(data=gg95_midLAAL,aes(x=long,y=lat,group=group), color="red",fill=NA)+
  geom_polygon(data=gg95_ternLAAL,aes(x=long,y=lat,group=group), color="blue",fill=NA)+
  ggtitle("95UD midLAAL (red), 95UD ternLAAL (blue) - \n averaged and not normalized") +
  annotate("label", x = 3800000, y = 6000000, label = "UDOI = 0.779, BA = 0.772", cex=2) +
  coord_equal()

#50 

image(midLAAL)
plot(getverticeshr(midLAAL,percent=50), add=T)
vert50_midLAAL <- getverticeshr(midLAAL,percent=50)
gg50_midLAAL <- fortify(vert50_midLAAL)

image(ternLAAL)
plot(getverticeshr(ternLAAL,percent=50), add=T)
vert50_ternLAAL <- getverticeshr(ternLAAL,percent=50)
gg50_ternLAAL <- fortify(vert50_ternLAAL)
ggplot()+
  geom_polygon(data=gg50_midLAAL,aes(x=long,y=lat,group=group), color="red",fill=NA)+
  geom_polygon(data=gg50_ternLAAL,aes(x=long,y=lat,group=group), color="blue",fill=NA)+
  ggtitle("50UD midLAAL (red), 50UD ternLAAL (blue) - \n averaged and not normalized") +
  annotate("label", x = 1200000, y = 5400000, label = "UDOI = 0.114, HR= 0.45") +
  coord_cartesian(xlim = c(-4e+06, 2e+06), ylim = c(4000000, 5500000))

# Randomization tests -----------------------------------------------------

path <- "/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/permutation_tests/test_stats/"
setwd(path)
load("all_test_stats.RData")

setwd("/Users/dallasjordan/Desktop/StonyBrook/SoMAS/Thesis/R/spatial_segregation/final_push/permutation_tests/data_to_load/")
path <- getwd()
file_list <- list.files(path=path)

for (i in 1:length(file_list)){
  load(file_list[i])
}

#### midLAAL v ternLAAL ###

# 95th/50th isopleth and BA
iter <- 1000
significance_tallyUDOI_95 <- 0
significance_tallyUDOI_50 <- 0
significance_tallyBA_95 <- 0
significance_tallyBA_50 <- 0
iter_tally <- 0
resultsUDOI_95_storage <- vector()
resultsUDOI_50_storage <- vector()
resultsBA_95_storage <- vector()
resultsBA_50_storage <- vector()

# Permute the cell values rather than the track labels (permuting spatially instead of by labels)
# take normalized and averaged estUD
# random sample order of grid cells
# recalculate overlap metrics of these randomized UDs
# repeat! simple. 

# New take: 
# see permutation_XX_XX.R scripts for track-ID permutation

# Begin permutation test

for (i in 1:iter){
  ##### Randomize order of normalized/averaged UD grid cells
  # This is doing it over the whole grid - is that right?
  herg_iter <- yiHERG_v_yiGBBG[[1]]
  herg_randomized_ud <- sample(all_UDs, length(yih_kde), replace = FALSE) #sampling randomly the overlap metric of a grid cell. What KAL wants to do is reassign UDs for each species
  
  gbbg_iter <- yiHERG_v_yiGBBG[[2]]
  gbbg_randomized_indices <- sample(all_UDs, length(yig_kde), replace = FALSE)
  gbbg_iter@data$ud <- gbbg_iter@data[gbbg_randomized_indices,]
  
  ##### Create estUDm ##### 
  imherg<-herg_iter
  itgbbg<-gbbg_iter
  imherg_v_itgbbg <- list(imherg,itgbbg)
  class(imherg_v_itgbbg)<-"estUDm"
  names(imherg_v_itgbbg)<-c("imherg","itgbbg")
  image(imherg_v_itgbbg)
  
  iter_mltl_UDOI_95 <- kerneloverlaphr(imherg_v_itgbbg, method="UDOI", percent=95, conditional=T)
  iter_mltl_UDOI_50 <- kerneloverlaphr(imherg_v_itgbbg, method="UDOI", percent=50, conditional=T)
  iter_mltl_BA_95 <- kerneloverlaphr(imherg_v_itgbbg, method="BA", percent=95, conditional=T) 
  iter_mltl_BA_50 <- kerneloverlaphr(imherg_v_itgbbg, method="BA", percent=50, conditional=T)
  
  
  iter_tally <- iter_tally+1
  print(iter_tally)
  
  resultsUDOI_95_storage[i] <- iter_mltl_UDOI_95[1,2]
  print(resultsUDOI_95_storage[i])
  if (iter_mltl_UDOI_95[1,2]<yi_UDOI_95){
    significance_tallyUDOI_95 <- significance_tallyUDOI_95+1
  }
  
  resultsUDOI_50_storage[i] <- iter_mltl_UDOI_50[1,2]
  print(resultsUDOI_50_storage[i])
  if (iter_mltl_UDOI_50[1,2]<yi_UDOI_50){
    print(iter_mltl_UDOI_50)
    print("+1!")
    significance_tallyUDOI_50 <- significance_tallyUDOI_50+1
  }
  
  resultsBA_95_storage[i] <- iter_mltl_BA_95[1,2]
  print(resultsBA_95_storage[i])
  if (iter_mltl_BA_95[1,2]<yi_BA_95){
    print(iter_mltl_BA_95)
    print("+1!")
    significance_tallyBA_95 <- significance_tallyBA_95+1
  }
  
  resultsBA_50_storage[i] <- iter_mltl_BA_50[1,2]
  print(resultsBA_50_storage[i])
  if (iter_mltl_BA_50[1,2]<yi_BA_50){
    print(iter_mltl_BA_50)
    print("+1!")
    significance_tallyBA_50 <- significance_tallyBA_50+1
  }
}


p_valueUDOI_95 <- significance_tallyUDOI_95/iter
mean_valueUDOI_95 <- mean(resultsUDOI_95_storage)
sd_valueUDOI_95 <- sd(resultsUDOI_95_storage)

p_valueUDOI_95 # 1
mean_valueUDOI_95 # 0.023
sd_valueUDOI_95 # 0.011

p_valueUDOI_50 <- significance_tallyUDOI_50/iter #the proportion of instances where the permuted overlap is less than the actual overlap (so, if p = 1, then all permuted overlaps are less than actual overlap showing significant overlap.)
mean_valueUDOI_50 <- mean(resultsUDOI_50_storage)
sd_valueUDOI_50 <- sd(resultsUDOI_50_storage)

p_valueUDOI_50 # 1
mean_valueUDOI_50 # 0.0005
sd_valueUDOI_50 # 0.0008

p_valueBA_95 <- significance_tallyBA_95/iter
mean_valueBA_95 <- mean(resultsBA_95_storage)
sd_valueBA_95 <- sd(resultsBA_95_storage)

p_valueBA_95 # 1
mean_valueBA_95 # 0.122
sd_valueBA_95 # 0.028

p_valueBA_50 <- significance_tallyBA_50/iter
mean_valueBA_50 <- mean(resultsBA_50_storage)
sd_valueBA_50 <- sd(resultsBA_50_storage)

p_valueBA_50 # 1
mean_valueBA_50 # 0.015
sd_valueBA_50 # 0.016

# book keeping - track counts, do not need to run when testing # 
add_lm <-lm
add_lt <-lt
add_lm$island <- "LAALmid"
add_lt$island <- "LAALtern"
combined_lm_lt <- rbind(add_lm,add_lt)
by_island <- split(combined_lm_lt, combined_lm_lt$island)
midLAAL_tracks <- split(by_island[[1]],by_island[[1]]$track)
ternLAAL_tracks <- split(by_island[[2]],by_island[[2]]$track)
all_tracks <- c(midLAAL_tracks,ternLAAL_tracks)
available_mid_tracks <- unique(by_island[[1]]$track)
available_tern_tracks <- unique(by_island[[2]]$track)
n_amt <- length(available_mid_tracks) # you are calculating these to preserve sample size in randomizations
n_att <- length(available_tern_tracks)
n_all <- n_amt+n_att
counts <- combined_lm_lt %>% count(id,track)





#### midBFAL v ternBFAL ###

# 95th/50th isopleth and BA
iter <- 1000
significance_tallyUDOI_95 <- 0
significance_tallyUDOI_50 <- 0
significance_tallyBA_95 <- 0
significance_tallyBA_50 <- 0
iter_tally <- 0
resultsUDOI_95_storage <- vector()
resultsUDOI_50_storage <- vector()
resultsBA_95_storage <- vector()
resultsBA_50_storage <- vector()

for (i in 1:iter){
  ##### Randomize order of normalized/averaged UD grid cells
  # This is doing it over the whole grid - is that right?
  midBFAL_iter <- midBFAL_v_ternBFAL[[1]]
  midBFAL_randomized_indices <- sample(nrow(midBFAL_v_ternBFAL[[1]]@data))
  midBFAL_iter@data$ud <- midBFAL_iter@data[midBFAL_randomized_indices,]
  
  ternBFAL_iter <- midBFAL_v_ternBFAL[[2]]
  ternBFAL_randomized_indices <- sample(nrow(midBFAL_v_ternBFAL[[2]]@data))
  ternBFAL_iter@data$ud <- ternBFAL_iter@data[ternBFAL_randomized_indices,]
  
  ##### Create estUDm ##### 
  imBFAL<-midBFAL_iter
  itBFAL<-ternBFAL_iter
  imBFAL_v_itBFAL <- list(imBFAL,itBFAL)
  class(imBFAL_v_itBFAL)<-"estUDm"
  names(imBFAL_v_itBFAL)<-c("imBFAL","itBFAL")
  
  iter_mbtb_UDOI_95 <- kerneloverlaphr(imBFAL_v_itBFAL, method="UDOI", percent=95, conditional=T)
  iter_mbtb_UDOI_50 <- kerneloverlaphr(imBFAL_v_itBFAL, method="UDOI", percent=50, conditional=T)
  iter_mbtb_BA_95 <- kerneloverlaphr(imBFAL_v_itBFAL, method="BA", percent=95, conditional=T) 
  iter_mbtb_BA_50 <- kerneloverlaphr(imBFAL_v_itBFAL, method="BA", percent=50, conditional=T)
  
  
  iter_tally <- iter_tally+1
  print(iter_tally)
  
  resultsUDOI_95_storage[i] <- iter_mbtb_UDOI_95[1,2]
  print(resultsUDOI_95_storage[i])
  if (iter_mbtb_UDOI_95[1,2]<mbtb_UDOI_95_test_stat){
    print(iter_mbtb_UDOI_95)
    print("+1!")
    significance_tallyUDOI_95 <- significance_tallyUDOI_95+1
  }
  
  resultsUDOI_50_storage[i] <- iter_mbtb_UDOI_50[1,2]
  print(resultsUDOI_50_storage[i])
  if (iter_mbtb_UDOI_50[1,2]<mbtb_UDOI_50_test_stat){
    print(iter_mbtb_UDOI_50)
    print("+1!")
    significance_tallyUDOI_50 <- significance_tallyUDOI_50+1
  }
  
  resultsBA_95_storage[i] <- iter_mbtb_BA_95[1,2]
  print(resultsBA_95_storage[i])
  if (iter_mbtb_BA_95[1,2]<mbtb_95_BA_test_stat){
    print(iter_mbtb_BA_95)
    print("+1!")
    significance_tallyBA_95 <- significance_tallyBA_95+1
  }
  
  resultsBA_50_storage[i] <- iter_mbtb_BA_50[1,2]
  print(resultsBA_50_storage[i])
  if (iter_mbtb_BA_50[1,2]<mbtb_50_BA_test_stat){
    print(iter_mbtb_BA_50)
    print("+1!")
    significance_tallyBA_50 <- significance_tallyBA_50+1
  }
}


p_valueUDOI_95 <- significance_tallyUDOI_95/iter
mean_valueUDOI_95 <- mean(resultsUDOI_95_storage)
sd_valueUDOI_95 <- sd(resultsUDOI_95_storage)

p_valueUDOI_95 # 1
mean_valueUDOI_95 # 0.049
sd_valueUDOI_95 # 0.017

p_valueUDOI_50 <- significance_tallyUDOI_50/iter
mean_valueUDOI_50 <- mean(resultsUDOI_50_storage)
sd_valueUDOI_50 <- sd(resultsUDOI_50_storage)

p_valueUDOI_50 # 0.913
mean_valueUDOI_50 # 0.001
sd_valueUDOI_50 # 0.001

p_valueBA_95 <- significance_tallyBA_95/iter
mean_valueBA_95 <- mean(resultsBA_95_storage)
sd_valueBA_95 <- sd(resultsBA_95_storage)

p_valueBA_95 # 1
mean_valueBA_95 # 0.183
sd_valueBA_95 # 0.026

p_valueBA_50 <- significance_tallyBA_50/iter
mean_valueBA_50 <- mean(resultsBA_50_storage)
sd_valueBA_50 <- sd(resultsBA_50_storage)

p_valueBA_50 # 0.921
mean_valueBA_50 # 0.022
sd_valueBA_50 # 0.016


#### midLAAL v midBFAL ###

# 95th/50th isopleth and BA
iter <- 1000
significance_tallyUDOI_95 <- 0
significance_tallyUDOI_50 <- 0
significance_tallyBA_95 <- 0
significance_tallyBA_50 <- 0
iter_tally <- 0
resultsUDOI_95_storage <- vector()
resultsUDOI_50_storage <- vector()
resultsBA_95_storage <- vector()
resultsBA_50_storage <- vector()

for (i in 1:iter){
  ##### Randomize order of normalized/averaged UD grid cells
  # This is doing it over the whole grid - is that right?
  midLAAL_iter <- midLAAL_v_midBFAL[[1]]
  midLAAL_randomized_indices <- sample(nrow(midLAAL_v_midBFAL[[1]]@data))
  midLAAL_iter@data$ud <- midLAAL_iter@data[midLAAL_randomized_indices,]
  
  midBFAL_iter <- midLAAL_v_midBFAL[[2]]
  midBFAL_randomized_indices <- sample(nrow(midLAAL_v_midBFAL[[2]]@data))
  midBFAL_iter@data$ud <- midBFAL_iter@data[midBFAL_randomized_indices,]
  
  ##### Create estUDm ##### 
  imLAAL<-midLAAL_iter
  imBFAL<-midBFAL_iter
  imLAAL_v_imBFAL <- list(imLAAL,imBFAL)
  class(imLAAL_v_imBFAL)<-"estUDm"
  names(imLAAL_v_imBFAL)<-c("imLAAL","imBFAL")
  
  iter_mlmb_UDOI_95 <- kerneloverlaphr(imLAAL_v_imBFAL, method="UDOI", percent=95, conditional=T)
  iter_mlmb_UDOI_50 <- kerneloverlaphr(imLAAL_v_imBFAL, method="UDOI", percent=50, conditional=T)
  iter_mlmb_BA_95 <- kerneloverlaphr(imLAAL_v_imBFAL, method="BA", percent=95, conditional=T) 
  iter_mlmb_BA_50 <- kerneloverlaphr(imLAAL_v_imBFAL, method="BA", percent=50, conditional=T)
  
  
  iter_tally <- iter_tally+1
  print(iter_tally)
  
  resultsUDOI_95_storage[i] <- iter_mlmb_UDOI_95[1,2]
  print(resultsUDOI_95_storage[i])
  if (iter_mlmb_UDOI_95[1,2]<mlmb_UDOI_95_test_stat){
    print(iter_mlmb_UDOI_95)
    print("+1!")
    significance_tallyUDOI_95 <- significance_tallyUDOI_95+1
  }
  
  resultsUDOI_50_storage[i] <- iter_mlmb_UDOI_50[1,2]
  print(resultsUDOI_50_storage[i])
  if (iter_mlmb_UDOI_50[1,2]<mlmb_UDOI_50_test_stat){
    print(iter_mlmb_UDOI_50)
    print("+1!")
    significance_tallyUDOI_50 <- significance_tallyUDOI_50+1
  }
  
  resultsBA_95_storage[i] <- iter_mlmb_BA_95[1,2]
  print(resultsBA_95_storage[i])
  if (iter_mlmb_BA_95[1,2]<mlmb_95_BA_test_stat){
    print(iter_mlmb_BA_95)
    print("+1!")
    significance_tallyBA_95 <- significance_tallyBA_95+1
  }
  
  resultsBA_50_storage[i] <- iter_mlmb_BA_50[1,2]
  print(resultsBA_50_storage[i])
  if (iter_mlmb_BA_50[1,2]<mlmb_50_BA_test_stat){
    print(iter_mlmb_BA_50)
    print("+1!")
    significance_tallyBA_50 <- significance_tallyBA_50+1
  }
}


p_valueUDOI_95 <- significance_tallyUDOI_95/iter
mean_valueUDOI_95 <- mean(resultsUDOI_95_storage)
sd_valueUDOI_95 <- sd(resultsUDOI_95_storage)

p_valueUDOI_95 # 1
mean_valueUDOI_95 # 0.032
sd_valueUDOI_95 # 0.013

p_valueUDOI_50 <- significance_tallyUDOI_50/iter
mean_valueUDOI_50 <- mean(resultsUDOI_50_storage)
sd_valueUDOI_50 <- sd(resultsUDOI_50_storage)

p_valueUDOI_50 # 0.98
mean_valueUDOI_50 # 0.001
sd_valueUDOI_50 # 0.001

p_valueBA_95 <- significance_tallyBA_95/iter
mean_valueBA_95 <- mean(resultsBA_95_storage)
sd_valueBA_95 <- sd(resultsBA_95_storage)

p_valueBA_95 # 1
mean_valueBA_95 # 0.146
sd_valueBA_95 # 0.028

p_valueBA_50 <- significance_tallyBA_50/iter
mean_valueBA_50 <- mean(resultsBA_50_storage)
sd_valueBA_50 <- sd(resultsBA_50_storage)

p_valueBA_50 # 0.987
mean_valueBA_50 # 0.019
sd_valueBA_50 # 0.016



#### ternLAAL v ternBFAL ###

# 95th/50th isopleth and BA
iter <- 1000
significance_tallyUDOI_95 <- 0
significance_tallyUDOI_50 <- 0
significance_tallyBA_95 <- 0
significance_tallyBA_50 <- 0
iter_tally <- 0
resultsUDOI_95_storage <- vector()
resultsUDOI_50_storage <- vector()
resultsBA_95_storage <- vector()
resultsBA_50_storage <- vector()

for (i in 1:iter){
  ##### Randomize order of normalized/averaged UD grid cells
  # This is doing it over the whole grid - is that right?
  ternLAAL_iter <- ternLAAL_v_ternBFAL[[1]]
  ternLAAL_randomized_indices <- sample(nrow(ternLAAL_v_ternBFAL[[1]]@data))
  ternLAAL_iter@data$ud <- ternLAAL_iter@data[ternLAAL_randomized_indices,]
  
  ternBFAL_iter <- ternLAAL_v_ternBFAL[[2]]
  ternBFAL_randomized_indices <- sample(nrow(ternLAAL_v_ternBFAL[[2]]@data))
  ternBFAL_iter@data$ud <- ternBFAL_iter@data[ternBFAL_randomized_indices,]
  
  ##### Create estUDm ##### 
  itLAAL<-ternLAAL_iter
  itBFAL<-ternBFAL_iter
  itLAAL_v_itBFAL <- list(itLAAL,itBFAL)
  class(itLAAL_v_itBFAL)<-"estUDm"
  names(itLAAL_v_itBFAL)<-c("itLAAL","itBFAL")
  
  iter_tltb_UDOI_95 <- kerneloverlaphr(itLAAL_v_itBFAL, method="UDOI", percent=95, conditional=T)
  iter_tltb_UDOI_50 <- kerneloverlaphr(itLAAL_v_itBFAL, method="UDOI", percent=50, conditional=T)
  iter_tltb_BA_95 <- kerneloverlaphr(itLAAL_v_itBFAL, method="BA", percent=95, conditional=T) 
  iter_tltb_BA_50 <- kerneloverlaphr(itLAAL_v_itBFAL, method="BA", percent=50, conditional=T)
  
  
  iter_tally <- iter_tally+1
  print(iter_tally)
  
  resultsUDOI_95_storage[i] <- iter_tltb_UDOI_95[1,2]
  print(resultsUDOI_95_storage[i])
  if (iter_tltb_UDOI_95[1,2]<tltb_UDOI_95_test_stat){
    print(iter_tltb_UDOI_95)
    print("+1!")
    significance_tallyUDOI_95 <- significance_tallyUDOI_95+1
  }
  
  resultsUDOI_50_storage[i] <- iter_tltb_UDOI_50[1,2]
  print(resultsUDOI_50_storage[i])
  if (iter_tltb_UDOI_50[1,2]<tltb_UDOI_50_test_stat){
    print(iter_tltb_UDOI_50)
    print("+1!")
    significance_tallyUDOI_50 <- significance_tallyUDOI_50+1
  }
  
  resultsBA_95_storage[i] <- iter_tltb_BA_95[1,2]
  print(resultsBA_95_storage[i])
  if (iter_tltb_BA_95[1,2]<tltb_95_BA_test_stat){
    print(iter_tltb_BA_95)
    print("+1!")
    significance_tallyBA_95 <- significance_tallyBA_95+1
  }
  
  resultsBA_50_storage[i] <- iter_tltb_BA_50[1,2]
  print(resultsBA_50_storage[i])
  if (iter_tltb_BA_50[1,2]<tltb_50_BA_test_stat){
    print(iter_tltb_BA_50)
    print("+1!")
    significance_tallyBA_50 <- significance_tallyBA_50+1
  }
}


p_valueUDOI_95 <- significance_tallyUDOI_95/iter
mean_valueUDOI_95 <- mean(resultsUDOI_95_storage)
sd_valueUDOI_95 <- sd(resultsUDOI_95_storage)

p_valueUDOI_95 # 1
mean_valueUDOI_95 # 0.036
sd_valueUDOI_95 # 0.015

p_valueUDOI_50 <- significance_tallyUDOI_50/iter
mean_valueUDOI_50 <- mean(resultsUDOI_50_storage)
sd_valueUDOI_50 <- sd(resultsUDOI_50_storage)

p_valueUDOI_50 # 0
mean_valueUDOI_50 # 0.001
sd_valueUDOI_50 # 0.001

p_valueBA_95 <- significance_tallyBA_95/iter
mean_valueBA_95 <- mean(resultsBA_95_storage)
sd_valueBA_95 <- sd(resultsBA_95_storage)

p_valueBA_95 # 1
mean_valueBA_95 # 0.153
sd_valueBA_95 # 0.027

p_valueBA_50 <- significance_tallyBA_50/iter
mean_valueBA_50 <- mean(resultsBA_50_storage)
sd_valueBA_50 <- sd(resultsBA_50_storage)

p_valueBA_50 # 0
mean_valueBA_50 # 0.018
sd_valueBA_50 # 0.017


#### allLAAL v allBFAL ###

# 95th/50th isopleth and BA
iter <- 1000
significance_tallyUDOI_95 <- 0
significance_tallyUDOI_50 <- 0
significance_tallyBA_95 <- 0
significance_tallyBA_50 <- 0
iter_tally <- 0
resultsUDOI_95_storage <- vector()
resultsUDOI_50_storage <- vector()
resultsBA_95_storage <- vector()
resultsBA_50_storage <- vector()

for (i in 1:iter){
  ##### Randomize order of normalized/averaged UD grid cells
  # This is doing it over the whole grid - is that right?
  allLAAL_iter <- allLAAL_v_allBFAL[[1]]
  allLAAL_randomized_indices <- sample(nrow(allLAAL_v_allBFAL[[1]]@data))
  allLAAL_iter@data$ud <- allLAAL_iter@data[allLAAL_randomized_indices,]
  
  allBFAL_iter <- allLAAL_v_allBFAL[[2]]
  allBFAL_randomized_indices <- sample(nrow(allLAAL_v_allBFAL[[2]]@data))
  allBFAL_iter@data$ud <- allBFAL_iter@data[allBFAL_randomized_indices,]
  
  ##### Create estUDm ##### 
  iaLAAL<-allLAAL_iter
  iaBFAL<-allBFAL_iter
  iaLAAL_v_iaBFAL <- list(iaLAAL,iaBFAL)
  class(iaLAAL_v_iaBFAL)<-"estUDm"
  names(iaLAAL_v_iaBFAL)<-c("iaLAAL","iaBFAL")
  
  iter_alab_UDOI_95 <- kerneloverlaphr(iaLAAL_v_iaBFAL, method="UDOI", percent=95, conditional=T)
  iter_alab_UDOI_50 <- kerneloverlaphr(iaLAAL_v_iaBFAL, method="UDOI", percent=50, conditional=T)
  iter_alab_BA_95 <- kerneloverlaphr(iaLAAL_v_iaBFAL, method="BA", percent=95, conditional=T) 
  iter_alab_BA_50 <- kerneloverlaphr(iaLAAL_v_iaBFAL, method="BA", percent=50, conditional=T)
  
  
  iter_tally <- iter_tally+1
  print(iter_tally)
  
  resultsUDOI_95_storage[i] <- iter_alab_UDOI_95[1,2]
  print(resultsUDOI_95_storage[i])
  if (iter_alab_UDOI_95[1,2]<alab_UDOI_95_test_stat){
    print(iter_alab_UDOI_95)
    print("+1!")
    significance_tallyUDOI_95 <- significance_tallyUDOI_95+1
  }
  
  resultsUDOI_50_storage[i] <- iter_alab_UDOI_50[1,2]
  print(resultsUDOI_50_storage[i])
  if (iter_alab_UDOI_50[1,2]<alab_UDOI_50_test_stat){
    print(iter_alab_UDOI_50)
    print("+1!")
    significance_tallyUDOI_50 <- significance_tallyUDOI_50+1
  }
  
  resultsBA_95_storage[i] <- iter_alab_BA_95[1,2]
  print(resultsBA_95_storage[i])
  if (iter_alab_BA_95[1,2]<alab_95_BA_test_stat){
    print(iter_alab_BA_95)
    print("+1!")
    significance_tallyBA_95 <- significance_tallyBA_95+1
  }
  
  resultsBA_50_storage[i] <- iter_alab_BA_50[1,2]
  print(resultsBA_50_storage[i])
  if (iter_alab_BA_50[1,2]<alab_50_BA_test_stat){
    print(iter_alab_BA_50)
    print("+1!")
    significance_tallyBA_50 <- significance_tallyBA_50+1
  }
}


p_valueUDOI_95 <- significance_tallyUDOI_95/iter
mean_valueUDOI_95 <- mean(resultsUDOI_95_storage)
sd_valueUDOI_95 <- sd(resultsUDOI_95_storage)

p_valueUDOI_95 # 1
mean_valueUDOI_95 # 0.046
sd_valueUDOI_95 # 0.015

p_valueUDOI_50 <- significance_tallyUDOI_50/iter
mean_valueUDOI_50 <- mean(resultsUDOI_50_storage)
sd_valueUDOI_50 <- sd(resultsUDOI_50_storage)

p_valueUDOI_50 # 0.561
mean_valueUDOI_50 # 0.001
sd_valueUDOI_50 # 0.001

p_valueBA_95 <- significance_tallyBA_95/iter
mean_valueBA_95 <- mean(resultsBA_95_storage)
sd_valueBA_95 <- sd(resultsBA_95_storage)

p_valueBA_95 # 1
mean_valueBA_95 # 0.181
sd_valueBA_95 # 0.026

p_valueBA_50 <- significance_tallyBA_50/iter
mean_valueBA_50 <- mean(resultsBA_50_storage)
sd_valueBA_50 <- sd(resultsBA_50_storage)

p_valueBA_50 # 0.568
mean_valueBA_50 # 0.024
sd_valueBA_50 # 0.016





