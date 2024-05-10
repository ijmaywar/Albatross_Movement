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
write_dir <- paste0(GD_dir,"L4/",loc,"/Tag_Data/GPS/")
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))
if (loc == "Bird_Island") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2019_2022/compiled_by_spp/")
} else if (loc == "Midway") {
  GPS_dir <- paste0(GD_dir,"L2/",loc,"/Tag_Data/GPS/compiled_2018_2023/compiled_by_spp/")
}

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
  if (nrow(birdmeta)==0) {
    # Catching birds that have been historically misnamed 
    if (current_bird=="WAAL_20220129_WB25") {
      birdmeta <- fullmeta %>% filter(Deployment_ID == "WAAL_20220129_WB24")
    }
    else if (current_bird == "WAAL_20220310_BP45") {
      birdmeta <- fullmeta %>% filter(Deployment_ID == "WAAL_20220310_BP43")
    }
  }
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
  sp::proj4string(BBAL_Inc.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BBAL_Inc.sp <- spTransform(BBAL_Inc.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_Inc.Href.kernel <- kernelUD(BBAL_Inc.sp, h="href", same4all = TRUE, grid=250)
  
  # Create BBAL BG Href Kernel
  BBAL_BG.sp <- BBAL_BG_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_BG.sp) <- c("lon","lat")
  sp::proj4string(BBAL_BG.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BBAL_BG.sp <- spTransform(BBAL_BG.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_BG.Href.kernel <- kernelUD(BBAL_BG.sp, h="href", same4all = TRUE, grid=250)
  
  # Create BBAL all Href Kernel
  BBAL_all.sp <- BBAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BBAL_all.sp) <- c("lon","lat")
  sp::proj4string(BBAL_all.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BBAL_all.sp <- spTransform(BBAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BBAL_all.Href.kernel <- kernelUD(BBAL_all.sp, h="href", same4all = TRUE, grid=250)
  
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
  
  # Create GHAL all Href Kernel
  GHAL_all.sp <- GHAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(GHAL_all.sp) <- c("lon","lat")
  sp::proj4string(GHAL_all.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  GHAL_all.sp <- spTransform(GHAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  GHAL_all.Href.kernel <- kernelUD(GHAL_all.sp, h="href", same4all = TRUE, grid=250)
  
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
  
  # Create WAAL all Href Kernel
  WAAL_all.sp <- WAAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(WAAL_all.sp) <- c("lon","lat")
  sp::proj4string(WAAL_all.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  WAAL_all.sp <- spTransform(WAAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  WAAL_all.Href.kernel <- kernelUD(WAAL_all.sp, h="href", same4all = TRUE, grid=250)
  
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
  
  # Create BFAL all Href Kernel
  BFAL_all.sp <- BFAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(BFAL_all.sp) <- c("lon","lat")
  sp::proj4string(BFAL_all.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  BFAL_all.sp <- spTransform(BFAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  BFAL_all.Href.kernel <- kernelUD(BFAL_all.sp, h="href", same4all = TRUE, grid=250)
  
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
  
  # Create LAAL all Href Kernel
  LAAL_all.sp <- LAAL_all_data %>% dplyr::select(tripID,lon,lat)
  sp::coordinates(LAAL_all.sp) <- c("lon","lat")
  sp::proj4string(LAAL_all.sp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  LAAL_all.sp <- spTransform(LAAL_all.sp,CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  LAAL_all.Href.kernel <- kernelUD(LAAL_all.sp, h="href", same4all = TRUE, grid=250)
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
BBAL_Inc_poly_4 <- BBAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[4]]

# Combine coordinates into a single df
BBAL_Inc_df_coords <- data.frame(c(rep(1,nrow(BBAL_Inc_poly_1@coords)),
                                   rep(2,nrow(BBAL_Inc_poly_2@coords)),
                                   rep(3,nrow(BBAL_Inc_poly_3@coords)),
                                   rep(4,nrow(BBAL_Inc_poly_4@coords))),
                                 rbind(BBAL_Inc_poly_1@coords,
                                       BBAL_Inc_poly_2@coords,
                                       BBAL_Inc_poly_3@coords,
                                       BBAL_Inc_poly_4@coords))
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

# Plot BBAL all KDEs and extract coordinates of polygons -----------------------

# BBAL all
BBAL_all_KDE_95 <- getverticeshr(BBAL_all_avg_estUD,percent=95)
plot(BBAL_all_KDE_95)
summary(BBAL_all_KDE_95)

# Extract pixel values
BBAL_all_KDE_95_longlat <- spTransform(BBAL_all_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons)

BBAL_all_poly_1 <- BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
BBAL_all_poly_2 <- BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
BBAL_all_poly_3 <- BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[3]]
BBAL_all_poly_4 <- BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[4]]
BBAL_all_poly_5 <- BBAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[5]]

# Combine coordinates into a single df
BBAL_all_df_coords <- data.frame(c(rep(1,nrow(BBAL_all_poly_1@coords)),
                                   rep(2,nrow(BBAL_all_poly_2@coords)),
                                   rep(3,nrow(BBAL_all_poly_3@coords)),
                                   rep(4,nrow(BBAL_all_poly_4@coords)),
                                   rep(5,nrow(BBAL_all_poly_5@coords))),
                                 rbind(BBAL_all_poly_1@coords,
                                       BBAL_all_poly_2@coords,
                                       BBAL_all_poly_3@coords,
                                       BBAL_all_poly_4@coords,
                                       BBAL_all_poly_5@coords))

colnames(BBAL_all_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BBAL_all_df_coords, file=paste0(write_dir,"BBAL_all_KDE_95_polygons.csv"), row.names=FALSE)



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


# Plot GHAL all KDEs and extract coordinates of polygons -----------------------

# GHAL all
GHAL_all_KDE_95 <- getverticeshr(GHAL_all_avg_estUD,percent=95)
plot(GHAL_all_KDE_95)
summary(GHAL_all_KDE_95)

# Extract pixel values
GHAL_all_KDE_95_longlat <- spTransform(GHAL_all_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(GHAL_all_KDE_95_longlat@polygons[[1]]@Polygons)

GHAL_all_poly_1 <- GHAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
GHAL_all_poly_2 <- GHAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
GHAL_all_poly_3 <- GHAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[3]]
GHAL_all_poly_4 <- GHAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[4]]

# Combine coordinates into a single df
GHAL_all_df_coords <- data.frame(c(rep(1,nrow(GHAL_all_poly_1@coords)),
                                   rep(2,nrow(GHAL_all_poly_2@coords)),
                                   rep(3,nrow(GHAL_all_poly_3@coords)),
                                   rep(4,nrow(GHAL_all_poly_4@coords))),
                                 rbind(GHAL_all_poly_1@coords,
                                       GHAL_all_poly_2@coords,
                                       GHAL_all_poly_3@coords,
                                       GHAL_all_poly_4@coords))

colnames(GHAL_all_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(GHAL_all_df_coords, file=paste0(write_dir,"GHAL_all_KDE_95_polygons.csv"), row.names=FALSE)


# Plot WAAL Inc KDEs and extract coordinates of polygons -----------------------

# WAAL Inc
WAAL_Inc_KDE_95 <- getverticeshr(WAAL_Inc_avg_estUD,percent=95)
plot(WAAL_Inc_KDE_95)
summary(WAAL_Inc_KDE_95)

# Extract pixel values
WAAL_Inc_KDE_95_longlat <- spTransform(WAAL_Inc_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(WAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons)
WAAL_Inc_poly_1 <- WAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
WAAL_Inc_poly_2 <- WAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[2]]
WAAL_Inc_poly_3 <- WAAL_Inc_KDE_95_longlat@polygons[[1]]@Polygons[[3]]

# Combine coordinates into a single df
WAAL_Inc_df_coords <- data.frame(c(rep(1,nrow(WAAL_Inc_poly_1@coords)),rep(2,nrow(WAAL_Inc_poly_2@coords)),
                                   rep(3,nrow(WAAL_Inc_poly_3@coords))),
                                 rbind(WAAL_Inc_poly_1@coords,WAAL_Inc_poly_2@coords,
                                       WAAL_Inc_poly_3@coords))
colnames(WAAL_Inc_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(WAAL_Inc_df_coords, file=paste0(write_dir,"WAAL_Inc_KDE_95_polygons.csv"), row.names=FALSE)



# Plot WAAL BG KDEs and extract coordinates of polygons -----------------------

# WAAL BG
WAAL_BG_KDE_95 <- getverticeshr(WAAL_BG_avg_estUD,percent=95)
plot(WAAL_BG_KDE_95)
summary(WAAL_BG_KDE_95)

# Extract pixel values
WAAL_BG_KDE_95_longlat <- spTransform(WAAL_BG_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(WAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons)
WAAL_BG_poly_1 <- WAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
WAAL_BG_poly_2 <- WAAL_BG_KDE_95_longlat@polygons[[1]]@Polygons[[2]]

# Combine coordinates into a single df
WAAL_BG_df_coords <- data.frame(c(rep(1,nrow(WAAL_BG_poly_1@coords)),rep(2,nrow(WAAL_BG_poly_2@coords))),
                                rbind(WAAL_BG_poly_1@coords,WAAL_BG_poly_2@coords))
colnames(WAAL_BG_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(WAAL_BG_df_coords, file=paste0(write_dir,"WAAL_BG_KDE_95_polygons.csv"), row.names=FALSE)


# Plot WAAL all KDEs and extract coordinates of polygons -----------------------

# WAAL all
WAAL_all_KDE_95 <- getverticeshr(WAAL_all_avg_estUD,percent=95)
plot(WAAL_all_KDE_95)
summary(WAAL_all_KDE_95)

# Extract pixel values
WAAL_all_KDE_95_longlat <- spTransform(WAAL_all_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(WAAL_all_KDE_95_longlat@polygons[[1]]@Polygons)

WAAL_all_poly_1 <- WAAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
WAAL_all_poly_2 <- WAAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[2]]

# Combine coordinates into a single df
WAAL_all_df_coords <- data.frame(c(rep(1,nrow(WAAL_all_poly_1@coords)),
                                   rep(2,nrow(WAAL_all_poly_2@coords))),
                                 rbind(WAAL_all_poly_1@coords,
                                       WAAL_all_poly_2@coords))

colnames(WAAL_all_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(WAAL_all_df_coords, file=paste0(write_dir,"WAAL_all_KDE_95_polygons.csv"), row.names=FALSE)

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


# Plot BFAL all KDEs and extract coordinates of polygons -----------------------

# BFAL all
BFAL_all_KDE_95 <- getverticeshr(BFAL_all_avg_estUD,percent=95)
plot(BFAL_all_KDE_95)
summary(BFAL_all_KDE_95)

# Extract pixel values
BFAL_all_KDE_95_longlat <- spTransform(BFAL_all_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(BFAL_all_KDE_95_longlat@polygons[[1]]@Polygons)

BFAL_all_poly_1 <- BFAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[1]]
BFAL_all_poly_2 <- BFAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[2]]

# Combine coordinates into a single df
BFAL_all_df_coords <- data.frame(c(rep(1,nrow(BFAL_all_poly_1@coords)),
                                   rep(2,nrow(BFAL_all_poly_2@coords))),
                                 rbind(BFAL_all_poly_1@coords,
                                       BFAL_all_poly_2@coords))

colnames(BFAL_all_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(BFAL_all_df_coords, file=paste0(write_dir,"BFAL_all_KDE_95_polygons.csv"), row.names=FALSE)



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


# Plot LAAL all KDEs and extract coordinates of polygons -----------------------

# LAAL all
LAAL_all_KDE_95 <- getverticeshr(LAAL_all_avg_estUD,percent=95)
plot(LAAL_all_KDE_95)
summary(LAAL_all_KDE_95)

# Extract pixel values
LAAL_all_KDE_95_longlat <- spTransform(LAAL_all_KDE_95, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
length(LAAL_all_KDE_95_longlat@polygons[[1]]@Polygons)

LAAL_all_poly_1 <- LAAL_all_KDE_95_longlat@polygons[[1]]@Polygons[[1]]

# Combine coordinates into a single df
LAAL_all_df_coords <- data.frame(c(rep(1,nrow(LAAL_all_poly_1@coords))),
                                 rbind(LAAL_all_poly_1@coords))

colnames(LAAL_all_df_coords) <- c("Polygon","lon","lat")

# write df as csv
write.csv(LAAL_all_df_coords, file=paste0(write_dir,"LAAL_all_KDE_95_polygons.csv"), row.names=FALSE)


################################################################################


