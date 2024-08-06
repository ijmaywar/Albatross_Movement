################################################################################
#
# Env data: Extract monthly avg netcdf data for the entire world.
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)
library(ggplot2)
library(maps)
library(sf)
library(rnaturalearth)
library(readxl)

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
env_dir <- paste0(GD_dir,"Analysis/Maywar/Global_Env/")

# Find env data for the entirety of this figure -------------------------------

setwd(env_dir)
wind_raster_0 <- rast("Global_Wind_18_23_All_Months.nc")
wave_raster <- rast("Global_Wave_19_23_All_Months.nc")

# Create data vectors
wind_raster <- subset(wind_raster_0,13:72) # Get rid of 2018 data
names(wind_raster) <- names(wind_raster_0)[1:60] # Get rid of 2018 data
crs(wind_raster) <- "epsg:4326"
wind_raster
crs(wave_raster) <- "epsg:4326"
wave_raster

# times_t1 <- time(wind_raster)  # stores times from each file 
# all_times <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
# all_times_num <- as.numeric(all_times)

grid_res <- 0.25
# Create a grid
grid <- expand.grid(lon = seq(0,360,grid_res), lat = seq(-90,90,grid_res))
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Create a grid of polygons
grid_polys <- st_make_grid(
  st_as_sfc(st_bbox(grid_sf)),
  cellsize = c(grid_res,grid_res),  # Grid cell size based on latitude and longitude intervals
  what = "polygons"
)

# Convert the grid to a data frame for easier manipulation
grid_polys_df <- st_sf(as.data.frame(grid_polys))

# Create empty columns
grid_polys_df$centroid_lon <- NA
grid_polys_df$centroid_lat <- NA
grid_polys_df[4:123] <- NA

for (j in 32808:nrow(grid_polys_df)) {
  
  # isolate center of polygon as the coordinates to extract wind data
  centroid <- st_centroid(grid_polys_df$geometry[[j]])
  grid_polys_df$centroid_lon[j] <- centroid[1]
  grid_polys_df$centroid_lat[j] <- centroid[2]
  
  xy_j <- as.data.frame(cbind(centroid[1],centroid[2]))
  colnames(xy_j) <- c("lon","lat")
  
  # if (xy_j$lat == 87) {
  #   next
  # } else {
  #   # Extract speed for cell j with centroid (x,y)
  #   wind_j <- terra::extract(wind_raster, xy_j, ID=FALSE)
  #   wave_j <- terra::extract(wave_raster, xy_j, ID=FALSE)
  # }
  
  wind_j <- terra::extract(wind_raster, xy_j, ID=FALSE)
  wave_j <- terra::extract(wave_raster, xy_j, ID=FALSE)
  
  monthly_wind_avgs <- as.data.frame(t(wind_j))
  monthly_wave_avgs <- as.data.frame(t(wave_j))
  matrix_data <- matrix(monthly_wave_avgs$V1, nrow = 60, ncol = 9, byrow = FALSE)
  monthly_wave_avgs <- as.data.frame(matrix_data)
  
  colnames(monthly_wind_avgs) <- varnames(wind_raster)
  rownames(monthly_wind_avgs) <- NULL
  colnames(monthly_wave_avgs) <- varnames(wave_raster)
  
  # Combine wind and waves into a single df
  monthly_avgs <- cbind(monthly_wind_avgs,monthly_wave_avgs)
  monthly_avgs$datetime <- time(wind_raster)
  
  # Find averages for each month
  for (mth_idx in 1:12) {
    current_mth_avg <- monthly_avgs %>% filter(month(datetime) == (mth_idx))
    
    # Fill dfs for each env var
    grid_polys_df[j,mth_idx+3+(0*12)] <- mean(current_mth_avg$si10)
    grid_polys_df[j,mth_idx+3+(1*12)] <- mean(current_mth_avg$mdts)
    grid_polys_df[j,mth_idx+3+(2*12)] <- mean(current_mth_avg$mdww)
    grid_polys_df[j,mth_idx+3+(3*12)] <- mean(current_mth_avg$mpts)
    grid_polys_df[j,mth_idx+3+(4*12)] <- mean(current_mth_avg$mpww)
    grid_polys_df[j,mth_idx+3+(5*12)] <- mean(current_mth_avg$mwd)
    grid_polys_df[j,mth_idx+3+(6*12)] <- mean(current_mth_avg$mwp)
    grid_polys_df[j,mth_idx+3+(7*12)] <- mean(current_mth_avg$swh)
    grid_polys_df[j,mth_idx+3+(8*12)] <- mean(current_mth_avg$shts)
    grid_polys_df[j,mth_idx+3+(9*12)] <- mean(current_mth_avg$shww)
    
  }
  
}

colnames(grid_polys_df) <- c("geometry","centroid_lon","centroid_lat",
                             names(subset(wind_raster,1:12)),
                             names(subset(wave_raster,1:12+(0*60))),
                             names(subset(wave_raster,1:12+(1*60))),
                             names(subset(wave_raster,1:12+(2*60))),
                             names(subset(wave_raster,1:12+(3*60))),
                             names(subset(wave_raster,1:12+(4*60))),
                             names(subset(wave_raster,1:12+(5*60))),
                             names(subset(wave_raster,1:12+(6*60))),
                             names(subset(wave_raster,1:12+(7*60))),
                             names(subset(wave_raster,1:12+(8*60))))

# Save gpkg file
# sf::st_write(grid_polys_df,dsn=paste0(env_dir,"Global_avg_env_GS",grid_res,".gpkg"))
sf::st_write(grid_polys_df,dsn=paste0(env_dir,"Global_avg_env_GS_max.gpkg"))

