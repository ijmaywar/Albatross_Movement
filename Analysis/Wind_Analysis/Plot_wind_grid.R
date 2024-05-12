# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"

# Packages  ---------------------------------------------------------

require(tidyverse)
library(lubridate)
library(terra)
library(geosphere)
library(foehnix)
library(ggplot2)
library(maps)
library(sf)

# Set Environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/",szn,"/ERA5_SingleLevels_10m/")
GPS_dir <- paste0(GD_dir,"L2/",location,"/Tag_Data/GPS/",szn,"/")
wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")

# User Functions ----------------------------------------------------------

wrap360 <- function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Both bearings must be [0,360)
bearingAngle <- function(bird_bearing,wind_bearing) {
  LHturn <- wrap360(bird_bearing - wind_bearing)
  RHturn <- wrap360(wind_bearing - bird_bearing)
  return(pmin(LHturn,RHturn))
}

# Get GPS data  ----------------------------------------------------------------

setwd(GPS_dir)
files <- list.files(pattern='*.csv') # GPS files 
m <- read.csv(files[3]) # Read "BBAL_20191202_O874_GPS_L2_600s.csv"

# Get wind data ----------------------------------------------------------------

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 

# Load netcdf file directly for Bird Island
wind_t1 <- rast(wind_files[1])
# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
times_t1 <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
wind_t1_u <- subset(wind_t1, 1:(nlyr(wind_t1)/2)) 
wind_t1_v <- subset(wind_t1, (nlyr(wind_t1)/2)+1:nlyr(wind_t1))

# Load netcdf file directly for Bird Island
wind_t2 <- rast(wind_files[2])
# Create data vectors
wind_t2 # Make sure you got the right stuff!
times_t2 <- time(wind_t2)  # stores times from each file 
times_t2 <- unique(times_t2) # find unique values because there should be two of every datetime (for u and v)
wind_t2_u <- subset(wind_t2, 1:(nlyr(wind_t2)/2)) 
wind_t2_v <- subset(wind_t2, (nlyr(wind_t2)/2)+1:nlyr(wind_t2))

# Stack rasters
# Single levels
u_stack <- c(wind_t1_u, wind_t2_u)
v_stack <- c(wind_t1_v, wind_t2_v)
all_times <- c(times_t1, times_t2)
all_times_num <- as.numeric(all_times)

# Create data grid -------------------------------------------------------------

# Get world map data
world_map <- map_data("world")

# Set boundaries
lon_max <- -35
lon_min <- -61
lat_max <- -50
lat_min <- -61

colony_coords <- c(-38.0658417,-54.0101833)  

# Create a grid with 0.25-degree spacing
lon <- seq(lon_min,lon_max,0.25) # Longitude (columns)
lat <- seq(lat_min,lat_max,0.25) # Latitude (rows)
grid <- expand.grid(lon = lon, lat = lat)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Create a grid of polygons
grid_polys <- st_make_grid(
  st_as_sfc(st_bbox(grid_sf)),
  cellsize = c(0.25,0.25),  # Grid cell size based on latitude and longitude intervals
  what = "polygons"
)

# Convert the grid to a data frame for easier manipulation
grid_polys_df <- st_sf(as.data.frame(grid_polys))

# Loop through m and add wind information: u, v --------------------------------

# Assume the same hour for the entire grid
timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
timej_num <- as.numeric(timej)

# Find index of current_time in all times. Use that index to pull out relevant raster layer.
timej_diff <- abs(all_times_num-timej_num) # taking difference between all gps points
raster_dt_index <- which.min(timej_diff)  # Find the min: tells which layer to pull out and isolate. 
# DateTimes in the exact middle will be assigned to the first index
# 1800 seconds is 30 minutes
min_time_diff <- 1800

# Isolate u and v rasters at time j
ustack_timej <- subset(u_stack, raster_dt_index) 
vstack_timej <- subset(v_stack, raster_dt_index)

if (min(timej_diff) > min_time_diff) {
  print("Current datetime is outside of the data provided by NETCDF files.")
  break
}

# create u and v wind vectors
for (j in 1:nrow(grid_polys_df)) {

  # isolate center of polygon as the coordinates to extract wind data
  centroid <- st_centroid(grid_polys_df$geometry[[j]])
  grid_polys_df$centroid_lon[j] <- centroid[1]
  grid_polys_df$centroid_lat[j] <- centroid[2]
  
  xy_j <- as.data.frame(cbind(centroid[1],centroid[2]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract u and v components for time j at location x and y
  u_j <- extract(ustack_timej, xy_j, ID=FALSE)
  v_j <- extract(vstack_timej, xy_j, ID=FALSE)
  
  if (length(u_j) != 1) {
    print("Number of wind measurements chosen != 1.")
    break
  } else if (is.na(u_j[[1]]) || is.na(v_j[[1]])) {
    print("Wind data for this coordinate cannot be found.")
    break
  }
  
  grid_polys_df$u[j]<- u_j[[1]]
  grid_polys_df$v[j]<- v_j[[1]]
  
}


# Add wind and bird speed and directions ---------------------------------------

# Wind velocity and heading
ddff <- uv2ddff(grid_polys_df)
grid_polys_df$wind_vel <- ddff$ff # m/s: the velocity of the wind
grid_polys_df$wind_dir <- ddff$dd # [0,360) degrees: the direction the wind is coming from


# Trim world_map
trim_world_map <- world_map %>% filter(long >= lon_min & long <= lon_max &
                                         lat >= lat_min & lat <= lat_max)

# Plot the map
ggplot() +
  geom_sf(grid_polys_df,mapping=aes(geometry=geometry,fill=wind_vel),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,15),
                      breaks = c(0,5,10,15),
                      labels = c("0","5","10","15"),
                      name = "Wind velocity (m/s)") +
  geom_polygon(data = trim_world_map, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  # geom_point(data = m, aes(x=lon, y=lat, color=state),size=1) + 
  geom_point(data = m, aes(x=lon, y=lat),size=1) + 
  theme_minimal() +
  labs(x="longitude",y="latitude")

