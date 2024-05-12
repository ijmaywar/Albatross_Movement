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
if (location == "Bird_Island") {
  nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/compiled_2019_2022/ERA5_Monthly_Avg_10m/")
} else {
  nc_dir <- paste0(GD_dir,"L0/",location,"/Wind_Data/compiled_2018_2023/ERA5_Monthly_Avg_10m/")
}
GPS_dir <- paste0(GD_dir,"L4/",location,"/Tag_Data/GPS/")
# wind_L1_dir <- paste0(GD_dir,"L1/",location,"/Wind_Data/ERA5_SingleLevels_10m/allbirds_GPS_with_wind/",szn,"/")

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

# Get Polygon data  ----------------------------------------------------------------

setwd(GPS_dir)
KDEs <- list.dirs(recursive = FALSE) # GPS files 
KDEs <- str_sub(KDEs,3,)

KDE_name <- "BBAL_all_KDE_95" # Read "BBAL_all_KDE_95_polygons.csv" 
Polygons <- vect(paste0(KDE_name,"/",KDE_name,".shp"))

# Get wind data ----------------------------------------------------------------

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 

if (location == "Bird_Island") {
  
  # Load netcdf file directly for Bird Island
  wind_t1 <- rast("Bird_Island_19_22.nc")
  # Create data vectors
  wind_t1 # Make sure you got the right stuff!
  times_t1 <- time(wind_t1)  # stores times from each file 
  all_times <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
  all_times_num <- as.numeric(all_times)
  
} else if (location == "Midway") {
  
  # Load netcdf file directly for Midway
  wind_t1 <- rast("Midway_18_23.nc")
  wind_t1 # Make sure you got the right stuff!
  
  # Wrap -180 to 180
  wind_crds <- as.data.frame(crds(wind_t1[[1]]))
  for (lyr_name in names(wind_t1)) {
    new_crds <- cbind(wind_crds,as.data.frame(values(wind_t1[[lyr_name]])))
    new_crds_neg180 <- new_crds %>% filter(x==-180)
    new_crds_neg180$x <- 180
    new_crds <- rbind(new_crds,new_crds_neg180)
    new_crds <- arrange(new_crds,desc(y),x)
    current_lyr <- rast(new_crds,type="xyz")
    if (lyr_name == names(wind_t1)[1]) {
      wind_t1_new <- current_lyr
    } else {
      wind_t1_new <- c(wind_t1_new,current_lyr)
    }
  }
  time(wind_t1_new) <- time(wind_t1)
  units(wind_t1_new) <- units(wind_t1)
  
  # Create data vectors
  wind_t1 <- wind_t1_new # Make sure you got the right stuff!
  wind_t1
  
  times_t1 <- time(wind_t1)  # stores times from each file 
  all_times <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
  all_times_num <- as.numeric(all_times)
  
}

# Create data grid -------------------------------------------------------------

# Get world map data
world_map <- map_data("world")

# Set boundaries
lon_max <- -15
lon_min <- -65
lat_max <- -35
lat_min <- -65

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
grid_polys_df$WindSpeed <- NA

# Loop through m and add wind information: speed -------------------------------

# # Assume the same month for the entire grid
# timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
# timej_num <- as.numeric(timej)

for (j in 1:nrow(grid_polys_df)) {
  
  # isolate center of polygon as the coordinates to extract wind data
  centroid <- st_centroid(grid_polys_df$geometry[[j]])
  grid_polys_df$centroid_lon[j] <- centroid[1]
  grid_polys_df$centroid_lat[j] <- centroid[2]
  
  xy_j <- as.data.frame(cbind(centroid[1],centroid[2]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract speed for time j at location x and y
  speed_j <- extract(wind_t1, xy_j, ID=FALSE)
  monthly_avgs <- as.data.frame(t(speed_j))
  colnames(monthly_avgs) <- c("speed")
  monthly_avgs$datetime <- time(wind_t1)
  
  # Sometimes there are NA values for some reason...
  if (sum(is.na(monthly_avgs))>0) {
    disp("There is an NA value")
    break
  }
  
  # Remove unwanted mth/yrs...
  if (location == "Bird_Island") {
    monthly_avgs <- monthly_avgs %>% filter(
      datetime > as.Date("2019-06-01") & datetime < as.Date("2022-06-01"))
  } else if (location == "Midway") {
    monthly_avgs <- monthly_avgs %>% filter(
      (datetime > as.Date("2018-06-01") & datetime < as.Date("2019-06-01")) |
        (datetime > as.Date("2021-06-01") & datetime < as.Date("2023-06-01")))
  }
  
  # # Trim data so that only the months you are interested are kept
  monthly_avgs <- monthly_avgs %>% filter(month(datetime) %in% c(1)) # just Jan
  
  # Add Speed data to m and m_all_mths
  grid_polys_df$WindSpeed[j] <- mean(monthly_avgs$speed)

}


# Add wind and bird speed and directions ---------------------------------------


# Trim world_map
trim_world_map <- world_map %>% filter(long >= lon_min & long <= lon_max &
                                         lat >= lat_min & lat <= lat_max)

# Plot the map
ggplot() +
  geom_sf(grid_polys_df,mapping=aes(geometry=geometry,fill=WindSpeed),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,15),
                      breaks = c(0,5,10,15),
                      labels = c("0","5","10","15"),
                      name = "Wind velocity (m/s)") +
  geom_polygon(data = trim_world_map, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  # geom_point(data = m, aes(x=lon, y=lat, color=state),size=1) + 
  # geom_sf(data = st_as_sf(Polygons)) + 
  theme_minimal() +
  labs(x="longitude",y="latitude")













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

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))

# Plot the SO KDEs -------------------------------------------------------------

GPS_dir <- paste0(GD_dir,"L4/Bird_Island/Tag_Data/GPS/")
setwd(GPS_dir)
KDEs <- list.dirs(recursive = FALSE) # GPS files 
KDEs <- str_sub(KDEs,3,)
BBAL_Polygon <- vect(paste0("BBAL_all_KDE_95/BBAL_all_KDE_95",".shp"))
crs(BBAL_Polygon) <- crs(worldmap)
GHAL_Polygon <- vect(paste0("GHAL_all_KDE_95/GHAL_all_KDE_95",".shp"))
crs(GHAL_Polygon) <- crs(worldmap)
WAAL_Polygon <- vect(paste0("WAAL_all_KDE_95/WAAL_all_KDE_95",".shp"))
crs(WAAL_Polygon) <- crs(worldmap)

min_lon <- as.numeric(-90)
max_lon <- as.numeric(-10)
min_lat <- as.numeric(-70)
max_lat <- as.numeric(-35)

ggplot() +
  geom_sf(data = (worldmap)) + 
  geom_sf(data = st_as_sf(BBAL_Polygon),fill=NA,color="Blue",linewidth=1,alpha=0.5) + 
  geom_sf(data = st_as_sf(GHAL_Polygon),fill=NA,color="Green",linewidth=1,alpha=0.5) + 
  geom_sf(data = st_as_sf(WAAL_Polygon),fill=NA,color="Black",linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(min_lon,max_lon), ylim = c(min_lat,max_lat), expand = FALSE) +
  # geom_point(aes(x=-38.0658417,y=-54.0101833),size=1) + 
  theme_bw() +
  labs(x="Longitude",y="Latitude")



# Find wind data for the entirety of this figure -------------------------------

setwd(nc_dir)
wind_files <- list.files(pattern='*.nc') 

# Load netcdf file directly for Bird Island
wind_t1 <- rast("Bird_Island_19_22.nc")
# Create data vectors
wind_t1 # Make sure you got the right stuff!
times_t1 <- time(wind_t1)  # stores times from each file 
all_times <- unique(times_t1) # find unique values because there should be two of every datetime (for u and v)
all_times_num <- as.numeric(all_times)

# Create a grid with 0.25-degree spacing
grid <- expand.grid(lon = seq(min_lon,max_lon,0.25), lat = seq(min_lat,max_lat,0.25))
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Create a grid of polygons
grid_polys <- st_make_grid(
  st_as_sfc(st_bbox(grid_sf)),
  cellsize = c(0.25,0.25),  # Grid cell size based on latitude and longitude intervals
  what = "polygons"
)

# Convert the grid to a data frame for easier manipulation
grid_polys_df <- st_sf(as.data.frame(grid_polys))
grid_polys_df$WindSpeed <- NA

for (j in 1:nrow(grid_polys_df)) {
  
  # isolate center of polygon as the coordinates to extract wind data
  centroid <- st_centroid(grid_polys_df$geometry[[j]])
  grid_polys_df$centroid_lon[j] <- centroid[1]
  grid_polys_df$centroid_lat[j] <- centroid[2]
  
  xy_j <- as.data.frame(cbind(centroid[1],centroid[2]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) 
  
  # Extract speed for time j at location x and y
  speed_j <- extract(wind_t1, xy_j, ID=FALSE)
  monthly_avgs <- as.data.frame(t(speed_j))
  colnames(monthly_avgs) <- c("speed")
  monthly_avgs$datetime <- time(wind_t1)
  
  # Sometimes there are NA values for some reason...
  if (sum(is.na(monthly_avgs))>0) {
    disp("There is an NA value")
    break
  }
  
  # Remove unwanted mth/yrs...
  if (location == "Bird_Island") {
    monthly_avgs <- monthly_avgs %>% filter(
      datetime > as.Date("2019-06-01") & datetime < as.Date("2022-06-01"))
  } else if (location == "Midway") {
    monthly_avgs <- monthly_avgs %>% filter(
      (datetime > as.Date("2018-06-01") & datetime < as.Date("2019-06-01")) |
        (datetime > as.Date("2021-06-01") & datetime < as.Date("2023-06-01")))
  }
  
  # # Trim data so that only the months you are interested are kept
  for (mth_idx in 1:12) {
    current_mth_avg <- monthly_avgs %>% filter(month(datetime) == (mth_idx))
    grid_polys_df <- cbind(grid_polys_df,mean(current_mth_avg$speed))
  }
  
}


# WRITE THIS FUCKING FILE BECAUSE IT TAKES FOREVER TO CREATE
write_csv(grid_polys_df,paste0(GD_dir,"Analysis/Maywar/Wind_KDEs/SO_grid_polys_df.csv"))

# Wind in October
ggplot() +
  geom_sf(data = (worldmap)) + 
  geom_sf(grid_polys_df,mapping=aes(geometry=geometry,fill=WindSpeed),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,15),
                      breaks = c(0,5,10,15),
                      labels = c("0","5","10","15"),
                      name = "Wind velocity (m/s)") +
  geom_sf(data = st_as_sf(BBAL_Polygon),fill=NA,color="Blue",linewidth=1,alpha=0.5) + 
  geom_sf(data = st_as_sf(GHAL_Polygon),fill=NA,color="Green",linewidth=1,alpha=0.5) + 
  geom_sf(data = st_as_sf(WAAL_Polygon),fill=NA,color="Black",linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(min_lon,max_lon), ylim = c(min_lat,max_lat), expand = FALSE) +
  # geom_point(aes(x=-38.0658417,y=-54.0101833),size=1) + 
  theme_bw() +
  labs(x="Longitude",y="Latitude")


# Plot the NP KDEs -------------------------------------------------------------

GPS_dir <- paste0(GD_dir,"L4/Midway/Tag_Data/GPS/")
setwd(GPS_dir)
KDEs <- list.dirs(recursive = FALSE) # GPS files 
KDEs <- str_sub(KDEs,3,)
BFAL_Polygon <- vect(paste0("BFAL_all_KDE_95/BFAL_all_KDE_95",".shp"))
crs(BFAL_Polygon) <- crs(worldmap)
LAAL_Polygon <- vect(paste0("LAAL_all_KDE_95/LAAL_all_KDE_95",".shp"))
crs(LAAL_Polygon) <- crs(worldmap)

ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=50))) + 
  geom_sf(data = sf::st_shift_longitude(st_as_sf(BFAL_Polygon)),fill=NA,color="Blue",linewidth=1,alpha=0.5) + 
  geom_sf(data = sf::st_shift_longitude(st_as_sf(LAAL_Polygon)),fill=NA,color="Green",linewidth=1,alpha=0.5) + 
  coord_sf(xlim = c(120,240), ylim = c(20,50), expand = FALSE) +
  geom_point(aes(x=-177.3813+360,y=28.19989),size=1) + 
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL"),aes(x=lon,y=lat,group=tripID),linewidth=0.1) +
  theme_bw() +
  labs(x="Longitude",y="Latitude")



Midway_GPS_compiled_complete <- read_csv(paste0(GD_dir,"L2/Midway/Tag_Data/GPS/compiled_2018_2023/compiled_complete/Midway_compiled_complete.csv"))
Midway_GPS_compiled_complete$datetime <- as.POSIXlt(Midway_GPS_compiled_complete$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# Sanity check - plot all GPS tracks for BFAL
ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=50))) + 
  geom_sf(data = sf::st_shift_longitude(st_as_sf(BFAL_Polygon)),fill=NA,color="Blue",linewidth=1,alpha=0.5) + 
  coord_sf(xlim = c(165,220), ylim = c(20,45), expand = FALSE) +
  geom_point(aes(x=-177.3813+360,y=28.19989),size=1) + 
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL" & TripType=="BG"),aes(x=lon,y=lat,group=tripID),linewidth=0.1) +
  theme_bw() +
  labs(x="Longitude",y="Latitude")
