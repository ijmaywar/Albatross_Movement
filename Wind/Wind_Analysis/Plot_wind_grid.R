library(tidyverse)
library(ggplot2)
library(maps)

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

# Trim world_map
trim_world_map <- world_map %>% filter(long >= lon_min & long <= lon_max &
                                         lat >= lat_min & lat <= lat_max)

# Plot the map
ggplot() +
  geom_polygon(data = trim_world_map, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_point(data = grid, aes(x = lon, y = lat), color = "black", size = 0.0005, alpha=1) +
  geom_point(data = trip_df, aes(x=lon, y=lat, color=state),size=1) + 
  # geom_point(data = trip_df, aes(x=lon, y=lat),size=1) + 
  theme_minimal() +
  labs(x="longitude",y="latitude")
