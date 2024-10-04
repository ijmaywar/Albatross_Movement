################################################################################
#
# Env data: Create plots for global env data
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
library(paletteer)

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
env_dir <- paste0(GD_dir,"Analysis/Maywar/Global_Env/")
worldmap <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()

# Find env data for the entirety of this figure -------------------------------

setwd(env_dir)
grid_global_df <- st_read("Global_avg_env_GS_1.gpkg")
# grid_global_df <- st_read("Global_avg_env_GS_10.gpkg")

# Plot the two study sites on a world map --------------------------------------

target_crs <- st_crs("+proj=longlat +x_0=0 +y_0=0 +lat_0=0 +lon_0=270")
offset <- 180 - 270

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldmap %>% st_difference(polygon)

# Transform
worldmap_rot <- world2 %>% st_transform(crs = target_crs)

ggplot() + 
  geom_sf(worldmap_rot,mapping=aes()) + 
  geom_point(aes(x=90-177.3813,y=28.19989),size=5,color="#26828EFF") +
  geom_point(aes(x=90-38.0658417,y=-54.0101833),size=5,color="#26828EFF") +
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_linedraw() + 
  xlim(-177,180) +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Plot global env vars ---------------------------------------------------------

# Add column for the average of Jan, Feb, Mar, Dec - the months we are mainly studying
# across both sites

grid_global_breeding_szn_df <- grid_global_df %>% mutate(breeding_szn_si10 = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("si10_", c("1","2","12")))),
                                          breeding_szn_mdts = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("mdts_", c("1","2","12")))),
                                          breeding_szn_mdww = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("mdww_", c("1","2","12")))),
                                          breeding_szn_mpts = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("mpww_", c("1","2","12")))),
                                          breeding_szn_mwd = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("mwd_", c("1","2","12")))),
                                          breeding_szn_mwp = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("mwp_", c("1","2","12")))),
                                          breeding_szn_swh = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("swh_", c("1","2","12")))),
                                          breeding_szn_shts = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("shts_", c("1","2","12")))),
                                          breeding_szn_shww = rowMeans(dplyr::select(as.data.frame(grid_global_df),paste0("shww_", c("1","2","12"))))) %>% 
                                          dplyr::select(centroid_lon,centroid_lat,breeding_szn_si10,breeding_szn_shts,geom)

# modify world dataset to remove overlapping portions with world's polygons
grid_global_df_mod <- grid_global_breeding_szn_df %>% st_difference(polygon)

# grid_global_df_mod <- grid_global_df_mod
# Transform
grid_global_df_rot <- grid_global_df_mod %>% st_transform(crs = target_crs)

# Assuming your data frame is named 'df' and the column is named 'longitude'
grid_global_df_rot$centroid_lon <- ifelse(grid_global_df_rot$centroid_lon-270 < -180, 
                                       grid_global_df_rot$centroid_lon + 360, 
                                       grid_global_df_rot$centroid_lon)

# Multiply windspeeds by 3.6 to get km/h
# Wind in Breeding szn (Dec, Jan, Feb)

avgwindspeed_cols <- colorRampPalette(c("white","red4"))

global_wind <- ggplot() +
  geom_tile(grid_global_df_rot,
    mapping=aes(centroid_lon-270,centroid_lat,fill=3.6*breeding_szn_si10)) +
  scale_fill_continuous(type="viridis",option="rocket",direction=-1) +
  geom_sf(worldmap_rot,mapping=aes(),fill="white") + 
  # geom_point(aes(x=90-177.3813,y=28.19989),shape=21,size=5,color='white',fill="#1170AAFF") +
  # geom_point(aes(x=90-38.0658417,y=-54.0101833),shape=21,size=5,color='white',fill="#479125FF") +
  geom_point(aes(x=90-177.3813,y=28.19989),shape=21,size=5,color='white',stroke=2) +
  geom_point(aes(x=90-38.0658417,y=-54.0101833),shape=21,size=5,color='white',stroke=2) +
  coord_sf(expand = FALSE) +
  labs(fill = "Windspeed (km/h)") +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_linedraw() + 
  xlim(-177,180) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "right")


# 1100 x 550



# Swell height in Breeding szn (Dec, Jan, Feb)

global_swell <- ggplot() +
  geom_tile(grid_global_df_rot,
            mapping=aes(centroid_lon-270,centroid_lat,fill=breeding_szn_shts)) +
  scale_fill_continuous(type="viridis",option="mako",direction=-1) +
  geom_sf(worldmap_rot,mapping=aes(),fill="white") + 
  # geom_point(aes(x=90-177.3813,y=28.19989),shape=21,size=5,color='white',fill="#1170AAFF") +
  # geom_point(aes(x=90-38.0658417,y=-54.0101833),shape=21,size=5,color='white',fill="#479125FF") +
  geom_point(aes(x=90-177.3813,y=28.19989),shape=21,size=5,color='white',stroke=2) +
  geom_point(aes(x=90-38.0658417,y=-54.0101833),shape=21,size=5,color='white',stroke=2) +
  coord_sf(expand = FALSE) +
  labs(fill = "Swell hieght (m)") +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_linedraw() + 
  xlim(-177,180) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "right")

# 1100 x 550

wrap_elements(panel = global_wind / global_swell)

# max x max

