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

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
env_dir <- paste0(GD_dir,"Analysis/Maywar/Global_Env/")
worldmap <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()

# Find env data for the entirety of this figure -------------------------------

setwd(env_dir)
grid_polys_df <- st_read("Global_avg_env_GS10.gpkg")

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
  # coord_sf(expand = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# Plot global env vars ---------------------------------------------------------

# Add column for the average of Jan, Feb, Mar, Dec - the months we are mainly studying
# across both sites

grid_polys_df <- grid_polys_df %>% mutate(breeding_szn_si10 = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("si10_", c("1","2","3","12")))),
                                          breeding_szn_mdts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mdts_", c("1","2","3","12")))),
                                          breeding_szn_mdww = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mdww_", c("1","2","3","12")))),
                                          breeding_szn_mpts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mpww_", c("1","2","3","12")))),
                                          breeding_szn_mwd = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mwd_", c("1","2","3","12")))),
                                          breeding_szn_mwp = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mwp_", c("1","2","3","12")))),
                                          breeding_szn_swh = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("swh_", c("1","2","3","12")))),
                                          breeding_szn_shts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("shts_", c("1","2","3","12")))),
                                          breeding_szn_shww = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("shww_", c("1","2","3","12")))))

# modify world dataset to remove overlapping portions with world's polygons
grid_polys_df_mod <- grid_polys_df %>% st_difference(polygon)

# Transform
grid_polys_df_rot <- grid_polys_df_mod %>% st_transform(crs = target_crs)

# Multiply windspeeds by 3.6 to get km/h
# Wind in Breeding szn
ggplot() +
  geom_sf(grid_polys_df_rot,mapping=aes(geometry=geom,fill=3.6*breeding_szn_si10),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,3.6*max(grid_polys_df_rot$breeding_szn_si10)),
                      breaks = c(0,10,20,30,40),
                      labels = c("0","10","20","30","40"),
                      name = "Windspeed (km/h)") +
  geom_sf(worldmap_rot,mapping=aes()) + 
  geom_point(aes(x=90-177.3813,y=28.19989),size=5,color="#26828EFF") +
  geom_point(aes(x=90-38.0658417,y=-54.0101833),size=5,color="#26828EFF") +
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())#, legend.position = "none")

