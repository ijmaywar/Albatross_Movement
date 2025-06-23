################################################################################
#
# Create supplemental figures showing environmental data with KDEs
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

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
library(patchwork)
library(viridis)
library(scales)
library(patchwork)
library(cowplot)

# User functions ---------------------------------------------------------------

Lon360to180 <- function(longitudes) {
  return(ifelse(longitudes > 180, longitudes - 360, longitudes))
}

# Set environment --------------------------------------------------------------

fullmeta <- read_excel('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/Full_Metadata.xlsx')

worldmap <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()

Bird_Island_df <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/ERA5_MonthlyAvg_10m/Bird_Island_KDEs_avg_env.csv')
Midway_df <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/ERA5_MonthlyAvg_10m/Midway_KDEs_avg_env.csv')
compiled_df <- rbind(Bird_Island_df,Midway_df)

grid_global_df <- st_read('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/Global_Env/Global_avg_env_GS_1.gpkg')

GPS_dir <- '/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/all_KDE_95/Bird_Island'
setwd(GPS_dir)
KDEs <- list.files(recursive = FALSE) # GPS files 
BBAL_Polygon <- vect("BBAL_all_KDE_95.gpkg")
crs(BBAL_Polygon) <- crs(worldmap)
GHAL_Polygon <- vect("GHAL_all_KDE_95.gpkg")
crs(GHAL_Polygon) <- crs(worldmap)
WAAL_Polygon <- vect("WAAL_all_KDE_95.gpkg")
crs(WAAL_Polygon) <- crs(worldmap)

Bird_Island_GPS_compiled_complete <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/GPS_compiled_600s_complete/Bird_Island_Compiled_600s_compiled_complete.csv')
Bird_Island_GPS_compiled_complete$datetime <- as.POSIXlt(Bird_Island_GPS_compiled_complete$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

GPS_dir <- '/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/all_KDE_95/Midway'
setwd(GPS_dir)
KDEs <- list.files(recursive = FALSE) # GPS files 
BFAL_Polygon <- vect("BFAL_all_KDE_95.gpkg")
crs(BFAL_Polygon) <- crs(worldmap)
LAAL_Polygon <- vect("LAAL_all_KDE_95.gpkg")
crs(LAAL_Polygon) <- crs(worldmap)

Midway_GPS_compiled_complete <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/GPS_compiled_600s_complete/Midway_Compiled_600s_compiled_complete.csv')
Midway_GPS_compiled_complete$datetime <- as.POSIXlt(Midway_GPS_compiled_complete$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")


# Pre-processing ---------------------------------------------------------------

# Turn columns into factors
compiled_df$Species <- factor(compiled_df$Species, levels=c("BBAL","GHAL","WAAL","BFAL","LAAL"))
compiled_df$KDE_type <- factor(compiled_df$KDE_type, levels=c("all","Inc","BG"))
compiled_df$on_colony <- factor(compiled_df$on_colony, levels=c(1,0))
compiled_df$Month <- factor(compiled_df$Month, levels=1:12)

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

swell_m_min <- min(grid_global_df_rot$breeding_szn_shts,na.rm=TRUE)
swell_m_max <- max(grid_global_df_rot$breeding_szn_shts,na.rm=TRUE)
wind_mps_min <- min(grid_global_df_rot$breeding_szn_si10,na.rm=TRUE)
wind_mps_max <- max(grid_global_df_rot$breeding_szn_si10,na.rm=TRUE)


################################################################################
# Southern Ocean --------------------------------------------------------------

SO_lat_min <- -80
SO_lat_max <- -20
SO_lon_min <- -150
SO_lon_max <- 30

# Load & preprocess wind/swell data
env_grid_SO <- grid_global_breeding_szn_df %>%
  mutate(
    lon_180 = ifelse(centroid_lon > 180, centroid_lon - 360, centroid_lon),
    wind_kph = 3.6 * breeding_szn_si10,
    swell_m = breeding_szn_shts
  ) %>%
  filter(
    between(centroid_lat, SO_lat_min, SO_lat_max),
    between(lon_180, SO_lon_min, SO_lon_max)
  )

line_width <- 0.7
BBAL_color <- '#CCFF00'
GHAL_color <- '#FF69B4'
WAAL_color <- '#FFFFFF'
colony_color <- '#479125FF'

# Build KDE composite for wind
SO_wind_KDE <- ggplot() +
  geom_tile(data = env_grid_SO, aes(x = lon_180, y = centroid_lat, fill = wind_kph)) +
  scale_fill_viridis_c(name = "Wind (km/h)", option = "rocket", direction = -1, limits = c(wind_mps_min*3.6, wind_mps_max*3.6)) +
  geom_sf(data = st_as_sf(BBAL_Polygon), fill = NA, color = BBAL_color, alpha = 0.2, linewidth = line_width) +
  geom_sf(data = st_as_sf(GHAL_Polygon), fill = NA, color = GHAL_color, alpha = 0.2, linewidth = line_width) +
  geom_sf(data = st_as_sf(WAAL_Polygon), fill = NA, color = WAAL_color, alpha = 0.2, linewidth = line_width) +
  geom_point(aes(x=-38.0658417,y=-54.0101833), size = 3, color = colony_color) + 
  geom_sf(data = worldmap) +
  coord_sf(xlim = c(SO_lon_min, SO_lon_max), ylim = c(SO_lat_min, SO_lat_max), expand = FALSE) +
  scale_x_continuous(
    breaks = c(-120,-80,-40,0)
  ) +
  scale_y_continuous(
    breaks = c(-70, -50, -30)
  ) +
  theme(axis.title = element_blank(),
        legend.position = "none")

SO_wind_KDE

# Build KDE composite for waves
SO_waves_KDE <- ggplot() +
  geom_tile(data = env_grid_SO, aes(x = lon_180, y = centroid_lat, fill = swell_m)) +
  scale_fill_viridis_c(name = "Swell height (m)", option="mako", direction = -1, limits = c(swell_m_min, swell_m_max)) +
  geom_sf(data = st_as_sf(BBAL_Polygon), fill = NA, color = BBAL_color, alpha = 0.1, linewidth = line_width) +
  geom_sf(data = st_as_sf(GHAL_Polygon), fill = NA, color = GHAL_color, alpha = 0.1, linewidth = line_width) +
  geom_sf(data = st_as_sf(WAAL_Polygon), fill = NA, color = WAAL_color, alpha = 0.1, linewidth = line_width) +
  # geom_sf(data = st_as_sf(BBAL_Polygon), fill = 'white', color = 'white', alpha = 0.2, linewidth = 0) +
  # geom_sf(data = st_as_sf(GHAL_Polygon), fill = 'white', color = 'white', alpha = 0.2, linewidth = 0) +
  # geom_sf(data = st_as_sf(WAAL_Polygon), fill = 'white', color = 'white', alpha = 0.2, linewidth = 0) +
  geom_point(aes(x=-38.0658417,y=-54.0101833), size = 3, color = colony_color) + 
  geom_sf(data = worldmap) +
  coord_sf(xlim = c(SO_lon_min, SO_lon_max), ylim = c(SO_lat_min, SO_lat_max), expand = FALSE) +
  scale_x_continuous(
    breaks = c(-120,-80,-40,0)
  ) +
  scale_y_continuous(
    breaks = c(-70, -50, -30)
  ) +
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank())

SO_waves_KDE

# Wrap figures
wrap_elements(panel = SO_wind_KDE / SO_waves_KDE)


# Midway -----------------------------------------------------------------------

NP_lat_min <- 10
NP_lat_max <- 70
NP_lon_min <- 90
NP_lon_max <- 270

# Load & preprocess wind/swell data
env_grid_NP <- grid_global_breeding_szn_df %>%
  mutate(
    wind_kph = 3.6 * breeding_szn_si10,
    swell_m = breeding_szn_shts,
    lon_180 = ifelse(centroid_lon > 180, centroid_lon - 360, centroid_lon)
  ) %>%
  filter(
    between(centroid_lat, NP_lat_min, NP_lat_max),
    between(centroid_lon, NP_lon_min, NP_lon_max)
  )

BFAL_color <- '#CCFF00'
LAAL_color <- '#FFFFFF'
colony_color <- '#1170AAFF'


world_left <- st_shift_longitude(st_crop(worldmap, xmin = 90, xmax = 192, ymin = 0, ymax = 90))
world_right <- st_shift_longitude(st_crop(worldmap, xmin = 192, xmax = 270, ymin = 0, ymax = 90))

# Optional: reproject or st_make_valid if needed

# Build KDE composite for wind
NP_wind_KDE <- ggplot() +
  geom_tile(data = env_grid_NP, aes(x = centroid_lon, y = centroid_lat, fill = wind_kph)) +
  scale_fill_viridis_c(name = "Wind (km/h)", option = "rocket", direction = -1, limits = c(wind_mps_min*3.6, wind_mps_max*3.6)) +
  geom_sf(data = st_shift_longitude(st_as_sf(BFAL_Polygon)),fill = NA, color = BFAL_color, alpha = 0.2, linewidth = line_width) +
  geom_sf(data = st_shift_longitude(st_as_sf(LAAL_Polygon)),fill = NA, color = LAAL_color, alpha = 0.2, linewidth = line_width) +
  geom_point(aes(x=360-177.3813,y=28.19989), size = 3, color = colony_color) +
  geom_sf(data = world_left) +
  geom_sf(data = world_right) +
  coord_sf(xlim = c(NP_lon_min, NP_lon_max), ylim = c(NP_lat_min, NP_lat_max), expand = FALSE) +
  scale_x_continuous(
    breaks = c(100, 140, 180, 220, 260)
  ) +
  scale_y_continuous(
    breaks = c(20, 40, 60)
  ) +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

NP_wind_KDE

# Build KDE composite for waves
NP_waves_KDE <- ggplot() +
  geom_tile(data = env_grid_NP, aes(x = centroid_lon, y = centroid_lat, fill = swell_m)) +
  scale_fill_viridis_c(name = "Swell height (m)", option="mako", direction = -1, limits = c(swell_m_min, swell_m_max)) +
  geom_sf(data = st_shift_longitude(st_as_sf(BFAL_Polygon)),fill = NA, color = BFAL_color, alpha = 0.2, linewidth = line_width) +
  geom_sf(data = st_shift_longitude(st_as_sf(LAAL_Polygon)),fill = NA, color = LAAL_color, alpha = 0.2, linewidth = line_width) +
  geom_point(aes(x=360-177.3813,y=28.19989), size = 3, color = colony_color) +
  geom_sf(data = world_left) +
  geom_sf(data = world_right) +
  coord_sf(xlim = c(NP_lon_min, NP_lon_max), ylim = c(NP_lat_min, NP_lat_max), expand = FALSE) +
  scale_x_continuous(
    breaks = c(100, 140, 180, 220, 260)
  ) +
  scale_y_continuous(
    breaks = c(20, 40, 60)
  ) +
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank())

NP_waves_KDE

 
# Wrap figures
wrap_elements(panel = NP_wind_KDE / NP_waves_KDE)

# Patchwork --------------------------------------------------------------------

patchwork <- (NP_wind_KDE + NP_waves_KDE) / (SO_wind_KDE + SO_waves_KDE) & 
  theme(plot.margin = margin(1, 4, 1, 4))

ggsave("~/Desktop/Manuscript_edits/Figures/KDEs_with_env.png", plot = patchwork, width = 11, height = 6, units = "in", dpi = 300)

# Get the legends --------------------------------------------------------------

legend_width <- 28.2

# Wind
wind_with_legend <- ggplot() +
  geom_tile(data = env_grid_NP, aes(x = centroid_lon, y = centroid_lat, fill = wind_kph)) +
  scale_fill_viridis_c(name = NULL, option = "rocket", direction = -1, limits = c(wind_mps_min*3.6, wind_mps_max*3.6),
                       guide = guide_colorbar(
                         barwidth = legend_width,   # length (horizontal) or thickness (vertical)
                         barheight = 1 # thickness (horizontal) or length (vertical)
                       )) +
  coord_sf(xlim = c(NP_lon_min, NP_lon_max), ylim = c(NP_lat_min, NP_lat_max), expand = FALSE) +
  theme(legend.position = "bottom")

wind_with_legend

g <- ggplotGrob(wind_with_legend)

# 3. Locate the legend grob
legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")

# 4. Extract the legend
legend <- g$grobs[[legend_index]]

ggsave(
  filename = "~/Desktop/Manuscript_edits/Figures/wind_legend.png",
  plot = ggdraw(legend),
  width = convertWidth(legend$widths, "in", valueOnly = TRUE) %>% sum(),
  height = convertHeight(legend$heights, "in", valueOnly = TRUE) %>% sum(),
  dpi = 300,
  units = "in"
)

# Swell
swell_with_legend <- ggplot() +
  geom_tile(data = env_grid_NP, aes(x = centroid_lon, y = centroid_lat, fill = swell_m)) +
  scale_fill_viridis_c(
    name = NULL,  # or "" for an empty string
    option = "mako",
    direction = -1,
    limits = c(swell_m_min, swell_m_max),
    guide = guide_colorbar(
      barwidth = legend_width,   # length (horizontal) or thickness (vertical)
      barheight = 1 # thickness (horizontal) or length (vertical)
    )) +
  coord_sf(xlim = c(NP_lon_min, NP_lon_max), ylim = c(NP_lat_min, NP_lat_max), expand = FALSE) +
  theme(legend.position = "bottom")

swell_with_legend

g <- ggplotGrob(swell_with_legend)

# 3. Locate the legend grob
legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")

# 4. Extract the legend
legend <- g$grobs[[legend_index]]

ggsave(
  filename = "~/Desktop/Manuscript_edits/Figures/swell_legend.png",
  plot = ggdraw(legend),
  width = convertWidth(legend$widths, "in", valueOnly = TRUE) %>% sum(),
  height = convertHeight(legend$heights, "in", valueOnly = TRUE) %>% sum(),
  dpi = 300,
  units = "in"
)

