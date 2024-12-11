################################################################################
# 
# Env data: Create plots for env data in the 95% KDEs
# 
################################################################################

# Clear environment ------------------------------------------------------------

rm(list = ls())

# Packages  --------------------------------------------------------------------

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
library(viridis)
library(scales)
library(patchwork)

# User functions ---------------------------------------------------------------

Lon360to180 <- function(longitudes) {
  return(ifelse(longitudes > 180, longitudes - 360, longitudes))
}

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))
worldmap <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()

Bird_Island_df <- read_csv(paste0(GD_dir,"L1/Bird_Island/Env_Data/ERA5_MonthlyAvg_10m/Bird_Island_KDEs_avg_env.csv"))
Midway_df <- read_csv(paste0(GD_dir,"L1/Midway/Env_Data/ERA5_MonthlyAvg_10m/Midway_KDEs_avg_env.csv"))
compiled_df <- rbind(Bird_Island_df,Midway_df)

# Turn columns into factors
compiled_df$Species <- factor(compiled_df$Species, levels=c("BBAL","GHAL","WAAL","BFAL","LAAL"))
compiled_df$KDE_type <- factor(compiled_df$KDE_type, levels=c("all","Inc","BG"))
compiled_df$on_colony <- factor(compiled_df$on_colony, levels=c(1,0))
compiled_df$Month <- factor(compiled_df$Month, levels=1:12)

# Load global env grid
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

# Box plots comparing average windspeeds during TripTypes ----------------------

# REPLACE THE Y VALUES IN ORDER TO CHANGE THE ENV VAR YOU WANT TO LOOK AT.

# Create plot for on and off colony wind data
ggplot(compiled_df %>% filter(KDE_type=="all"), aes(x = Species, y = si10, fill=on_colony)) +
  scale_fill_brewer(palette = "Set1") +
  geom_boxplot() +
  labs(title = "", x = "Species", y = "Average WindSpeed") +
  # ylim(0,12) + 
  scale_y_continuous(limits=c(0,12),
                     breaks=seq(0,12,by=2)) +
  theme_bw()

# Create plot for Inc and BG wind data
ggplot(compiled_df %>% filter(on_colony==1 &
                                KDE_type=="Inc" | KDE_type=="BG"), aes(x = Species, y = si10, fill=KDE_type)) +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot() +
  labs(title = "", x = "Species", y = "Average WindSpeed") +
  ylim(0,12)

# Create plot for all on-colony data
ggplot(compiled_df %>% filter(KDE_type=="all" & on_colony==1), aes(x = Species, y = si10)) +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot() +
  labs(title = "", x = "Species", y = "Average WindSpeed") +
  scale_y_continuous(limits=c(0,12),
                     breaks=seq(0,12,by=2)) +
  theme_bw()

# Create plot for months of the year
ggplot(compiled_df %>% filter(KDE_type=="all"), aes(x = Species, y = si10*3.6)) +
  geom_boxplot(width=0.6,outliers = FALSE) +
  labs(y = "Average windspeed (km/h)") +
  theme_linedraw() + 
  facet_wrap(~Month, nrow=1) + 
  theme(axis.title.x = element_blank(),
        panel.spacing = unit(4, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# Create plot for months of the year
ggplot(compiled_df %>% filter(KDE_type=="BG"), aes(x = Species, y = si10)) +
  geom_boxplot() +
  labs(title = "", x = "Species") + #, y = "Average WindSpeed") +
  theme_bw() + 
  facet_wrap(~Month)

# Create plot for months of the year
ggplot(compiled_df %>% filter(KDE_type=="Inc"), aes(x = Species, y = si10)) +
  geom_boxplot() +
  labs(title = "", x = "Species") + #, y = "Average WindSpeed") +
  theme_bw() + 
  facet_wrap(~Month)


# Plot the SO KDEs -------------------------------------------------------------

GPS_dir <- paste0(GD_dir,"L4/Bird_Island/Tag_Data/GPS/")
setwd(GPS_dir)
KDEs <- list.files(recursive = FALSE) # GPS files 
BBAL_Polygon <- vect("BBAL_all_KDE_95.gpkg")
crs(BBAL_Polygon) <- crs(worldmap)
GHAL_Polygon <- vect("GHAL_all_KDE_95.gpkg")
crs(GHAL_Polygon) <- crs(worldmap)
WAAL_Polygon <- vect("WAAL_all_KDE_95.gpkg")
crs(WAAL_Polygon) <- crs(worldmap)

Bird_Island_GPS_compiled_complete <- read_csv(paste0(GD_dir,"L2/Bird_Island/Tag_Data/GPS/compiled_2019_2022/compiled_complete/Bird_Island_Compiled_600s_compiled_complete.csv"))
Bird_Island_GPS_compiled_complete$datetime <- as.POSIXlt(Bird_Island_GPS_compiled_complete$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# All KDEs at the same time
SO_KDEs <- ggplot() +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(BBAL_Polygon),fill='black',color='black',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(GHAL_Polygon),fill='#6A00A8FF',color='#6A00A8FF',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(WAAL_Polygon),fill='#FCA636FF',color='#FCA636FF',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-120,-10), ylim = c(-72.5,-32.5), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="BBAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="GHAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='#6A00A8FF') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="WAAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='#FCA636FF') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='#479125FF') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

SO_KDEs

# Individually
BBAL_KDE <- ggplot() +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(BBAL_Polygon),fill='black',color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-120,-10), ylim = c(-72.5,-32.5), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="BBAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='#479125FF') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

GHAL_KDE <- ggplot() +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(GHAL_Polygon),fill='black',color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-120,-10), ylim = c(-72.5,-32.5), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="GHAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='#479125FF') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

WAAL_KDE <- ggplot() +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(WAAL_Polygon),fill='black',color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-120,-10), ylim = c(-72.5,-32.5), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="WAAL"),
            aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='#479125FF') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

wrap_elements(panel = BBAL_KDE / GHAL_KDE / WAAL_KDE)

# Extent map of SO foraging map
ggplot() + 
  geom_sf(worldmap_rot,mapping=aes()) + # worldmap_rot is from Plot_Global_Env.R
  geom_rect(aes(xmin = 90-120, ymin = -72.5, xmax = 90-10, ymax = -32.5), 
            fill = NA, colour = "black", size = 1) + 
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_linedraw() + 
  xlim(-177,180) +
  theme(text = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# Plot the NP KDEs -------------------------------------------------------------

GPS_dir <- paste0(GD_dir,"L4/Midway/Tag_Data/GPS/")
setwd(GPS_dir)
KDEs <- list.files(recursive = FALSE) # GPS files 
BFAL_Polygon <- vect("BFAL_all_KDE_95.gpkg")
crs(BFAL_Polygon) <- crs(worldmap)
LAAL_Polygon <- vect("LAAL_all_KDE_95.gpkg")
crs(LAAL_Polygon) <- crs(worldmap)

Midway_GPS_compiled_complete <- read_csv(paste0(GD_dir,"L2/Midway/Tag_Data/GPS/compiled_2018_2023/compiled_complete/Midway_Compiled_600s_compiled_complete.csv"))
Midway_GPS_compiled_complete$datetime <- as.POSIXlt(Midway_GPS_compiled_complete$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# All NP species
NP_KDEs <- ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=70))) + 
  geom_sf(data = st_shift_longitude(st_as_sf(BFAL_Polygon)),fill='black',color='black',linewidth=1,alpha=0.5) +
  geom_sf(data = st_shift_longitude(st_as_sf(LAAL_Polygon)),fill='#6A00A8FF',color='#6A00A8FF',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(140,220), ylim = c(15,55), expand = FALSE) +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL"),
            aes(x=lon,y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="LAAL"),
            aes(x=lon,y=lat,group=tripID),linewidth=0.2,color='#6A00A8FF') +
  geom_point(aes(x=360-177.3813,y=28.19989),size=5,color='#1170AAFF') +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

NP_KDEs

# Individually
BFAL_KDE <- ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=70))) + 
  geom_sf(data = st_shift_longitude(st_as_sf(BFAL_Polygon)),fill='black',color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(140,220), ylim = c(15,55), expand = FALSE) +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL"),
            aes(x=lon,y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_point(aes(x=360-177.3813,y=28.19989),size=5,color='#1170AAFF') +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

LAAL_KDE <- ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=70))) + 
  geom_sf(data = st_shift_longitude(st_as_sf(LAAL_Polygon)),fill='black',color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(140,220), ylim = c(15,55), expand = FALSE) +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="LAAL"),
            aes(x=lon,y=lat,group=tripID),linewidth=0.2,color='black') +
  geom_point(aes(x=360-177.3813,y=28.19989),size=5,color='#1170AAFF') +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

wrap_elements(panel = BFAL_KDE / LAAL_KDE)

# Extent map of SO foraging map
ggplot() + 
  geom_sf(worldmap_rot,mapping=aes()) + # worldmap_rot is from Plot_Global_Env.R
  geom_rect(aes(xmin = 90-220, ymin = 15, xmax = 90-140, ymax = 55), 
            fill = NA, colour = "black", size = 1) + 
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_linedraw() + 
  xlim(-177,180) +
  theme(text = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# Plot both KDE maps
wrap_elements(panel = SO_KDEs / NP_KDEs)

# max x max



# Plot all species individually
library(patchwork)
BBAL_KDE + GHAL_KDE + WAAL_KDE + BFAL_KDE + LAAL_KDE +
  plot_layout(ncol = 3)
# 1200 x 600

