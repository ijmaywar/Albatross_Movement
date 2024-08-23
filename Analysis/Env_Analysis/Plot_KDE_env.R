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

grid_polys_df <- st_read(paste0(GD_dir,"Analysis/Maywar/Global_Env/Global_avg_env_GS_max.gpkg"))

grid_polys_df <- grid_polys_df %>% mutate(breeding_szn_si10 = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("si10_", c("1","2","3","12")))),
                                          breeding_szn_mdts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mdts_", c("1","2","3","12")))),
                                          breeding_szn_mdww = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mdww_", c("1","2","3","12")))),
                                          breeding_szn_mpts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mpww_", c("1","2","3","12")))),
                                          breeding_szn_mwd = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mwd_", c("1","2","3","12")))),
                                          breeding_szn_mwp = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("mwp_", c("1","2","3","12")))),
                                          breeding_szn_swh = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("swh_", c("1","2","3","12")))),
                                          breeding_szn_shts = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("shts_", c("1","2","3","12")))),
                                          breeding_szn_shww = rowMeans(dplyr::select(as.data.frame(grid_polys_df),paste0("shww_", c("1","2","3","12"))))) %>% 
                                          dplyr::select(centroid_lon,centroid_lat,breeding_szn_si10,breeding_szn_shts,geom)

# modify world dataset to remove overlapping portions with world's polygons
grid_polys_df_mod <- grid_polys_df %>% st_difference(polygon)

# Transform
grid_polys_df_rot <- grid_polys_df_mod %>% st_transform(crs = target_crs)

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

ggplot() +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(BBAL_Polygon),fill=NA,color='blue',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(GHAL_Polygon),fill=NA,color='green',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(WAAL_Polygon),fill=NA,color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-120,-10), ylim = c(-75,-32.5), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="BBAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='blue') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="GHAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='green') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="WAAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='black') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='yellow') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggplot() +
  geom_sf(grid_polys_df_mod,mapping=aes(geometry=geom,fill=3.6*breeding_szn_si10),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,3.6*max(grid_polys_df_mod$breeding_szn_si10)),
                      breaks = c(0,10,20,30,40),
                      labels = c("0","10","20","30","40"),
                      name = "Windspeed (km/h)") +
  geom_sf(worldmap,mapping=aes()) + 
  geom_sf(data = st_as_sf(BBAL_Polygon),fill=NA,color='blue',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(GHAL_Polygon),fill=NA,color='green',linewidth=1,alpha=0.5) +
  geom_sf(data = st_as_sf(WAAL_Polygon),fill=NA,color='black',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(-90,-10), ylim = c(-80,-30), expand = FALSE) +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="BBAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='blue') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="GHAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='green') +
  geom_path(data=Bird_Island_GPS_compiled_complete %>% filter(substr(id,1,4)=="WAAL"),aes(x=Lon360to180(lon),y=lat,group=tripID),linewidth=0.1,color='black') +
  geom_point(aes(x=-38.0658417,y=-54.0101833),size=5,color='yellow') + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


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

# Without windfield, with complete GPS track
ggplot() +
  geom_sf(data = st_shift_longitude(st_crop(worldmap,xmin=-120,xmax=120,ymin=10,ymax=70))) + 
  geom_sf(data = st_shift_longitude(st_as_sf(BFAL_Polygon)),fill=NA,color="Blue",linewidth=1,alpha=0.5) + 
  geom_sf(data = st_shift_longitude(st_as_sf(LAAL_Polygon)),fill=NA,color="Green",linewidth=1,alpha=0.5) + 
  coord_sf(xlim = c(140,220), ylim = c(15,55), expand = FALSE) +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL"),aes(x=lon,y=lat,group=tripID),linewidth=0.1,color='blue') +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="LAAL"),aes(x=lon,y=lat,group=tripID),linewidth=0.1,color='green') +
  geom_point(aes(x=360-177.3813,y=28.19989),size=5,color='yellow') +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# With windfield and complete GPS tracks
ggplot() +
  # geom_sf(grid_polys_df_mod,mapping=aes(geometry=geom,fill=3.6*breeding_szn_si10),color=NA,alpha=1) +
  geom_sf(grid_polys_df_rot,mapping=aes(geometry=geom,fill=3.6*breeding_szn_si10),color=NA,alpha=1) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA,
                      limits = c(0,3.6*max(grid_polys_df_rot$breeding_szn_si10)),
                      breaks = c(0,10,20,30,40),
                      labels = c("0","10","20","30","40"),
                      name = "Windspeed (km/h)") +
  geom_sf(worldmap_rot,mapping=aes()) + 
  geom_sf(data = sf::st_shift_longitude(st_as_sf(BFAL_Polygon)),fill=NA,color='blue',linewidth=1,alpha=0.5) +
  geom_sf(data = sf::st_shift_longitude(st_as_sf(LAAL_Polygon)),fill=NA,color='green',linewidth=1,alpha=0.5) +
  coord_sf(xlim = c(Lon360to180(120+90),Lon360to180(240+90)), ylim = c(10,70), expand = FALSE) +
  geom_point(aes(x=90-177.3813,y=28.19989),size=5,color='black') +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="BFAL"),aes(x=Lon360to180(lon+90),y=lat,group=tripID),linewidth=0.1,color='blue') +
  geom_path(data=Midway_GPS_compiled_complete %>% filter(substr(id,1,4)=="LAAL"),aes(x=Lon360to180(lon+90),y=lat,group=tripID),linewidth=0.1,color='green') +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


