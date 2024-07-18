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

# User Functions ---------------------------------------------------------------

Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

# Set environment --------------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
worldmap <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()
fullmeta <- read_excel(paste0(GD_dir,"/metadata/Full_Metadata.xlsx"))
env_dir <- paste0(GD_dir,"/Analysis/Maywar/Wind_KDEs/")

Bird_Island_df <- read_csv(paste0(env_dir,"Bird_Island_KDEs_avg_env.csv"))
Midway_df <- read_csv(paste0(env_dir,"Midway_KDEs_avg_env.csv")) 
compiled_df <- rbind(Bird_Island_df,Midway_df)

# Turn columns into factors
compiled_df$Species <- factor(compiled_df$Species, levels=c("BBAL","GHAL","WAAL","BFAL","LAAL"))
compiled_df$KDE_type <- factor(compiled_df$KDE_type, levels=c("all","Inc","BG"))
compiled_df$on_colony <- factor(compiled_df$on_colony, levels=c(1,0))
compiled_df$Month <- factor(compiled_df$Month, levels=1:12)

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
ggplot(compiled_df %>% filter(KDE_type=="Inc" | KDE_type=="BG"), aes(x = Species, y = si10, fill=KDE_type)) +
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
ggplot(compiled_df %>% filter(KDE_type=="all"), aes(x = Species, y = si10)) +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot() +
  labs(title = "", x = "Species", y = "Average WindSpeed") +
  theme_bw() + 
  facet_wrap(~Month)


