################################################################################
#
# Wind data: create plots with wind data.
# This will need to be edited in order to account for times removed by on-water
# and foraging/resting (HMM)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(readxl)
library(lme4)
library(stringr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(gridExtra)
library(patchwork)
library(gratia)
library(readr)
library(RColorBrewer)
library(viridis)

# Set Environment ---------------------------------------------------------

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/"
read_dir <- paste0(GD_dir, "Analysis/Maywar/Merged_Data/Merged_Hourly_Compiled/")

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(read_dir)
files <- list.files(pattern = '*.csv')
m_all <- read_csv(files[2])

# Classify 2BEP, E_pip, Ep as BG
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","BG")))

# Datetime stuff
m_all$datetime <- as.POSIXlt(m_all$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# Categorize BWAs
m_all <- m_all %>% mutate(bird_wind_angle_cat = case_when(bird_wind_angle<60 ~ "tail",
                                                          bird_wind_angle>=60 & bird_wind_angle<120 ~ "cross",
                                                          bird_wind_angle>=120 ~ "head"))

m_all <- m_all %>% mutate(bird_wave_angle_cat = case_when(bird_wave_angle<60 ~ "tail",
                                                          bird_wave_angle>=60 & bird_wave_angle<120 ~ "cross",
                                                          bird_wave_angle>=120 ~ "head"))

m_all <- m_all %>% mutate(bird_swell_angle_cat = case_when(bird_swell_angle<60 ~ "tail",
                                                           bird_swell_angle>=60 & bird_swell_angle<120 ~ "cross",
                                                           bird_swell_angle>=120 ~ "head"))

m_all <- m_all %>% mutate(bird_ww_angle_cat = case_when(bird_ww_angle<60 ~ "tail",
                                                        bird_ww_angle>=60 & bird_ww_angle<120 ~ "cross",
                                                        bird_ww_angle>=120 ~ "head"))

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$Field_Season <- as.factor(m_all$Field_Season)
m_all$Location <- as.factor(m_all$Location)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$Species <- as.factor(m_all$Species)
m_all$bird_wind_angle_cat <- as.factor(m_all$bird_wind_angle_cat)
m_all$bird_wave_angle_cat <- as.factor(m_all$bird_wave_angle_cat)
m_all$bird_swell_angle_cat <- as.factor(m_all$bird_swell_angle_cat)
m_all$bird_ww_angle_cat <- as.factor(m_all$bird_ww_angle_cat)
m_all$HMM_2S_state <- as.factor(m_all$HMM_2S_state)
m_all$HMM_3S_state <- as.factor(m_all$HMM_3S_state)

# Re-order Species groups and give them their full name
m_all <- m_all %>% mutate(Species = factor(replace(as.character(Species),Species=="BBAL","Black-browed")),
                          Species = factor(replace(as.character(Species),Species=="GHAL","Grey-headed")),
                          Species = factor(replace(as.character(Species),Species=="WAAL","Wandering")),
                          Species = factor(replace(as.character(Species),Species=="BFAL","Black-footed")),
                          Species = factor(replace(as.character(Species),Species=="LAAL","Laysan")))
m_all$Species <- factor(m_all$Species, levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

m_all$bird_wind_angle_cat <- factor(m_all$bird_wind_angle_cat, levels=c("head", "cross", "tail"))
m_all$bird_wave_angle_cat <- factor(m_all$bird_wave_angle_cat, levels=c("head", "cross", "tail"))
m_all$bird_swell_angle_cat <- factor(m_all$bird_swell_angle_cat, levels=c("head", "cross", "tail"))
m_all$bird_ww_angle_cat <- factor(m_all$bird_ww_angle_cat, levels=c("head", "cross", "tail"))

# Add km/hr of wind_vel
m_all$wind_vel_kmh <- 3.6*(m_all$wind_vel)

m_all_nonaflaps <- m_all %>% drop_na(flaps)
m_all_nona_flaps_env <- m_all_nonaflaps %>% drop_na(colnames(m_all)[6:29])
m_all_poscomplete <- m_all %>% filter(Pos_complete==1)

# Sample stats -----------------------------------------------------------------

nrow(m_all)
m_all_nonaflaps %>% count(Species)
m_all_nonaflaps %>% group_by(Species) %>% summarize(unique_IDs=n_distinct(id))

# stats on the performance of HMM in relation to GLS
m_all %>% group_by(GLS_state,HMM_2S_state) %>% summarize(count=n())
m_all %>% group_by(GLS_state,HMM_3S_state) %>% summarize(count=n())

# stats on the performance of OWB in relation to GLS
m_all %>% group_by(GLS_state,OWB_state) %>% summarize(count=n())

m_all_nona_flaps_env %>% filter((HMM_3S_state != 1)) %>% 
  group_by(Species) %>% summarize(max=max(swh))




################################################################################
# te(swell height, swell angle) ------------------------------------------------

fac_k <- 3
GAM_list_swells <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all_nona_flaps_env %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_swells[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                            bird_swell_angle = evenly(bird_swell_angle, n=100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(shts,bird_swell_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(shts,bird_swell_angle)"))[,4:7])
  
  colnames(link_df) <- c("shts","bird_swell_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_swells <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_swells <- rbind(fv_df_swells,link_df)
  }
}

fv_df_swells$Species <- factor(fv_df_swells$Species , 
                               levels=c("Black-browed", "Grey-headed", "Wandering", 
                                        "Black-footed", "Laysan"))

# ggplot(fv_df_swells %>% filter(exp(fitted_global)<2000)) +
ggplot(fv_df_swells) +
  # geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),binwidth = 100) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global))) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all_nona_flaps_env %>% filter(HMM_3S_state!=1),aes(x=shts,y=bird_swell_angle),size=0.001,alpha=1,color="white") + 
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()
# theme(text = element_text(size = 24),
# axis.title.x=element_blank(),
# axis.title.y=element_blank(),
# legend.position = "none")

# Interaction between wave height and period
# ggplot(fv_df_swells %>% filter(exp(fitted_global)<1500)) +
ggplot(fv_df_swells) +
  # geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global))) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=fitted_global)) +
  scale_fill_manual(values=inferno(15),drop=FALSE) +
  # geom_hline(yintercept=60,linetype=2,color="white") +
  # geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour") +
  # ylab(expression(atop("Relative wave angle","(degrees)"))) +
  # scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()
# theme(text = element_text(size = 24),
# axis.title.x=element_blank(),
# axis.title.y=element_blank(),
# legend.position = "none")

# The problem here is that the estimates for flap rate go up to around 450000.
# This makes the scale for the figure completely fucked. I need to create custom
# bins so that the colors reflect only up to about 5000 or so flaps. 

# Metrics
for (i in 1:5) {
  summary_i <- summary(GAM_list_swells[[i]])
  print(c(format(round(summary_i$dev.expl,3),scientific=F),round(summary_i$r.sq,3),round(AIC(GAM_list_swells[[i]]),3)))
}



################################################################################
# te(total wave height, total wave angle) --------------------------------------

fac_k <- 3
GAM_list_total_waves <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  # m_current <- m_all_nona_flaps_env %>% filter((HMM_3S_state != 1) & (Species == spp))
  m_current <- m_all_nona_flaps_env %>% filter((HMM_3S_state != 1) & (Species == spp) & (swh<7.90))
  
  current_GAM <- gam(formula = flaps ~ te(swh,bird_wave_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_total_waves[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, swh = evenly(swh, n = 100),
                            bird_wave_angle = evenly(bird_wave_angle, n=100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(swh,bird_wave_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(swh,bird_wave_angle)"))[,4:7])
  
  colnames(link_df) <- c("swh","bird_wave_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_total_waves <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_total_waves <- rbind(fv_df_total_waves,link_df)
  }
}

fv_df_total_waves$Species <- factor(fv_df_total_waves$Species , 
                               levels=c("Black-browed", "Grey-headed", "Wandering", 
                                        "Black-footed", "Laysan"))

# ggplot(fv_df_total_waves %>% filter(exp(fitted_global)<2000)) +
ggplot(fv_df_total_waves) +
  # geom_contour_filled(aes(swh,bird_wave_angle,z=exp(fitted_global)),binwidth = 100) +
  geom_contour_filled(aes(swh,bird_wave_angle,z=exp(fitted_global))) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all_nona_flaps_env %>% filter(HMM_3S_state!=1),aes(x=swh,y=bird_wave_angle),size=0.001,alpha=1,color="white") + 
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()
# theme(text = element_text(size = 24),
# axis.title.x=element_blank(),
# axis.title.y=element_blank(),
# legend.position = "none")


################################################################################
# How does wind velocity compare to swell and wave height? ---------------------


ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=swh),size=0.001,alpha=1,color="black") + 
  # geom_line(aes(y=exp(fitted_global),x=bird_wind_angle),linewidth=1) +
  # geom_ribbon(fv_df_bird_wind_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_wind_angle),alpha=0.2) +
  # labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# Total waves
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=shts),size=0.001,alpha=1,color="black") + 
  # geom_line(aes(y=exp(fitted_global),x=bird_wind_angle),linewidth=1) +
  # geom_ribbon(fv_df_bird_wind_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_wind_angle),alpha=0.2) +
  # labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# Wind waves
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=shww),size=0.001,alpha=1,color="black") + 
  # geom_line(aes(y=exp(fitted_global),x=bird_wind_angle),linewidth=1) +
  # geom_ribbon(fv_df_bird_wind_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_wind_angle),alpha=0.2) +
  # labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# VERY HIGH correlation between wind_vel and shww because wind cause wind waves.
# Pretty high correlation between wind_vel and swh because wind waves are a component of total waves.
# No/low correlation between wind_vel and shts because wind_vel doesn't directly effect swell.


m_all_poscomplete |>
  ggplot(aes(Species,mwp)) +
  geom_boxplot(width=0.4) +
  theme_bw()

m_all_poscomplete |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  theme_bw()

m_all_poscomplete |>
  ggplot(aes(Species,mpts)) +
  geom_boxplot(width=0.4) +
  theme_bw()

m_all_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  theme_bw()



# At low windspeeds, NP birds are flapping less
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  theme_bw()

# At low windspeeds, the height of swells == total wave height and are 
# greater for NP spp
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  theme_bw()
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  theme_bw()
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  theme_bw()

# At high windspeeds, the height of swells == total wave height and are 
# greater for NP spp
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  theme_bw()
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  theme_bw()
m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  theme_bw()


ggplot(m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25))) +
  geom_point(aes(x=swh,y=flaps),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()






################################################################################
# s() terms --------------------------------------------------------------------

fac_k <- 3
GAM_list_swells <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all_nona_flaps_env %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(bird_wind_angle,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_swells[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, bird_wind_angle = evenly(bird_wind_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(bird_wind_angle)","s(id)"))[,3:6],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(bird_wind_angle)"))[,3:6])
  
  colnames(link_df) <- c("bird_wind_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_bird_wind_angle <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_bird_wind_angle <- rbind(fv_df_bird_wind_angle,link_df)
  }
}

fv_df_bird_wind_angle$Species <- factor(fv_df_bird_wind_angle$Species , levels=c("Black-browed", 
                                                                                 "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Line plot for bird_wind_angle
ggplot(fv_df_bird_wind_angle) +
  geom_line(aes(y=exp(fitted_global),x=bird_wind_angle),linewidth=1) +
  geom_ribbon(fv_df_bird_wind_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_wind_angle),alpha=0.2) +
  labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list[[i]])
  print(c(format(round(summary_i$dev.expl,3),scientific=F),round(summary_i$r.sq,3),round(AIC(GAM_list[[i]]),3)))
}







# # Continuous figure for all species
cont_all <- ggplot(fv_df_cont) +
  geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(13),drop=FALSE) +
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour", x="Windspeed (km/h)") +
  ylab(expression(atop("Relative wind angle","(degrees)"))) +
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")



# # Continuous figure for all species
cont_all <- ggplot(fv_df_cont) +
  geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(13),drop=FALSE) +
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour", x="Windspeed (km/h)") +
  ylab(expression(atop("Relative wind angle","(degrees)"))) +
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

# Continuous for SO species
cont_SO <- ggplot(fv_df_cont %>% filter (Species %in% c("Black-browed","Grey-headed","Wandering"))) +
  geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(13),drop=FALSE) +
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour", x="Windspeed (km/h)") +
  ylab(expression(atop("Relative wind angle","(degrees)"))) +
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

# SO species with scatter overlay
ggplot(fv_df_cont %>% filter (Species %in% c("Black-browed","Grey-headed","Wandering"))) +
  geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(13),drop=FALSE) +
  geom_point(data = m_all_nonaflapsbwas %>% filter(HMM_3S_state!=1 & 
                                                     Species %in% c("Black-browed","Grey-headed","Wandering")),
             aes(x=wind_vel_kmh,y=bwa),size=0.001,alpha=1,color="white") + 
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour", x="Windspeed (km/h)") +
  ylab(expression(atop("Relative wind angle","(degrees)"))) +
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

# all species with scatter overlay
ggplot(fv_df_cont) +
  geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(13),drop=FALSE) +
  geom_point(data = m_all_nonaflapsbwas %>% filter(HMM_3S_state!=1),
             aes(x=wind_vel_kmh,y=bwa),size=0.001,alpha=1,color="white") + 
  geom_hline(yintercept=60,linetype=2,color="white") +
  geom_hline(yintercept=120,linetype=2,color="white") +
  labs(fill = "Flaps/hour", x="Windspeed (km/h)") +
  ylab(expression(atop("Relative wind angle","(degrees)"))) +
  scale_y_continuous(breaks=c(0,60,120,180)) + 
  facet_wrap(~Species,nrow=2) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")


# # Link for NP species
# ggplot(fv_df_cont %>% filter (Species %in% c("BFAL","LAAL"))) +
#   geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100) +
#   # geom_contour(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),breaks=550) +
#   scale_fill_manual(values=mycolors,drop=FALSE) +
#   geom_hline(yintercept=45,linetype=2) +
#   geom_hline(yintercept=135,linetype=2) +
#   labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
#   facet_wrap(~Species,nrow=1) + 
#   theme(text = element_text(size = 24))

# # Add scatter plots of (wind_vel_kmh,bwa) data
# ggplot(fv_df_cont) +
#   geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=wind_vel_kmh,y=bwa),size=0.001,alpha=1) + 
#   geom_contour_filled(aes(wind_vel_kmh,bwa,z=exp(fitted_global)),binwidth = 100,alpha=0.9) +
#   scale_fill_manual(values=mycolors,drop=FALSE) +
#   labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
#   facet_wrap(~Species)


# Flaps/hour vs wind_vel_kmh (categorical) after removing HMM_3S_state == 1 ----------------------

GAM_categorical_list <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all_nonaflapsbwas %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,BWA_cat,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_categorical_list[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100), 
                            id = unique(m_current$id)[1:10],
                            BWA_cat = unique(m_current$BWA_cat))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh,BWA_cat)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh,BWA_cat)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(wind_vel_kmh,BWA_cat)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel_kmh","id","BWA_cat","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_fs","se_fs","lower_fs","upper_fs",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "Black-browed") {
    ds_df_cat <- current_ds
    fv_df_cat <- link_df
  } else {
    ds_df_cat <- rbind(ds_df_cat,current_ds)
    fv_df_cat <- rbind(fv_df_cat,link_df)
  }
}

fv_df_cat$Species <- factor(fv_df_cat$Species,levels=c("Black-browed", 
                                                       "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# fv_df_cat <- fv_df_cat %>%  mutate(Species = factor(replace(as.character(Species),Species=="BBAL","Black-browed")),
#                Species = factor(replace(as.character(Species),Species=="GHAL","Grey-headed")),
#                Species = factor(replace(as.character(Species),Species=="WAAL","Wandering")),
#                Species = factor(replace(as.character(Species),Species=="BFAL","Black-footed")),
#                Species = factor(replace(as.character(Species),Species=="LAAL","Laysan")))

# Link: global for all app
cat_all <- fv_df_cat |>
  ggplot(aes(wind_vel_kmh,exp(fitted_global),color=BWA_cat)) +
  geom_line(linewidth=1.5) +
  # geom_line(aes(wind_vel_kmh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=BWA_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour",x="Windspeed (km/h)") +
  # xlim(0,25) + 
  ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        strip.text.x = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

# Link: global for SO spp
cat_SO <- fv_df_cat %>% filter(Species %in% c("Black-browed","Grey-headed","Wandering"))|>
  ggplot(aes(wind_vel_kmh,exp(fitted_global),color=BWA_cat)) +
  geom_line(linewidth=1.5) +
  # geom_line(aes(wind_vel_kmh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=BWA_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour",x="Windspeed (km/h)") +
  # xlim(0,25) + 
  ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x=element_blank(),
        strip.text.x = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

# # Link: global for NP spp
# fv_df_cat %>% filter(Species %in% c("BFAL","LAAL"))|>
#   ggplot(aes(wind_vel_kmh,exp(fitted_global),color=BWA_cat)) +
#   geom_line() +
#   # geom_line(aes(wind_vel_kmh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
#   geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=BWA_cat),alpha=0.3) +
#   guides(color=guide_legend(title="Relative wind condition")) +
#   scale_color_manual(values=c("head" = "#e74c3c",
#                               "cross" = "#2980b9",
#                               "tail" = "#27ae60")) + 
#   labs(y="Flaps per hour",x="Wind velocity (m/s)") +
#   # xlim(0,25) + 
#   ylim(0,1500) +
#   facet_wrap(~Species,nrow = 1) + 
#   theme(text = element_text(size = 24))



# Plot figures on top of eaechother --------------------------------------------

# Use the tag label as an x-axis label
wrap_elements(panel = cont_SO / cat_SO) +
  labs(tag = "Windspeed (km/h)") +
  theme(
    plot.tag = element_text(size = 24),
    plot.tag.position = "bottom"
  )


wrap_elements(panel = cont_all / cat_all) +
  labs(tag = "Windspeed (km/h)") +
  theme(
    plot.tag = element_text(size = 24),
    plot.tag.position = "bottom"
  )
