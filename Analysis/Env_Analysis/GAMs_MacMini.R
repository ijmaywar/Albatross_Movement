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
library(ks)
library(sp)
library(terra)
library(tidyterra)
library(BAMMtools)
library(see)

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

spp_vec <- c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")

# Sample stats -----------------------------------------------------------------

nrow(m_all)
m_all_nonaflaps %>% count(Species)
m_all_nonaflaps %>% group_by(Species) %>% summarize(unique_IDs=n_distinct(id))

temp <- m_all_nonaflaps %>% group_by(id) %>% summarize(n_rows=n())
temp2 <- m_all_poscomplete %>% group_by(id) %>% summarize(n_rows=n())

# stats on the performance of HMM in relation to GLS
m_all %>% group_by(GLS_state,HMM_2S_state) %>% summarize(count=n())
m_all %>% group_by(GLS_state,HMM_3S_state) %>% summarize(count=n())

# stats on the performance of OWB in relation to GLS
m_all %>% group_by(GLS_state,OWB_state) %>% summarize(count=n())

m_all_nona_flaps_env %>% filter((HMM_3S_state != 1)) %>% 
  group_by(Species) %>% summarize(max=max(swh))


################################################################################
# s(wind_vel_kmh), s(id) -------------------------------------------------------

fac_k <- 3
GAM_list_wind_vel_kmh <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  # m_current <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  # # Limit data to 5-95% quantile bounds.
  # m_current <- m_current %>% filter(wind_vel_kmh>quantile(m_current$wind_vel_kmh,probs=0.05) & 
  #                                     wind_vel_kmh<quantile(m_current$wind_vel_kmh,probs=0.95))
  
  # # filter for low winds
  # m_current <- m_current %>% filter(wind_vel_kmh<25)
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_wind_vel_kmh[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(id)"))[,3:6],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)"))[,3:6])
  
  colnames(link_df) <- c("wind_vel_kmh","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_wind_vel_kmh <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_wind_vel_kmh <- rbind(fv_df_wind_vel_kmh,link_df)
  }
}

fv_df_wind_vel_kmh$Species <- factor(fv_df_wind_vel_kmh$Species, 
 levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Line plot for wind_vel_kmh
ggplot(fv_df_wind_vel_kmh) +
  geom_line(aes(y=exp(fitted_global),x=wind_vel_kmh),linewidth=1) +
  geom_ribbon(fv_df_wind_vel_kmh,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=wind_vel_kmh),alpha=0.2) +
  geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=wind_vel_kmh,y=flaps),size=0.001,alpha=.1,color="black") + 
  labs(y="Flaps/hour") +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()



################################################################################
# s(bird_wind_angle), s(id) -------------------------------------------------------

fac_k <- 3
GAM_list_bird_wind_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(bird_wind_angle,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_bird_wind_angle[[spp]] <- current_GAM
  
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

fv_df_bird_wind_angle$Species <- factor(fv_df_bird_wind_angle$Species, 
                                     levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Line plot for bird_wind_angle
ggplot(fv_df_bird_wind_angle) +
  geom_line(aes(y=exp(fitted_global),x=bird_wind_angle),linewidth=1) +
  geom_ribbon(fv_df_bird_wind_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_wind_angle),alpha=0.2) +
  geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=bird_wind_angle,y=flaps),size=0.001,alpha=.1,color="black") + 
  labs(y="Flaps/hour") +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_bird_wind_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_bird_wind_angle[[i]]),3))))
}

################################################################################
# s(wind_vel_kmh), s(bird_wind_angle), s(id) -----------------------------------

fac_k <- 3
GAM_list_wind_vel_plus_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                       s(bird_wind_angle,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_wind_vel_plus_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            bird_wind_angle = evenly(bird_wind_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(bird_wind_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(bird_wind_angle)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","bird_wind_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_wind_vel_plus_angle <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_wind_vel_plus_angle <- rbind(fv_df_wind_vel_plus_angle,link_df)
  }
}

fv_df_wind_vel_plus_angle$Species <- factor(fv_df_wind_vel_plus_angle$Species, 
                                        levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

ggplot(fv_df_wind_vel_plus_angle) +
  geom_contour_filled(aes(wind_vel_kmh,bird_wind_angle,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=wind_vel_kmh,y=bird_wind_angle),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_wind_vel_plus_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_wind_vel_plus_angle[[i]]),3))))
}

################################################################################
# te(wind_vel_kmh,bird_wind_angle), s(id) --------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
                         drop_na(wind_vel_kmh,bird_wind_angle)

  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bird_wind_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_wind_vel_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            bird_wind_angle = evenly(bird_wind_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","bird_wind_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_wind_vel_angle <- link_df
  } else {
    fv_df_te_wind_vel_angle <- rbind(fv_df_te_wind_vel_angle,link_df)
  }
}

fv_df_te_wind_vel_angle$Species <- factor(fv_df_te_wind_vel_angle$Species , 
  levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

fv_df_te_wind_vel_angle_trim <- distinct(fv_df_te_wind_vel_angle %>% 
               dplyr::select(wind_vel_kmh,bird_wind_angle,fitted_global,Species))

ggplot(fv_df_te_wind_vel_angle_trim) +
  geom_contour_filled(aes(wind_vel_kmh,bird_wind_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_wind_vel_angle_trim$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1) %>% drop_na(wind_vel_kmh,bird_wind_angle),
    # aes(x=shts,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_te_wind_vel_angle[[i]]),3))))
}

################################################################################
# s(wind_vel_kmh,bird_wind_angle_cat), s(id) ----------------------------------------------

GAM_list_wind_vel_kmh_cat <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,bird_wind_angle)
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,bird_wind_angle_cat,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_wind_vel_kmh_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bird_wind_angle_cat = unique(m_current$bird_wind_angle_cat))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh,bird_wind_angle_cat)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh,bird_wind_angle_cat)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","bird_wind_angle_cat","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_wind_vel_kmh_cat <- link_df
  } else {
    fv_df_wind_vel_kmh_cat <- rbind(fv_df_wind_vel_kmh_cat,link_df)
  }
}

fv_df_wind_vel_kmh_cat$Species <- factor(fv_df_wind_vel_kmh_cat$Species,levels=c("Black-browed", 
                                                                 "Grey-headed", "Wandering", "Black-footed", "Laysan"))

ggplot(fv_df_wind_vel_kmh_cat) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global),color=bird_wind_angle_cat)) +
  # geom_line(aes(wind_vel_kmh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wind_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour",x="Windspeed (km/h)") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

################################################################################
# s(shts), s(id) -------------------------------------------------------

fac_k <- 3
GAM_list_shts <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  # m_current <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  # # Limit data to 5-95% quantile bounds.
  # m_current <- m_current %>% filter(shts>quantile(m_current$shts,probs=0.05) & 
  #                                     shts<quantile(m_current$shts,probs=0.95))
  
  # # filter for low winds
  # m_current <- m_current %>% filter(shts<25)
  
  current_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_shts[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts)","s(id)"))[,3:6],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts)"))[,3:6])
  
  colnames(link_df) <- c("shts","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_shts <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_shts <- rbind(fv_df_shts,link_df)
  }
}

fv_df_shts$Species <- factor(fv_df_shts$Species, 
                                     levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Line plot for shts
ggplot(fv_df_shts) +
  geom_line(aes(y=exp(fitted_global),x=shts),linewidth=1) +
  geom_ribbon(fv_df_shts,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=shts),alpha=0.2) +
  geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=shts,y=flaps),size=0.001,alpha=.1,color="black") + 
  labs(y="Flaps/hour") +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_shts[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_shts[[i]]),3))))
}

################################################################################
# s(bird_swell_angle), s(id) -------------------------------------------------------

fac_k <- 3
GAM_list_bird_swell_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(bird_swell_angle,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_bird_swell_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, bird_swell_angle = evenly(bird_swell_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(bird_swell_angle)","s(id)"))[,3:6],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(bird_swell_angle)"))[,3:6])
  
  colnames(link_df) <- c("bird_swell_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_bird_swell_angle <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_bird_swell_angle <- rbind(fv_df_bird_swell_angle,link_df)
  }
}

fv_df_bird_swell_angle$Species <- factor(fv_df_bird_swell_angle$Species, 
                                        levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Line plot for bird_swell_angle
ggplot(fv_df_bird_swell_angle) +
  geom_line(aes(y=exp(fitted_global),x=bird_swell_angle),linewidth=1) +
  geom_ribbon(fv_df_bird_swell_angle,mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,x=bird_swell_angle),alpha=0.2) +
  geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=bird_swell_angle,y=flaps),size=0.001,alpha=.1,color="black") + 
  labs(y="Flaps/hour") +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_bird_swell_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_bird_swell_angle[[i]]),3))))
}

################################################################################
# s(shts), s(bird_swell_angle), s(id) -----------------------------------

fac_k <- 3
GAM_list_shts_plus_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
                       s(bird_swell_angle,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_shts_plus_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                            bird_swell_angle = evenly(bird_swell_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts)","s(bird_swell_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts)","s(bird_swell_angle)"))[,4:7])
  
  colnames(link_df) <- c("shts","bird_swell_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_shts_plus_angle <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_shts_plus_angle <- rbind(fv_df_shts_plus_angle,link_df)
  }
}

fv_df_shts_plus_angle$Species <- factor(fv_df_shts_plus_angle$Species, 
                                            levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

ggplot(fv_df_shts_plus_angle) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),binwidth = 100) +
  # geom_contour_filled(aes(shts,shts,z=exp(fitted_global))) +
  scale_fill_manual(values=inferno(10),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=shts,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_shts_plus_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_shts_plus_angle[[i]]),3))))
}

################################################################################
# te(shts,bird_swell_angle), s(id) ---------------------------------------------

fac_k <- 3
GAM_list_te_shts_angle <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
                         drop_na(shts,bird_swell_angle)
  
  current_GAM <- gam(formula = flaps ~ te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_shts_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                            bird_swell_angle = evenly(bird_swell_angle, n = 100),
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
    fv_df_te_shts_angle <- link_df
  } else {
    fv_df_te_shts_angle <- rbind(fv_df_te_shts_angle,link_df)
  }
}

fv_df_te_shts_angle$Species <- factor(fv_df_te_shts_angle$Species , 
  levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

fv_df_te_shts_angle_trim <- distinct(fv_df_te_shts_angle %>% 
                              dplyr::select(shts,bird_swell_angle,fitted_global,Species))

ggplot(fv_df_te_shts_angle_trim) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
    breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=shts,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

# Blues for swells
blues <- colorRampPalette(c("white", "navy"))
ggplot(fv_df_te_shts_angle_trim) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
    breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),21)) +
  scale_fill_manual(values=blues(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=shts,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

ggplot(fv_df_te_shts_angle_trim) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=shts,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_shts_angle[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_te_shts_angle[[i]]),3))))
}




################################################################################
# s(shts,bird_swell_angle_cat), s(id) ------------------------------------------

GAM_list_shts_cat <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(shts,bird_swell_angle_cat,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_shts_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bird_swell_angle_cat = unique(m_current$bird_swell_angle_cat)[1:3])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts,bird_swell_angle_cat)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(shts,bird_swell_angle_cat)"))[,4:7])
  
  colnames(link_df) <- c("shts","id","bird_swell_angle_cat","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_shts_cat <- link_df
  } else {
    fv_df_shts_cat <- rbind(fv_df_shts_cat,link_df)
  }
}

fv_df_shts_cat$Species <- factor(fv_df_shts_cat$Species,levels=c("Black-browed", 
                          "Grey-headed", "Wandering", "Black-footed", "Laysan"))

ggplot(fv_df_shts_cat) +
  geom_line(aes(shts,exp(fitted_global),color=bird_swell_angle_cat)) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_swell_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative swell angle")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour",x="Significant swell height (m)") +
  # xlim(0,25) + 
  ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

################################################################################
# s(wind_vel_kmh), s(shts), s(id) -----------------------------------

fac_k <- 3
GAM_list_wind_vel_kmh_plus_shts <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                       s(shts,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_wind_vel_kmh_plus_shts[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            shts = evenly(shts, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(shts)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(shts)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","shts","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_wind_vel_kmh_plus_shts <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_wind_vel_kmh_plus_shts <- rbind(fv_df_wind_vel_kmh_plus_shts,link_df)
  }
}

fv_df_wind_vel_kmh_plus_shts$Species <- factor(fv_df_wind_vel_kmh_plus_shts$Species, 
                                        levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

ggplot(fv_df_wind_vel_kmh_plus_shts) +
  geom_contour_filled(aes(wind_vel_kmh,shts,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(24),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

# ************* PLOT WITH DATA DENSITIES ************
ggplot(fv_df_wind_vel_kmh_plus_shts) +
  geom_contour_filled(aes(wind_vel_kmh,shts,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=inferno(24),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  geom_density_2d(m_all_poscomplete, mapping=aes(x=wind_vel_kmh, y=shts)) +
  # scale_fill_manual(values=inferno(12),drop=FALSE) +
  geom_point(data = m_all_,aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_wind_vel_kmh_plus_shts[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_wind_vel_kmh_plus_shts[[i]]),3))))
}

################################################################################
# te(wind_vel_kmh,shts), s(id) --------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_shts <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,shts)
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_wind_vel_shts[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            shts = evenly(shts, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","shts","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_te_wind_vel_shts <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_te_wind_vel_shts <- rbind(fv_df_te_wind_vel_shts,link_df)
  }
}

fv_df_te_wind_vel_shts$Species <- factor(fv_df_te_wind_vel_shts$Species , 
                                      levels=c("Black-browed", "Grey-headed", "Wandering", 
                                               "Black-footed", "Laysan"))

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_shts[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_te_wind_vel_shts[[i]]),3))))
}

# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_all <- list()
for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_all %>% 
                          filter(HMM_3S_state != 1, Species == spp) %>% 
                          dplyr::select(wind_vel_kmh,shts), 
                        compute.cont=TRUE,gridsize = grid_size)
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                           z=estimate, levels=cont["1%"])[[1]]))
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_current <- rast(fv_df_te_wind_vel_shts %>% filter(Species == spp) %>% select(wind_vel_kmh, shts, fitted_global), type='xyz')
  response_rast_mask_current = terra::mask(response_rast_current, contour_99_current_vect)
  response_df_mask_current = as.data.frame(response_rast_mask_current, xy=T)
  
  # Save values for all spp
  # response_df_mask_all[[spp]] <- response_df_mask_current
  
  contour_99_current$Species <- spp
  response_df_mask_current$Species <- spp
  
  if (spp == "Black-browed") {
    contour_99_all <- contour_99_current
    response_df_mask_all <- response_df_mask_current
  } else {
    contour_99_all <- rbind(contour_99_all,contour_99_current)
    response_df_mask_all <- rbind(response_df_mask_all,response_df_mask_current)
  }
  
}

# Try breaking it so that everything over 1500 is bright yellow, and everything
# else is evenly breaked.


response_df_mask_all$Species <- factor(response_df_mask_all$Species, 
  levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Figures: geom_contour with wrap applied
ggplot(response_df_mask_all) +
  # geom_contour_filled(aes(x,y,z=exp(fitted_global)),binwidth=100) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_all$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

# plot everything and layer wrap contour on top
# this is with the RAW GAM output
contour_99_all |> 
  mutate(across(Species, ~factor(., levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))))
fv_df_te_wind_vel_shts |> 
  mutate(across(Species, ~factor(., levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))))
m_all |> 
  mutate(across(Species, ~factor(., levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))))

ggplot(fv_df_te_wind_vel_shts) +
  geom_contour_filled(aes(wind_vel_kmh,shts,z=fitted_global)) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  geom_path(data=contour_99_all,aes(x,y),color="red") +
  geom_point(data = m_all %>% filter(HMM_3S_state!=1),
             aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()
  


################################################################################
# te(wind_vel_kmh,bird_wind_angle), te(shts,bird_swell_angle), s(id) -----------

fac_k <- 3
GAM_list_te_all <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bird_wind_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_all[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            bird_wind_angle = evenly(bird_wind_angle,n=100),
                            shts = evenly(shts, n = 100),
                            bird_swell_angle = evenly(bird_swell_angle,n=100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)","te(shts,bird_swell_angle)","s(id)"))[,6:9],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)","te(shts,bird_swell_angle)"))[,6:9])
  
  colnames(link_df) <- c("wind_vel_kmh","bird_wind_angle","shts","swell_wind_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cont <- current_ds
    fv_df_te_all <- link_df
  } else {
    # ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_te_all <- rbind(fv_df_te_all,link_df)
  }
}

fv_df_te_all$Species <- factor(fv_df_te_all$Species , 
                                         levels=c("Black-browed", "Grey-headed", "Wandering", 
                                                  "Black-footed", "Laysan"))

ggplot(fv_df_te_all) +
  geom_contour_filled(aes(wind_vel_kmh,shts,z=exp(fitted_global)),binwidth = 10000) +
  scale_fill_manual(values=inferno(7),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # geom_point(data = m_all %>% filter(HMM_3S_state!=1),aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_shts[[i]])
  print(c(as.numeric(format(round(summary_i$dev.expl,3),scientific=F)),
          as.numeric(round(summary_i$r.sq,3)),
          as.numeric(round(AIC(GAM_list_te_wind_vel_shts[[i]]),3))))
}


################################################################################










################################################################################
# Proportion of time spent in swells and winds ---------------------------------

# Proportion of time spent across shts
more_shts_stats <- m_all_poscomplete %>% 
  group_by(Species) %>% 
  summarize(max_shts_density = density(shts)$x[which.max(density(shts)$y)],
            mean_shts = mean(shts))

m_all_poscomplete <- m_all_poscomplete %>%
  left_join(more_shts_stats, by = "Species")

m_all_poscomplete |>
  ggplot(aes(x=shts)) +
  geom_density(aes(y=after_stat(density))) +
  # geom_vline(aes(xintercept = max_shts_density)) + 
  geom_vline(aes(xintercept = mean_shts)) +
  facet_wrap(~Species,nrow = 1) +
  theme_bw()



# Proportion of time spent across wind_vel_kmh
more_wind_vel_kmh_stats <- m_all_poscomplete %>% 
  group_by(Species) %>% 
  summarize(max_wind_vel_kmh_density = density(wind_vel_kmh)$x[which.max(density(wind_vel_kmh)$y)],
            mean_wind_vel_kmh = mean(wind_vel_kmh))

m_all_poscomplete <- m_all_poscomplete %>%
  left_join(more_wind_vel_kmh_stats, by = "Species")

m_all_poscomplete |>
  ggplot(aes(x=wind_vel_kmh)) +
  geom_density(aes(y=after_stat(density))) +
  # geom_vline(aes(xintercept = max_wind_vel_kmh_density)) + 
  geom_vline(aes(xintercept = mean_wind_vel_kmh)) +
  facet_wrap(~Species,nrow = 1) +
  theme_bw()



# 2D density plot using continuous data
ggplot(m_all_poscomplete, aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled() +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  # geom_point(data = m_all_poscomplete,aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=.1,color="white") + 
  facet_wrap(~Species,nrow = 1)

# 2D density plot using continuous data
density_BBAL <- ggplot(m_all_poscomplete %>% filter(Species=="Black-browed"), aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled(binwidth=.002) +
  xlim(0,100) +
  ylim(0,8) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

density_GHAL <- ggplot(m_all_poscomplete %>% filter(Species=="Grey-headed"), aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled(binwidth=.002) +
  xlim(0,100) +
  ylim(0,8) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

density_WAAL <- ggplot(m_all_poscomplete %>% filter(Species=="Wandering"), aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled(binwidth=.002) +
  xlim(0,100) +
  ylim(0,8) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

density_BFAL <- ggplot(m_all_poscomplete %>% filter(Species=="Black-footed"), aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled(binwidth=.002) +
  xlim(0,100) +
  ylim(0,8) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

density_LAAL <- ggplot(m_all_poscomplete %>% filter(Species=="Laysan"), aes(x=wind_vel_kmh, y=shts)) +
  geom_density_2d_filled(binwidth=.002) +
  xlim(0,100) +
  ylim(0,8) +
  scale_fill_manual(values=inferno(12),drop=FALSE) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

# Use the tag label as an x-axis label
wrap_elements(panel = density_BBAL | density_GHAL | density_WAAL | density_BFAL | density_LAAL) +
  labs(tag = "Windspeed (km/h)") +
  theme(
    plot.tag = element_text(size = 24),
    plot.tag.position = "bottom"
  )




################################################################################
# How does wind velocity compare to swell and wave height? ---------------------

# Total waves
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=swh),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# Swells
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# Wind waves
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=shww),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# Total waves vs. swell + ww:
# It looks like total waves isn't exactly the addition of swell and wind components.
# This is probably because of different periods and directions
ggplot(m_all_nona_flaps_env %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=shww+shts,y=swh),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()


# Use histograms to show that shts > shww
# Try to justify from the literature that waves need to be a certain height to be useful
# for windwave soar
# Period
hist(m_all$shts)

# VERY HIGH correlation between wind_vel and shww because wind cause wind waves.
# Pretty high correlation between wind_vel and swh because wind waves are a component of total waves.
# No/low correlation between wind_vel and shts because wind_vel doesn't directly effect swell.

wind_vel_kmh_box <- m_all_poscomplete |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_boxplot(width=0.4) +
  theme_bw()

swh_box <- m_all_poscomplete |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) + 
  theme_bw()

shts_box <- m_all_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) +
  theme_bw()

shww_box <- m_all_poscomplete |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) +
  theme_bw()

# Display all 4 figs
wrap_elements(panel = swh_box | shts_box | shww_box | wind_vel_kmh_box)


mwp_box <- m_all_poscomplete |>
  ggplot(aes(Species,mwp)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

mpts_box <- m_all_poscomplete |>
  ggplot(aes(Species,mpts)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

mpww_box <- m_all_poscomplete |>
  ggplot(aes(Species,mpww)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = mwp_box | mpts_box | mpww_box)




# Flapping rate is generally lower for NP species
flap_box_all_winds <- m_all %>% filter((HMM_3S_state != 1)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="All winds") + 
  theme_bw()
# At low windspeeds, NP birds are flapping less
flap_box_low_winds <- m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="Low winds") + 
  theme_bw()
# At high windspeeds, all birds are flapping at a similar rate
flap_box_high_winds <- m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="High winds") + 
  theme_bw()

# Display all 3 figs
wrap_elements(panel = flap_box_all_winds | flap_box_low_winds | flap_box_high_winds)


# At low windspeeds, the height of swells == total wave height and are 
# greater for NP spp
low_winds_swh <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
low_winds_shts <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
low_winds_shww <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = low_winds_swh | low_winds_shts | low_winds_shww)

# At high windspeeds, swh is less similar to shts and 
high_winds_swh <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
high_winds_shts <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
high_winds_shww <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = high_winds_swh | high_winds_shts | high_winds_shww)














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





# Flaps/hour vs swh (categorical) after removing HMM_3S_state == 1 ----------------------

GAM_list_total_waves_cat <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(swh,bird_wave_angle,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_total_waves_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, swh = evenly(swh, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bird_wave_angle = unique(m_current$bird_wave_angle))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(swh,bird_wave_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(swh,bird_wave_angle)"))[,4:7])
  
  colnames(link_df) <- c("swh","bird_wave_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    # ds_df_cat <- current_ds
    fv_df_total_waves_cat <- link_df
  } else {
    # ds_df_cat <- rbind(ds_df_cat,current_ds)
    fv_df_total_waves_cat <- rbind(fv_df_total_waves_cat,link_df)
  }
}

fv_df_total_waves_cat$Species <- factor(fv_df_total_waves_cat$Species,levels=c("Black-browed", 
                                                                               "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# Link: global for all app
cat_all <- fv_df_total_waves_cat |>
  ggplot(aes(swh,exp(fitted_global),color=bird_wave_angle)) +
  geom_line(linewidth=1.5) +
  # geom_line(aes(swh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wave_angle),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour",x="Windspeed (km/h)") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()



# Link: global for SO spp
cat_SO <- fv_df_total_waves_cat %>% filter(Species %in% c("Black-browed","Grey-headed","Wandering"))|>
  ggplot(aes(swh,exp(fitted_global),color=bird_wave_angle)) +
  geom_line(linewidth=1.5) +
  # geom_line(aes(swh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wave_angle),alpha=0.2) +
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
# fv_df_total_waves_cat %>% filter(Species %in% c("BFAL","LAAL"))|>
#   ggplot(aes(swh,exp(fitted_global),color=bird_wave_angle)) +
#   geom_line() +
#   # geom_line(aes(swh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
#   geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wave_angle),alpha=0.3) +
#   guides(color=guide_legend(title="Relative wind condition")) +
#   scale_color_manual(values=c("head" = "#e74c3c",
#                               "cross" = "#2980b9",
#                               "tail" = "#27ae60")) + 
#   labs(y="Flaps per hour",x="Wind velocity (m/s)") +
#   # xlim(0,25) + 
#   ylim(0,1500) +
#   facet_wrap(~Species,nrow = 1) + 
#   theme(text = element_text(size = 24))














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
