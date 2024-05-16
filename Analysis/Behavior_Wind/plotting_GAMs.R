################################################################################
#
# Wind data: create plots with wind data.
# This will need to be edited in order to account for times removed by on-water
# and foraging/resting (HMM)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

locations = c('Bird_Island','Midway')
min_peak_prob = 0 # What was the min_peak_prob used to create summary data?

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

if (min_peak_prob == 0) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_0/")
} else if (min_peak_prob == 0.85) {
  read_dir <- paste0(GD_dir, "Analysis/Maywar/Flaps_Hourly/Flaps_HMM_GLS_ECG_Compiled/p_085/")
}

fullmeta <- read_excel(paste0(GD_dir,"metadata/Full_Metadata.xlsx"))

setwd(read_dir)
files <- list.files(pattern = '*.csv')
m_all <- read_csv(files[1])

# Classify 2BEP, E_pip, Ep as BG
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","BG")))

# Datetime stuff
m_all$datetime <- as.POSIXlt(m_all$datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT")

# Categorize BWAs
m_all <- m_all %>% mutate(BWA_cat = case_when(bwa<60 ~ "tail",
                                              bwa>=60 & bwa<120 ~ "cross",
                                              bwa>=120 ~ "head"))

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$Field_Season <- as.factor(m_all$Field_Season)
m_all$Location <- as.factor(m_all$Location)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$Species <- as.factor(m_all$Species)
m_all$BWA_cat <- as.factor(m_all$BWA_cat)

# Re-order Species groups and give them their full name
m_all <- m_all %>% mutate(Species = factor(replace(as.character(Species),Species=="BBAL","Black-browed")),
                          Species = factor(replace(as.character(Species),Species=="GHAL","Grey-headed")),
                          Species = factor(replace(as.character(Species),Species=="WAAL","Wandering")),
                          Species = factor(replace(as.character(Species),Species=="BFAL","Black-footed")),
                          Species = factor(replace(as.character(Species),Species=="LAAL","Laysan")))
m_all$Species <- factor(m_all$Species, levels=c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan"))

m_all$BWA_cat <- factor(m_all$BWA_cat, levels=c("head", "cross", "tail"))

# Add km/hr of wind_vel
m_all$wind_vel_kmh <- 3.6*(m_all$wind_vel)

# df without the flaps==NA rows:
m_all_nonaflaps <- m_all %>% drop_na(flaps)
m_all_nonaflapsbwas <- m_all_nonaflaps %>% drop_na(bwa)

# Sample stats -----------------------------------------------------------------

nrow(m_all)
m_all_nonaflaps %>% count(Species)
m_all_nonaflaps %>% group_by(Species) %>% summarize(unique_IDs=n_distinct(id))

# stats on the performance of HMM in relation to GLS
m_all %>% group_by(GLS_state,HMM_2S_state) %>% summarize(count=n())
m_all %>% group_by(GLS_state,HMM_3S_state) %>% summarize(count=n())

# stats on the performance of OWB in relation to GLS
m_all %>% group_by(GLS_state,OWB_state) %>% summarize(count=n())


# Flaps/hour vs wind_vel_kmh (continuous) after removing HMM_3S_state == 1 ----------------------
# WITHOUT A WIND TERM ON ITS OWN.

fac_k <- 3
GAM_list <- list()

for (spp in c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")) {
  
  m_current <- m_all_nonaflapsbwas %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))

  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bwa)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("te(wind_vel_kmh,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel_kmh","id","bwa","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_intrxn","se_intrxn","lower_intrxn","upper_intrxn",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "Black-browed") {
    ds_df_cont <- current_ds
    fv_df_cont <- link_df
  } else {
    ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_cont <- rbind(fv_df_cont,link_df)
  }
}

fv_df_cont$Species <- factor(fv_df_cont$Species , levels=c("Black-browed", 
                      "Grey-headed", "Wandering", "Black-footed", "Laysan"))

# fv_df_cont <- fv_df_cont %>%  mutate(Species = factor(replace(as.character(Species),Species=="BBAL","Black-browed")),
#                                    Species = factor(replace(as.character(Species),Species=="GHAL","Grey-headed")),
#                                    Species = factor(replace(as.character(Species),Species=="WAAL","Wandering")),
#                                    Species = factor(replace(as.character(Species),Species=="BFAL","Black-footed")),
#                                    Species = factor(replace(as.character(Species),Species=="LAAL","Laysan")))

# mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(13)

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
