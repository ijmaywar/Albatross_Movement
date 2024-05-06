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
m_all <- m_all %>% mutate(BWA_cat = case_when(bwa<=45 ~ "tail",
                                              bwa>45 & bwa<135 ~ "cross",
                                              bwa>=135 ~ "head"))

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$Field_Season <- as.factor(m_all$Field_Season)
m_all$Location <- as.factor(m_all$Location)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$Species <- as.factor(m_all$Species)
m_all$BWA_cat <- as.factor(m_all$BWA_cat)

# Re-order Species groups
m_all$Species <- factor(m_all$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Split data between species
m_BBAL <- m_all %>% filter(Species=="BBAL")
m_GHAL <- m_all %>% filter(Species=="GHAL")
m_WAAL <- m_all %>% filter(Species=="WAAL")
m_LAAL <- m_all %>% filter(Species=="LAAL")
m_BFAL <- m_all %>% filter(Species=="BFAL")

# df without the flaps==NA rows:
m_all_nonaflaps <- m_all %>% drop_na(flaps)
m_BBAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BBAL")
m_GHAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="GHAL")
m_WAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="WAAL")
m_BFAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BFAL")
m_LAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="LAAL")

# Sample stats -----------------------------------------------------------------

nrow(m_all)
m_all %>% count(Species)
m_all %>% group_by(Species, BWA_cat) %>% summarize(count=n())

# Filtering based on HMM state
# In the 2-state model, state 1 is foraging/on-water and state 2 is commuting
# In the 3-state model, state 1 is on-water, state 2 is foraging, and state 3 is commuting

# 2-state HMM
m_HMM_2S <- m_all %>% filter(HMM_2S_state==2)
m_HMM_2S %>% count(Species)
m_HMM_2S %>% group_by(Species, BWA_cat) %>% summarize(count=n())

# 3-state HMM selecting for just commuting
m_HMM_3S_commuting <- m_all %>% filter(HMM_3S_state==3)
m_HMM_3S_commuting %>% count(Species)
m_HMM_3S_commuting %>% group_by(Species, BWA_cat) %>% summarize(count=n())

# 3-state HMM selecting for commuting and foraging
m_HMM_3S_cf <- m_all %>% filter(HMM_3S_state==3 | HMM_3S_state==2)
m_HMM_3S_cf %>% count(Species)
m_HMM_3S_cf %>% group_by(Species, BWA_cat) %>% summarize(count=n())

# Filtering based on GLS state
# Birds that have GLS tags will be filtered by GLS state while birds that do 
# not have GLS tags will be filtered using an HMM
m_GLS_dry <- m_all %>% filter(GLS_state=='dry')
m_GLS_dry %>% count(Species)
m_GLS_dry %>% group_by(Species, BWA_cat) %>% summarize(count=n())

m_GLS_wet <- m_all %>% filter(GLS_state=='wet')

# stats on the performance of HMM in relation to GLS
m_all %>% group_by(GLS_state,HMM_2S_state) %>% summarize(count=n())
m_all %>% group_by(GLS_state,HMM_3S_state) %>% summarize(count=n())

# stats on the performance of OWB in relation to GLS
m_all %>% group_by(GLS_state,OWB_state) %>% summarize(count=n())

# Flaps/hour vs wind_vel (continuous) after removing GLS_State == "wet" ----------------------
# WITHOUT A WIND TERM ON ITS OWN.

main_k <- 3
fac_k <- 3
GAM_list_GLS <- list()

for (spp in c("BBAL", "GHAL", "WAAL")) {
  
  m_current <- m_all %>% filter((GLS_state =="dry") & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_GLS[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(wind_vel)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel","id","bwa","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_windvel","se_windvel","lower_windvel","upper_windvel",
                         "fitted_bwa","se_bwa","lower_bwa","upper_bwa",
                         "fitted_intrxn","se_intrxn","lower_intrxn","upper_intrxn",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_GLS <- current_ds
    fv_df_GLS <- link_df
  } else {
    ds_df_GLS <- rbind(ds_df_GLS,current_ds)
    fv_df_GLS <- rbind(fv_df_GLS,link_df)
  }
}

fv_df_GLS$Species <- factor(fv_df_GLS$Species , levels=c("BBAL", "GHAL", "WAAL"))

mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(14)
# Contour
fv_df_GLS |>
  ggplot(aes(wind_vel,bwa,z=exp(fitted_global))) +
  geom_contour_filled(binwidth = 100) +
  scale_fill_manual(values=mycolors) +
  labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
  facet_wrap(~Species)

fv_df_cont_link |>
  ggplot(aes(wind_vel,exp(fitted_int+fitted_windvel))) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_int+lower_windvel),ymax=exp(upper_int+upper_windvel),y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="Flaps/hour",x="Wind velocity (m/s)")

# Link: Wind + Intercept
fv_df_cont_link |>
  ggplot(aes(wind_vel,fitted_windvel)) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=lower_windvel,ymax=upper_windvel,y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="GAM wind effect",x="Wind velocity (m/s)")

fv_df_cont_link |>
  ggplot(aes(Species,fitted_id)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="GAM wind effect",x="Wind velocity (m/s)")

m_all %>% filter(id=="LAAL_20190202_24") |>
  ggplot(aes(wind_vel,flaps)) +
  geom_point()
