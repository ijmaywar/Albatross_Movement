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
library(RColorBrewer)

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


# Downsampling Bird_Island to Midway --------------------------------------

downsampled_ids <- c(sample(unique(m_BBAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_GHAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_WAAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_LAAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     unique(m_BFAL_nonaflaps$id))
ds_m_all_nonaflaps <- m_all_nonaflaps %>% filter(id %in% downsampled_ids)


# Flaps/hour vs wind_vel (continuous) after removing HMM_3S_state == 1
# WITHOUT A WIND TERM ON ITS OWN.

main_k <- 3
fac_k <- 3
GAM_list_cont <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- ds_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(bwa,k=fac_k,bs='tp') +
                       ti(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_cont[[spp]] <- current_GAM
  
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
    ds_df_cont_ds <- current_ds
    fv_df_cont_link_ds <- link_df
  } else {
    ds_df_cont_ds <- rbind(ds_df_cont_ds,current_ds)
    fv_df_cont_link_ds <- rbind(fv_df_cont_link_ds,link_df)
  }
}

fv_df_cont_link_ds$Species <- factor(fv_df_cont_link_ds$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(18)
# Add scatter plots of (wind_vel,bwa) data
ggplot(fv_df_cont_link_ds) +
  geom_contour_filled(aes(wind_vel,bwa,z=exp(fitted_global)),binwidth = 100) +
  scale_fill_manual(values=mycolors,drop=FALSE) +
  geom_hline(yintercept=45,linetype=2) +
  geom_hline(yintercept=135,linetype=2) +
  labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
  facet_wrap(~Species,nrow=1)

# Add scatter plots of (wind_vel,bwa) data
ggplot(fv_df_cont_link_ds) +
  geom_point(data = ds_m_all_nonaflaps %>% filter(HMM_3S_state!=1),aes(x=wind_vel,y=bwa),size=0.001,alpha=1) + 
  geom_contour_filled(aes(wind_vel,bwa,z=exp(fitted_global)),binwidth = 100,alpha=0.9) +
  scale_fill_manual(values=mycolors,drop=FALSE) +
  labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
  facet_wrap(~Species)

# Plot ti(wind_vel) with confidence intervals
ggplot(fv_df_cont_link_ds) +
  geom_ribbon(aes(x=wind_vel,ymin=lower_windvel,ymax=upper_windvel,y=NULL),alpha=0.3,color='yellow') +
  geom_line(aes(wind_vel,fitted_windvel)) +
  labs(fill = "Flaps/hour", x="Wind Velocity (m/s)", y="GAM effect") +
  ylim(-3,1) +
  facet_wrap(~Species)

# Plot ti(bwa) with confidence intervals
ggplot(fv_df_cont_link_ds) +
  geom_ribbon(aes(x=bwa,ymin=lower_bwa,ymax=upper_bwa,y=NULL),alpha=0.3,color='yellow') +
  geom_line(aes(bwa,fitted_bwa)) +
  labs(fill = "Flaps/hour", x="Bird-wind angle (degrees)", y="GAM effect") +
  ylim(-0.5,0.5) +
  facet_wrap(~Species)

# Flaps/hour vs wind_vel (categorical) after removing HMM_3S_state == 1 ----------------------

GAM_categorical_list_ds <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- ds_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel,BWA_cat,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_categorical_list_ds[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            BWA_cat = unique(m_current$BWA_cat))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel,BWA_cat)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel,BWA_cat)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(wind_vel,BWA_cat)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel","id","BWA_cat","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_fs","se_fs","lower_fs","upper_fs",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_cat_ds <- current_ds
    fv_df_cat_link_ds <- link_df
  } else {
    ds_df_cat_ds <- rbind(ds_df_cat,current_ds)
    fv_df_cat_link_ds <- rbind(fv_df_cat_link_ds,link_df)
  }
}

fv_df_cat_link_ds$Species <- factor(fv_df_cat_link_ds$Species,levels=c("BBAL","GHAL","WAAL","BFAL","LAAL"))

# Link: Wind + Intercept
fv_df_cat_link_ds |>
  ggplot(aes(wind_vel,exp(fitted_global),color=BWA_cat)) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=BWA_cat),alpha=0.3) +
  labs(y="Flaps per hour",x="Wind velocity (m/s)") +
  # xlim(0,25) + 
  ylim(0,2000) +
  facet_wrap(~Species,nrow = 1)




# Upsampling Midway to Bird_Island  --------------------------------------------

upsampled_rows <- c(sample(which(m_all_nonaflaps$Species=="BBAL"),
                            size=nrow(m_GHAL_nonaflaps),
                            replace=TRUE),
                    which(m_all_nonaflaps$Species=="GHAL"),
                    sample(which(m_all_nonaflaps$Species=="WAAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE),
                    sample(which(m_all_nonaflaps$Species=="BFAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE),
                    sample(which(m_all_nonaflaps$Species=="LAAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE))
us_m_all_nonaflaps <- m_all_nonaflaps[upsampled_rows,]


# Flaps/hour vs wind_vel (continuous) after removing HMM_3S_state == 1
# WITHOUT A WIND TERM ON ITS OWN.

main_k <- 3
fac_k <- 3
GAM_list_cont_us <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- us_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_cont_us[[spp]] <- current_GAM
  
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
    ds_df_cont_us <- current_ds
    fv_df_cont_link_us <- link_df
  } else {
    ds_df_cont_us <- rbind(ds_df_cont_us,current_ds)
    fv_df_cont_link_us <- rbind(fv_df_cont_link_us,link_df)
  }
}

fv_df_cont_link_us$Species <- factor(fv_df_cont_link$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Link
fv_df_cont_link_us |>
  ggplot(aes(wind_vel,bwa,z=exp(fitted_global))) +
  geom_contour_filled(breaks=seq(0,2000,by=100)) +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1000) +
  facet_wrap(~Species)

# Link: Wind + Intercept
fv_df_cont_link_us |>
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
fv_df_cont_link_us |>
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


