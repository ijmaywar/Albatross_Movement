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
# library(gamm4)
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
# m_all$julian <- m_all$datetime$yday + 1 
# # plotday are the days since the beginning of the year (January 1 of the first
# # year is 1)
# m_all$plotday <- ifelse(m_all$julian > 200, m_all$julian, m_all$julian + 365)
# # adjust for leap years
# m_all$plotday <- ifelse(m_all$plotday > 365 & m_all$datetime$year+1900 == 2021, m_all$plotday+1, m_all$plotday)

# Remove unnecessary columns
# m_all <- m_all %>% dplyr::select(-lon,-lat,-u,-v,-datetime,-wind_dir,-bird_dir,-bird_vel)

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

# How many samples per Species per BWA_cat? ----------------------------------------

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


# Flap plots --------------------------------------------------------------

main_k <- 3
fac_k <- 3

m_BBAL <- filter(m_GLS_dry %>% Species=="BBAL")
m_GHAL <- filter(m_GLS_dry %>% Species=="GHAL")
m_BBAL <- filter(m_GLS_dry %>% Species=="BBAL")

GAM_BBAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,BWA_cat,bs='fs',k=fac_k,m=2) + 
                              s(id,k=length(unique(m_BBAL$id)),bs="re"),
                            data = m_BBAL,
                            family = "poisson",
                            method = "REML")
m_all |>
  ggplot(aes(wind_vel,flaps)) +
  geom_point() +
  geom_point(m_GLS_wet,mapping=aes(wind_vel,flaps),color='red',alpha=1)

m_GLS_dry |>
  ggplot(aes(wind_vel,flaps)) +
  geom_point() +
  geom_point(m_GLS_wet,mapping=aes(wind_vel,flaps),color='red',alpha=.3)

m_GLS_wet |>
  ggplot(aes(wind_vel,flaps)) +
  geom_point() +
  geom_point(m_GLS_dry,mapping=aes(wind_vel,flaps),color='red',alpha=.3)


# Create GAMs for "usable" and "unusable" data according to the GLS...
GAM_GLS_list <- list()
for (spp in levels(m_GLS_dry$Species)) {
  GAM_GLS_state_list <- list()
  for (GLS_cat in c("dry","wet","all")) {
    m_current <- m_all %>% filter(Species==spp)
    if (GLS_cat=="dry" | GLS_cat=="wet") {
      m_current <- m_current %>% filter(GLS_state==GLS_cat)
    } else if (GLS_cat != "all") {
      print("invalid GLS_cat.")
      break
    }
    current_GAM_GLS <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=3,m=2) +
                       s(wind_vel,BWA_cat,bs='fs',k=3,m=2) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
    current_ds_GLS  <- data_slice(current_GAM_GLS, wind_vel = evenly(wind_vel, n = 100), 
                              id = unique(m_current$id)[1:10],
                              BWA_cat = unique(m_current$BWA_cat))
    
    current_fv_GLS <- fitted_values(current_GAM_GLS, data = current_ds_GLS, scale = "response",
                                terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)","s(id)"))
    
    current_fv_GLS_global <- fitted_values(current_GAM_GLS, data = current_ds_GLS, scale = "response",
                                       terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)"))
    
    # Add metadata for the current species and GLS category
    current_ds_GLS <- cbind(current_ds_GLS,data.frame(Species = rep(spp,nrow(current_ds_GLS)),
                                                      GLS_state = rep(GLS_cat,nrow(current_ds_GLS))))
    current_fv_GLS <- cbind(current_fv_GLS,data.frame(Species = rep(spp,nrow(current_fv_GLS)),
                                                      GLS_state = rep(GLS_cat,nrow(current_fv_GLS))))
    current_fv_GLS_global <- cbind(current_fv_GLS_global,data.frame(Species = rep(spp,nrow(current_fv_GLS_global)),
                                                      GLS_state = rep(GLS_cat,nrow(current_fv_GLS_global))))
    
    GAM_GLS_state_list[[GLS_cat]] <- current_GAM_GLS
    if (spp == "BBAL" & GLS_cat == "dry") {
      ds_GLS_df <- current_ds_GLS
      fv_GLS_df <- current_fv_GLS
      fv_GLS_global_df <- current_fv_GLS_global
    } else {
      ds_GLS_df <- rbind(ds_GLS_df,current_ds_GLS)
      fv_GLS_df <- rbind(fv_GLS_df,current_fv_GLS)
      fv_GLS_global_df <- rbind(fv_GLS_global_df,current_fv_GLS_global)
    }
  }
  GAM_GLS_list[[spp]] <- GAM_GLS_state_list
}


# Instead of putting ds and fv into lists, put them into a df with columns describing
# the spp and state. I can then use facet_wrap() when plotting.

# Plot the GAMs alongside eachother - A similar results would be strange...
# Could show that because these are hour-long summaries, on-water behavior is not really important
# Could also show that the GLSs are not providing the correct output for some reason.

fv_GLS_global_df %>% filter(Species=="BBAL") |>
  ggplot(aes(wind_vel,fitted,color=BWA_cat)) +
  geom_line() +
  geom_ribbon(mapping=aes(ymin = lower, ymax = upper, y = NULL, color = BWA_cat),alpha = 0.3) +
  labs(title="BBAL") +
  xlim(0,25) + 
  ylim(0,1000) +
  facet_wrap(~GLS_state)

# It looks like the shape of the GAMs don't change too much when accounting for GLS state...

fv_GLS_wet_global |>
  ggplot(aes(wind_vel,fitted,color=BWA_cat)) +
  geom_line() +
  geom_ribbon(mapping=aes(ymin = lower, ymax = upper, y = NULL, color = BWA_cat),alpha = 0.3) +
  geom_point(m_GLS_wet,mapping=aes(wind_vel,flaps),alpha=0.1,color='black') +
  labs(title="GLS Wet") +
  xlim(0,25) + 
  ylim(0,1000)

fv_GLS_dry_global |>
  ggplot(aes(wind_vel,fitted,color=BWA_cat)) +
  geom_line() +
  geom_ribbon(mapping=aes(ymin = lower, ymax = upper, y = NULL, color = BWA_cat),alpha = 0.3) +
  geom_point(m_GLS_dry,mapping=aes(wind_vel,flaps),alpha=0.1) + 
  labs(title="GLS Dry") + 
  ylim(0,7000)



fv_GLS_wet_global |>
  ggplot(aes(wind_vel,fitted)) +
  geom_line(linewidth=1,color='red') +
  geom_ribbon(mapping=aes(ymin = lower, ymax = upper, y = NULL),alpha = 0.3,color='red') +
  geom_point(m_GLS_wet,mapping=aes(wind_vel,flaps),alpha=0.1) + 
  labs(title="GLS Wet") + 
  ylim(0,7000)





# Tensor product of wind_vel and bwa

GAM_BBAL <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') +
                                  s(bwa,k=5,bs='tp') +
                                  ti(wind_vel,bwa,k=c(5,5),bs=c('tp','tp')) +
                    s(id,k=length(unique(m_BBAL$id)),bs="re"),
                  data = m_BBAL,
                  family = "poisson",
                  method = "REML")

plot(GAM_BBAL,scheme=2)

ds  <- data_slice(GAM_BBAL, wind_vel = evenly(wind_vel, n = 100), 
                  id = unique(m_BBAL$id)[1:10],
                  bwa = evenly(bwa,n=100))

# fv <- fitted_values(GAM_BBAL, data = ds, scale = "response",
#                          terms = c("(Intercept)","te(wind_vel,bwa)","s(id)"))
# 
# fv_global <- fitted_values(GAM_BBAL, data = ds, scale = "response",
#                                 terms = c("(Intercept)","te(wind_vel,bwa)"))


fv <- fitted_values(GAM_BBAL, data = ds, scale = "response",
                    terms = c("(Intercept)","s(wind_vel)","s(bwa)","ti(wind_vel,bwa)","s(id)"))

fv_global <- fitted_values(GAM_BBAL, data = ds, scale = "response",
                           terms = c("(Intercept)","s(wind_vel)","s(bwa)","ti(wind_vel,bwa)"))



fv %>% filter(id==unique(m_BBAL$id)[1])|>
  ggplot(aes(wind_vel,bwa, z=fitted)) +
  geom_contour_filled() +
  labs(title="individual 1")

fv %>% filter(id==unique(m_BBAL$id)[2])|>
  ggplot(aes(wind_vel,bwa, z=fitted)) +
  geom_contour_filled() +
  labs(title="individual 2")

fv %>% filter(id==unique(m_BBAL$id)[3])|>
  ggplot(aes(wind_vel,bwa, z=fitted)) +
  geom_contour_filled() +
  labs(title="individual 3")

fv_global |>
  ggplot(aes(wind_vel,bwa, z=fitted)) +
  geom_contour_filled() + 
  labs(title="global")

################################################################################

# Remove outliers --------------------------------------------------------------

# Look at domains of different IDs
for (fig_i in 1:length(unique(m_BBAL$id))) {
  print(m_BBAL |>
    ggplot(aes(wind_vel,flaps)) +
    geom_point(color='black') +
    geom_point(m_BBAL %>% filter(id==unique(m_BBAL$id)[fig_i]),mapping=aes(x=wind_vel,y=flaps),color='red') +
    labs(title=fig_i))
  
  readline(prompt="Press [enter] to continue")
}

# THIS IS A HUGE OUTLIER, so remove it!         
m_BBAL <- m_BBAL %>% filter(id != "BBAL_20200114_RF18")

# Interaction term for categorized bwa -----------------------------------------

# Random wiggly curves:
# Produces random smooth curves for each level of BWA_cat
# Main effect (wind_vel) plus factor level smooth deviations from that effect.

main_k <- 3
fac_k <- 3

GAM_BBAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                                      s(wind_vel,BWA_cat,bs='fs',k=fac_k,m=2) + 
                                      s(id,k=length(unique(m_BBAL$id)),bs="re"),
                            data = m_BBAL,
                            family = "poisson",
                            method = "REML")

# Figure out the domains of the ids

GAM_BBAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,id,bs='fs',k=length(unique(m_BBAL$id)),m=1),
                            data = m_BBAL,
                            family = "poisson",
                            method = "REML")

GAM_BBAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,id,bs='fs',k=length(unique(m_BBAL$id)),m=1),
                            data = m_BBAL,
                            family = "poisson",
                            method = "REML")


# subset the data and change to bs="fs"
# the 

GAM_GHAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,BWA_cat,bs='fs',k=fac_k,m=2) + 
                              s(id,k=length(unique(m_GHAL$id)),bs="re"),
                            data = m_GHAL,
                            family = "poisson",
                            method = "REML")

GAM_BFAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,BWA_cat,bs='fs',k=fac_k,m=2) + 
                              s(id,k=length(unique(m_BFAL$id)),bs="re"),
                            data = m_BFAL,
                            family = "poisson",
                            method = "REML")

GAM_LAAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                              s(wind_vel,BWA_cat,bs='fs',k=fac_k,m=2) + 
                              s(id,k=length(unique(m_LAAL$id)),bs="re"),
                            data = m_LAAL,
                            family = "poisson",
                            method = "REML")


# Group-level smooths for BWA_cat:
# (Doesn't allow for major deviations from the main effect)
# GAM_BBAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=3) +
#                               s(wind_vel,by=BWA_cat,k=3,m=1) + 
#                               s(id,k=length(unique(m_BBAL$id)),bs="re"),
#                             data = m_BBAL,
#                             family = "poisson",
#                             method = "REML")

ds_directional  <- rbind(data_slice(GAM_BBAL_directional, wind_vel = evenly(wind_vel, n = 100), id = unique(m_BBAL$id),
                              BWA_cat = unique(m_BBAL$BWA_cat)),
                         data_slice(GAM_GHAL_directional, wind_vel = evenly(wind_vel, n = 100), id = unique(m_GHAL$id),
                                    BWA_cat = unique(m_GHAL$BWA_cat)),
                         data_slice(GAM_BFAL_directional, wind_vel = evenly(wind_vel, n = 100), id = unique(m_BFAL$id),
                                    BWA_cat = unique(m_BFAL$BWA_cat)),
                         data_slice(GAM_LAAL_directional, wind_vel = evenly(wind_vel, n = 100), id = unique(m_LAAL$id),
                                    BWA_cat = unique(m_LAAL$BWA_cat)))

ds_directional$BWA_cat <- factor(ds_directional$BWA_cat, levels=c("head", "cross", "tail"))

fv_directional <- rbind(fitted_values(GAM_BBAL_directional, data = ds_directional %>% filter(str_detect(id,"BBAL")), scale = "response"),
                        fitted_values(GAM_GHAL_directional, data = ds_directional %>% filter(str_detect(id,"GHAL")), scale = "response"),
                        fitted_values(GAM_BFAL_directional, data = ds_directional %>% filter(str_detect(id,"BFAL")), scale = "response"),
                        fitted_values(GAM_LAAL_directional, data = ds_directional %>% filter(str_detect(id,"LAAL")), scale = "response"))
fv_directional <- fv_directional %>% mutate(Species = substr(id,1,4))
fv_directional$Species <- factor(fv_directional$Species, levels=c("BBAL","GHAL","BFAL","LAAL"))

fv_directional_global <- rbind(fitted_values(GAM_BBAL_directional, data = ds_directional %>% filter(str_detect(id,"BBAL")), scale = "response",
                                       terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)")),
                               fitted_values(GAM_GHAL_directional, data = ds_directional %>% filter(str_detect(id,"GHAL")), scale = "response",
                                             terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)")),
                               fitted_values(GAM_BFAL_directional, data = ds_directional %>% filter(str_detect(id,"BFAL")), scale = "response",
                                             terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)")),
                               fitted_values(GAM_LAAL_directional, data = ds_directional %>% filter(str_detect(id,"LAAL")), scale = "response",
                                             terms = c("(Intercept)","s(wind_vel)","s(wind_vel,BWA_cat)")))
fv_directional_global <- fv_directional_global %>% mutate(Species = substr(id,1,4))
fv_directional_global$Species <- factor(fv_directional_global$Species, levels=c("BBAL","GHAL","BFAL","LAAL"))
                               

fv_directional %>% filter(Species=="BBAL") |>
  ggplot(aes(x = wind_vel, y = fitted, color=id)) +
  geom_line() +
  # geom_point(m_BBAL,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_line(fv_directional_global %>% filter(Species=="BBAL"),mapping=aes(wind_vel,fitted),color='black',linewidth=1) +
  geom_ribbon(fv_directional_global %>% filter(Species=="BBAL"),mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.1,fill='black') +
  facet_wrap(~BWA_cat,ncol=3) +
  theme(legend.position="none")

fv_directional_global |>
  ggplot(aes(x = wind_vel, y = fitted, color=BWA_cat)) +
  geom_line(linewidth=1) +
  geom_ribbon(fv_directional_global,mapping=aes(ymin = lower, ymax = upper, y = NULL,fill=BWA_cat),alpha = 0.3,color=NA) +
  labs(title="Global for three directions") +
  facet_wrap(~Species,ncol=4)

  ################################################################################
# GHAL

# interaction between factor and continuous variable
GAM_GHAL_directional <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=3) +
                              s(wind_vel,by=BWA_cat,k=3,m=1) + 
                              s(id,k=length(unique(m_GHAL$id)),bs="re"),
                            data = m_GHAL,
                            family = "poisson",
                            method = "REML")

ds_directional  <- data_slice(GAM_GHAL_directional, wind_vel = evenly(wind_vel, n = 100), id = unique(m_GHAL$id)[1:10],
                              BWA_cat = unique(m_GHAL$BWA_cat))

ds_directional$BWA_cat <- factor(ds_directional$BWA_cat, levels=c("head", "cross", "tail"))

fv_directional <- fitted_values(GAM_GHAL_directional, data = ds_directional, scale = "response")

fv_directional_global <- fitted_values(GAM_GHAL_directional, data = ds_directional, scale = "response",
                                       terms = c("(Intercept)","s(wind_vel)","s(wind_vel):BWA_catcross",
                                                 "s(wind_vel):BWA_cathead","s(wind_vel):BWA_cattail"))

fv_directional |>
  ggplot(aes(x = wind_vel, y = fitted, color=id)) +
  geom_line() +
  geom_point(m_GHAL,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_line(fv_directional_global,mapping=aes(wind_vel,fitted),color='black',linewidth=1) +
  geom_ribbon(fv_directional_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.1,fill='black') +
  facet_wrap(~BWA_cat,ncol=3)

fv_directional_global |>
  ggplot(aes(x = wind_vel, y = fitted, color=BWA_cat)) +
  geom_line(linewidth=1) +
  geom_ribbon(fv_directional_global,mapping=aes(ymin = lower, ymax = upper, y = NULL,fill=BWA_cat),alpha = 0.3,color=NA) +
  labs(title="Global for three directions")


################################################################################
# Breaking up data into tail, cross, and tail-winds

# RUN tail, CROSS, TAIL SPLIT ---------------------------------------------
# Command, option, T to run this section

directional_k <- 3

m_BBAL_head <- m_BBAL %>% filter(bwa<=45) # only head-winds
m_BBAL_cross <- m_BBAL %>% filter(bwa>45 & bwa<135) # only cross-winds
m_BBAL_tail <- m_BBAL %>% filter(bwa>=135) # only tail-winds

# base model with individual randomness for headWINDS
GAM_BBAL_head <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=directional_k) +
                     s(id,k=length(unique(m_BBAL_head$id)),bs="re"),
                   data = m_BBAL_head,
                   family = "poisson",
                   method = "REML")

ds_head  <- data_slice(GAM_BBAL_head, wind_vel = evenly(wind_vel, n = 100), id = unique(m_BBAL_head$id)[1:10])

fv_head <- fitted_values(GAM_BBAL_head, data = ds_head, scale = "response",
                       terms = c("(Intercept)","s(wind_vel)","s(id)"))

fv_head_global <- fitted_values(GAM_BBAL_head, data = ds_head, scale = "response",
                              terms = c("(Intercept)","s(wind_vel)"))

fv_head |>
  ggplot(aes(x = wind_vel, y = fitted, color = id)) +
  geom_line() +
  geom_point(m_BBAL_head,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_line(fv_head_global,mapping=aes(wind_vel,fitted),color='black') +
  geom_ribbon(fv_head_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.1) +
  labs(title="headwinds")

fv_head_global |>
  ggplot(aes(x = wind_vel, y = fitted)) +
  geom_line(color='black') +
  geom_ribbon(fv_head_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.4) +
  labs(title="headwinds")

summary(GAM_BBAL_head)


# base model with individual randomness for crossWINDS
GAM_BBAL_cross <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=directional_k) +
                       s(id,k=length(unique(m_BBAL_cross$id)),bs="re"),
                     data = m_BBAL_cross,
                     family = "poisson",
                     method = "REML")

ds_cross  <- data_slice(GAM_BBAL_cross, wind_vel = evenly(wind_vel, n = 100), id = unique(m_BBAL_cross$id)[1:10])

fv_cross <- fitted_values(GAM_BBAL_cross, data = ds_cross, scale = "response",
                         terms = c("(Intercept)","s(wind_vel)","s(id)"))

fv_cross_global <- fitted_values(GAM_BBAL_cross, data = ds_cross, scale = "response",
                                terms = c("(Intercept)","s(wind_vel)"))

fv_cross |>
  ggplot(aes(x = wind_vel, y = fitted, color = id)) +
  geom_line() +
  geom_point(m_BBAL_cross,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_line(fv_cross_global,mapping=aes(wind_vel,fitted),color='black') +
  geom_ribbon(fv_cross_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.1) +
  labs(title="crosswinds")



# base model with individual randomness for tailWINDS
GAM_BBAL_tail <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=directional_k) +
                       s(id,k=length(unique(m_BBAL_tail$id)),bs="re"),
                     data = m_BBAL_tail,
                     family = "poisson",
                     method = "REML")

ds_tail  <- data_slice(GAM_BBAL_tail, wind_vel = evenly(wind_vel, n = 100), id = unique(m_BBAL_tail$id)[1:10])

fv_tail <- fitted_values(GAM_BBAL_tail, data = ds_tail, scale = "response",
                         terms = c("(Intercept)","s(wind_vel)","s(id)"))

fv_tail_global <- fitted_values(GAM_BBAL_tail, data = ds_tail, scale = "response",
                                terms = c("(Intercept)","s(wind_vel)"))

fv_tail |>
  ggplot(aes(x = wind_vel, y = fitted, color = id)) +
  geom_line() +
  geom_point(m_BBAL_tail,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_line(fv_tail_global,mapping=aes(wind_vel,fitted),color='black') +
  geom_ribbon(fv_tail_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.1) +
  labs(title="tailwinds")



# Plots with all 3 directions

fv_head_global |>
  ggplot(aes(x = wind_vel, y = fitted)) +
  geom_ribbon()
  geom_ribbon(fv_cross_global,mapping=aes(ymin = lower, ymax = upper, y = NULL,fill=BWA_cat),alpha = 0.3,color=NA) +
  geom_ribbon(fv_tail_global,mapping=aes(ymin = lower, ymax = upper, y = NULL,fill=BWA_cat),alpha = 0.3,color=NA) +
  labs(title="Global for three directions")


ggplot(m_BBAL, aes(wind_vel,flaps)) + 
  geom_point(alpha=0.1) +
  geom_line(fv_head_global,mapping=aes(wind_vel,fitted),color='green',linewidth=1) +
  geom_line(fv_cross_global,mapping=aes(wind_vel,fitted),color='blue',linewidth=1) +
  geom_line(fv_tail_global,mapping=aes(wind_vel,fitted),color='red',linewidth=1) +
  labs(title="Global smooths for the three directions") +
  ylim(0,1000)

# Plots with all 3 directions
ggplot(fv_head_global, aes(wind_vel,fitted)) + 
  geom_line(color='green',linewidth=1) +
  geom_ribbon(fv_head_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.2, fill='green') +
  geom_line(fv_cross_global,mapping=aes(wind_vel,fitted),color='blue',linewidth=1) +
  geom_ribbon(fv_cross_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.2, fill='blue') +
  geom_line(fv_tail_global,mapping=aes(wind_vel,fitted),color='red',linewidth=1) +
  geom_ribbon(fv_tail_global,mapping=aes(ymin = lower, ymax = upper, y = NULL), alpha = 0.2, fill='red') +
  labs(title="Global smooths for the three directions. k = 5")



################################################################################
# Try without removing landings -----------------------------------------------


################################################################################
# GHAL --------------------------------------------------------------------





################################################################################
# For BFAL
fiveP_BFAL <- quantile(m_BFAL$wind_vel,probs=(c(.05,.95)))
m_BFAL_filtered <- m_BFAL
m_BFAL_filtered <- m_BFAL %>% filter(wind_vel>fiveP_BFAL[[1]] & wind_vel<fiveP_BFAL[[2]])
# m_BFAL_filtered <- m_BFAL_filtered %>% filter(bwa<=45) # only tail-winds
# m_BFAL_filtered <- m_BFAL_filtered %>% filter(bwa>45 & bwa<135) # only cross-winds
# m_BFAL_filtered <- m_BFAL_filtered %>% filter(bwa>=135) # only tail-winds
ggplot(m_BFAL_filtered, aes(wind_vel,flaps)) + 
  geom_point(color='black')

GAM_BFAL <- gam(formula = flaps ~ te(wind_vel, bwa, k = c(5, 5), bs = c('tp', 'tp')),
                data = m_BFAL_filtered,
                family = poisson(),
                method = "REML")

preds <- predict.gam(GAM_BFAL,m_BFAL_filtered %>% select(wind_vel,bwa))
preds_df <- data.frame(cbind(m_BFAL_filtered,preds))
preds_df <- preds_df %>% mutate(est_trans = exp(preds))

ggplot(m_BFAL_filtered, aes(wind_vel,flaps)) +
  geom_point(color='red') +
  geom_point(data=preds_df, aes(wind_vel,est_trans), color="black")

summary(GAM_BFAL)

################################################################################
# For LAAL
fiveP_LAAL <- quantile(m_LAAL$wind_vel,probs=(c(.05,.95)))
m_LAAL_filtered <- m_LAAL
m_LAAL_filtered <- m_LAAL %>% filter(wind_vel>fiveP_LAAL[[1]] & wind_vel<fiveP_LAAL[[2]])
# m_LAAL_filtered <- m_LAAL_filtered %>% filter(bwa<=45) # only tail-winds
# m_LAAL_filtered <- m_LAAL_filtered %>% filter(bwa>45 & bwa<135) # only cross-winds
# m_LAAL_filtered <- m_LAAL_filtered %>% filter(bwa>=135) # only tail-winds
ggplot(m_LAAL_filtered, aes(wind_vel,flaps)) + 
  geom_point(color='black')

GAM_LAAL <- gam(formula = flaps ~ te(wind_vel, bwa, k = c(5, 5), bs = c('tp', 'tp')),
                data = m_LAAL_filtered,
                family = poisson(),
                method = "REML")

preds <- predict.gam(GAM_LAAL,m_LAAL_filtered %>% select(wind_vel,bwa))
preds_df <- data.frame(cbind(m_LAAL_filtered,preds))
preds_df <- preds_df %>% mutate(est_trans = exp(preds))

ggplot(m_LAAL_filtered, aes(wind_vel,flaps)) +
  geom_point(color='red') +
  geom_point(data=preds_df, aes(wind_vel,est_trans), color="black")

summary(GAM_LAAL)

################################################################################
# Facet wrap by species, split bwa into tail, cross, and tail:







# all smooths (summed)
ggplot(smooth_ind_summed, aes(wind_vel,estimate_summed,color=id.x)) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black")
  # geom_point(m_BBAL,mapping=aes(wind_vel,flaps),color='black')

# all smooths (not summed)
ggplot(smooth_ind, aes(wind_vel,est,color=id)) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black")

# smooth 1 (summed)
ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20191208_B282"), aes(wind_vel,estimate_summed)) + 
  geom_point(color='blue') +
  # geom_point(data=smooth_global, aes(wind_vel,est_trans), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20191208_B282"), mapping = aes(wind_vel,flaps),color='red')

ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20191221_RP92"), aes(wind_vel,estimate_summed)) + 
  geom_point(color='blue') +
  # geom_point(data=smooth_global, aes(wind_vel,est_trans), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20191221_RP92"), mapping = aes(wind_vel,flaps),color='red')


# smooth 2 (not summed)
ggplot(smooth_ind %>% filter(id == "BBAL_20191221_RP92"), aes(wind_vel,est,color='red')) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20191221_RP92"), mapping = aes(wind_vel,flaps),color='black')

# smooth 3 (summed)
ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20191229_RB25"), aes(wind_vel,estimate_summed,color='red')) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20191229_RB25"), mapping = aes(wind_vel,flaps),color='black')

# smooth 4 (summed)
ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20200106_UB12"), aes(wind_vel,estimate_summed,color='red')) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20200106_UB12"), mapping = aes(wind_vel,flaps),color='black')

# smooth 5 (summed)
ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20201220_BF48"), aes(wind_vel,estimate_summed,color='red')) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20201220_BF48"), mapping = aes(wind_vel,flaps),color='black')

# smooth 6 (summed)
ggplot(smooth_ind_summed %>% filter(id.x == "BBAL_20201223_O543"), aes(wind_vel,estimate_summed,color='red')) + 
  geom_point() +
  geom_point(data=smooth_global, aes(wind_vel,est), color="black") +
  geom_point(m_BBAL %>% filter(id == "BBAL_20201223_O543"), mapping = aes(wind_vel,flaps),color='black')


summary(GAM_BBAL_rnd)

# gam_plot_BBAL <- ggplot(m_BBAL, aes(wind_vel,flaps)) + 
#   geom_point() +
#   geom_line(aes(y=predict(GAM_BBAL_rnd,m_BBAL), color="red", group=id)) + 
#   labs(title = "GAM + scatter plots split by Species (tail-winds)", x="Wind Velocity", y="Flaps per hour")
# gam_plot_BBAL

# GAM_BBAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'),data=m_BBAL)
# plot(GAM_BBAL_rnd)
# summary(GAM_BBAL_rnd)

plot(GAM)

GAM_LAAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_LAAL)
plot(GAM_LAAL_rnd)
summary(GAM_LAAL_rnd)

GAM_BFAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_BFAL)
plot(GAM_BFAL_rnd)
summary(GAM_BFAL_rnd)

GAM_LAAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_LAAL)
plot(GAM_LAAL_rnd)
summary(GAM_LAAL_rnd)

#gamm() to allow for indivudal as a random effect






GAM_BBAL <- getViz(GAM_BBAL)
plot_GAM_BBAL <- plot( sm(GAM_BBAL, 1) )
plot_GAM_BBAL <- plot_GAM_BBAL + l_fitLine(colour = "red") + 
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BBAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")
plot_GAM_BBAL



## why are there so many scatter plot points in the negatives? ? 


GAM_BBAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_BBAL)
GAM_BBAL_rnd <- getViz(GAM_BBAL_rnd)
plot_GAM_BBAL_rnd <- plot( sm(GAM_BBAL_rnd, 1) )
plot_GAM_BBAL_rnd <- plot_GAM_BBAL_rnd + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BBAL (rnd)", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")
plot_GAM_BBAL_rnd


# LAAL model
GAM_LAAL <- gam(flaps ~ s(wind_vel), data=m_LAAL)
GAM_LAAL <- getViz(GAM_LAAL)
plot_GAM_LAAL <- plot( sm(GAM_LAAL, 1) )
plot_GAM_LAAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "LAAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")

# BFAL model
GAM_BFAL <- gam(flaps ~ s(wind_vel), data=m_BFAL)
GAM_BFAL <- getViz(GAM_BFAL)
plot_GAM_BFAL <- plot( sm(GAM_BFAL, 1) )
plot_GAM_BFAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BFAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")

# LAAL model
GAM_LAAL <- gam(flaps ~ s(wind_vel), data=m_LAAL)
GAM_LAAL <- getViz(GAM_LAAL)
plot_GAM_LAAL <- plot( sm(GAM_LAAL, 1) )
plot_GAM_LAAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "LAAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")




# Wind angle plots -------------------------------------------------------------

# set breaks
breaks <- seq(0,360,by=5)
categories <- cut(m_all$w_rel,breaks,include.lowest=TRUE,right=FALSE)
frequency_table <- table(categories)
data <- as.data.frame(frequency_table)
data$categories <- breaks[-length(breaks)]

polar_plot <- ggplot(data, aes(x = categories, y = Freq)) + #, group=3)) +  #, group = day, color = day)) +
  geom_line() +
  coord_polar(start=0) 
  # + scale_y_continuous(limits=c(0,75))
             
polar_plot

polar_plot_bar <- ggplot(data, aes(x = categories, y = Freq)) +
  # geom_bar(stat = "identity", width = 1, fill = "skyblue") +
  geom_bar(stat="identity", alpha=0.5, fill="blue") + 
  coord_polar(start = 0)  # Adjust the starting angle if needed
  # scale_y_continuous(limits=c(0,1200))
  
polar_plot_bar


# Flap plots --------------------------------------------------------------

scatter_plot <- ggplot(m_all, aes(x=wind_vel,y=flaps)) +
  geom_point()

scatter_plot




# this gets rid of tailwinds (setting bwa>60)
m_selectedWinds <- filter(m_all,bwa>60)

# do i separate into inc and BG? 

lme1<-lmer(flaps~wind_vel+(1+wind_vel|Species)+(1+wind_vel|id),data=m_selectedWinds)

# Ok, so including both Species and id leads to singularity
# lme1<-lmer(flaps~wind_vel + (1|Species) + (1|id),data=m_selectedWinds)

lme1 <- lmer(flaps~wind_vel + (1|id),data=m_selectedWinds)

predicted_values <- predict(lme1, newdata = m_selectedWinds, re.form = NULL)

sjPlot::plot_model(lme1, type = "pred", terms = c("wind_vel"), show.ci = TRUE)

combined_plot <- scatter_plot +
  geom_line(aes(y = predicted_values), color = "red", linewidth = 1) +
  labs(title = "Scatter Plot with LME Model")

combined_plot













WS_breaks <- seq(0,max(m_selectedWinds$wind_vel))
WS_categories <- cut(m_selectedWinds$wind_vel,WS_breaks,include.lowest=TRUE,right=FALSE)

# count flaps within these categories!
WS_freqtable <- table(WS_categories)
WS_data <- as.data.frame(WS_freqtable)

## I THINK I NEED TO MAKE A GAM TO FIT THIS......NICE 

# chunk wind vel and average flaps?






