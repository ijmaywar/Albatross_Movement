################################################################################
#
# CREATE ALL FIGURES FOR THESIS
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
library(reshape2)
library(ggExtra)
library(grid)
library(paletteer)
library(bbmle)

# Set Environment --------------------------------------------------------------

read_dir <- '/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/Merged_Hourly_Compiled/'
fullmeta <- read_excel('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/Full_Metadata.xlsx')

setwd(read_dir)
files <- list.files(pattern = '*.csv')
m_all <- read_csv(files[2])

# Edit m_all file as a dataframe for analysis ----------------------------------

# Classify 2BE, 2BEP, E_pip, Ep as Inc
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BE","Inc")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","Inc")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","Inc")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","Inc")))

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

# Create df for analyzable data (remove on-water, remove NAs)
m_model <- m_all %>% filter(HMM_3S_state != 1) %>% 
  drop_na(flaps,wind_vel_kmh,shts,bird_wind_angle,bird_swell_angle,bird_wind_angle_cat,bird_swell_angle_cat)

# Create df for data where the entire trip is captured by GPS
m_poscomplete <- m_all %>% filter(Pos_complete==1)

spp_vec <- c("Black-browed", "Grey-headed", "Wandering", "Black-footed", "Laysan")

# KDE data ---------------------------------------------------------------------

Bird_Island_KDE_df <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/ERA5_MonthlyAvg_10m/Bird_Island_KDEs_avg_env.csv')
Midway_KDE_df <- read_csv('/Users/ian/Library/Mobile Documents/com~apple~CloudDocs/Projects/Albatross/Data_for_analysis/ERA5_MonthlyAvg_10m/Midway_KDEs_avg_env.csv')
compiled_KDE_df <- rbind(Bird_Island_KDE_df,Midway_KDE_df)

# Turn columns into factors
compiled_KDE_df$Species <- factor(compiled_KDE_df$Species, levels=c("BBAL","GHAL","WAAL","BFAL","LAAL"))
compiled_KDE_df$KDE_type <- factor(compiled_KDE_df$KDE_type, levels=c("all","Inc","BG"))
compiled_KDE_df$on_colony <- factor(compiled_KDE_df$on_colony, levels=c(1,0))
compiled_KDE_df$Month <- factor(compiled_KDE_df$Month, levels=1:12)


# Sample stats -----------------------------------------------------------------

nrow(m_all)

m_all %>% summarize(n_distinct(id))
m_model %>% summarize(n_distinct(id))
m_poscomplete %>% summarize(n_distinct(id))

m_model %>% summarize(total_hours=n())
m_model %>% count(Species)
m_model %>% group_by(Species) %>% summarize(unique_IDs=n_distinct(id))
m_model %>% filter(Species == "Black-footed") %>% group_by(Trip_Type) %>% summarize(unique_IDs=n_distinct(id))

m_all_meta <- m_all %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

clipr::write_clip(m_all_meta)

m_model_meta <- m_model %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

clipr::write_clip(m_model_meta)

m_poscomplete_meta <- m_poscomplete %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

clipr::write_clip(m_poscomplete_meta)

# Compare GLS states to 3-state HMM
m_all %>% group_by(GLS_state,HMM_3S_state) %>% summarize(count=n())

# Mean foraging trip duration, max foraging trip duration for each spp, trip_type
dur_stats <- m_all %>% group_by(Species,Trip_Type) %>% 
  summarize(mean_GPS_dur=mean(Pos_L1_2_Dur_Days),mean_acc_dur=mean(na.omit(Acc_L1_Dur_Days)))
dur_stats
clipr::write_clip(dur_stats)

m_all %>% group_by(Trip_Type) %>% 
  summarize(mean_GPS_dur=mean(Pos_L1_2_Dur_Days),mean_acc_dur=mean(na.omit(Acc_L1_Dur_Days)))


# for only complete trips
m_all %>% filter(Pos_complete==1) %>% group_by(Species,Trip_Type) %>% 
  summarize(mean_GPS_dur=mean(Pos_L1_2_Dur_Days),mean_acc_dur=mean(na.omit(Acc_L1_Dur_Days)))
dur_stats
clipr::write_clip(dur_stats)

m_all %>% filter(Pos_complete==1) %>% group_by(Trip_Type) %>% 
  summarize(mean_GPS_dur=mean(Pos_L1_2_Dur_Days),mean_acc_dur=mean(na.omit(Acc_L1_Dur_Days)))

################################################################################
# run all GAMs -----------------------------------------------------------------

fac_k <- 3
All_GAMs <- list()

dist <- "nb"
for (spp in spp_vec) {
  
  m_current <- m_model %>% filter(Species == spp)
  
  # null_GAM <- gam(formula = flaps ~ s(id,k=length(unique(m_current$id)),bs="re"),
  #                 data = m_current,
  #                 family = dist,
  #                 method = "REML")
  # 
  # uni_wind_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
  #                       s(id,k=length(unique(m_current$id)),bs="re"),
  #                     data = m_current,
  #                     family = dist,
  #                     method = "REML")
  # 
  # uni_swell_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
  #                        s(id,k=length(unique(m_current$id)),bs="re"),
  #                      data = m_current,
  #                      family = dist,
  #                      method = "REML")
  # 
  # cat_wind_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,bs='tp',k=3) +
  #                       bird_wind_angle_cat + 
  #                       s(wind_vel_kmh,by=bird_wind_angle_cat,bs='tp',k=3) +
  #                       s(id,k=length(unique(m_current$id)),bs="re"),
  #                     data = m_current,
  #                     family = "nb",
  #                     method = "REML")
  # 
  # cat_swell_GAM <- gam(formula = flaps ~ s(shts,bs='tp',k=3) +
  #                        bird_swell_angle_cat + 
  #                        s(shts,by=bird_swell_angle_cat,bs='tp',k=3) +
  #                        s(id,k=length(unique(m_current$id)),bs="re"),
  #                      data = m_current,
  #                      family = "nb",
  #                      method = "REML")
  # 
  # cont_wind_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bird_wind_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
  #                        s(id,k=length(unique(m_current$id)),bs="re"),
  #                      data = m_current,
  #                      family = dist,
  #                      method = "REML")
  # 
  # cont_swell_GAM <- gam(formula = flaps ~ te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
  #                         s(id,k=length(unique(m_current$id)),bs="re"),
  #                       data = m_current,
  #                       family = dist,
  #                       method = "REML")
  
  te_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                  s(id,k=length(unique(m_current$id)),bs="re"),
                data = m_current,
                family = dist,
                method = "REML")
  
  # All_GAMs[[paste0(spp,"_0")]] <- null_GAM
  # All_GAMs[[paste0(spp,"_1")]] <- uni_wind_GAM
  # All_GAMs[[paste0(spp,"_2")]] <- uni_swell_GAM
  # All_GAMs[[paste0(spp,"_3")]] <- cont_wind_GAM
  # All_GAMs[[paste0(spp,"_4")]] <- cont_swell_GAM
  All_GAMs[[paste0(spp,"_5")]] <- te_GAM
  # All_GAMs[[paste0(spp,"_s1")]] <- cat_wind_GAM
  # All_GAMs[[paste0(spp,"_s2")]] <- cat_swell_GAM
  
}

################################################################################
# find metrics -----------------------------------------------------------------

for (spp in spp_vec) {
  
  spp_All_AICs <- AICctab(All_GAMs[[paste0(spp,"_0")]], All_GAMs[[paste0(spp,"_1")]], 
                          All_GAMs[[paste0(spp,"_2")]], All_GAMs[[paste0(spp,"_3")]],
                          All_GAMs[[paste0(spp,"_4")]], All_GAMs[[paste0(spp,"_5")]],
                          All_GAMs[[paste0(spp,"_s1")]], All_GAMs[[paste0(spp,"_s2")]],
                          weights = TRUE, delta = TRUE, base=TRUE, sort = FALSE)
  
  spp_All_r.sq <- c()
  spp_All_dev.expl <- c()
  
  for (model in c(0:5,"s1","s2")) {
    
    spp_All_r.sq <- c(spp_All_r.sq,summary(All_GAMs[[paste0(spp,"_",model)]])$r.sq)
    spp_All_dev.expl <- c(spp_All_dev.expl,summary(All_GAMs[[paste0(spp,"_",model)]])$dev.expl)
    
  }
  
  spp_All_metrics <- as.data.frame(spp_All_AICs)
  spp_All_metrics$r.sq <- spp_All_r.sq
  spp_All_metrics$dev.expl <- spp_All_dev.expl
  spp_All_metrics$spp <- spp
  
  if (spp == "Black-browed") {
    All_metrics <- spp_All_metrics
  } else {
    All_metrics <- rbind(All_metrics,spp_All_metrics)
  }
  
}

All_metrics

clipr::write_clip(All_metrics)

################################################################################
# Figure of mean wind speed during breeding season relative to study site location 
#
# THESE DON'T HAVE THE CORRECT COLORS

wave_height_long <- melt(m_poscomplete %>% dplyr::select(Species,shts,shww,swh), 
                         id.vars = "Species", variable.name = "Wave_type", value.name = "Wave_height")

# Create the boxplot
ggplot(wave_height_long, aes(x = Species, y = Wave_height, fill = Wave_type)) +
  # scale_fill_brewer(palette = "Set1", labels = c("Swell", "Wind wave", "Combined")) +
  scale_fill_manual(values = c(paletteer_d("nationalparkcolors::Arches")[1],
                               paletteer_d("nationalparkcolors::Arches")[2],
                               paletteer_d("nationalparkcolors::Arches")[4]),
                    labels = c("Swell", "Wind wave", "Combined")) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Species",
       y = "Significant wave height (m)") +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Wave type"))

################################################################################
# Create all fitted value data for plots

for (spp in spp_vec) {
  
  # Call GAMs from list
  # null_GAM <- All_GAMs[[paste0(spp,"_0")]] 
  # uni_wind_GAM <- All_GAMs[[paste0(spp,"_1")]]
  # uni_swell_GAM <- All_GAMs[[paste0(spp,"_2")]]
  # cont_wind_GAM <- All_GAMs[[paste0(spp,"_3")]] 
  # cont_swell_GAM <- All_GAMs[[paste0(spp,"_4")]]
  best_GAM <- All_GAMs[[paste0(spp,"_5")]]
  # cat_wind_GAM <- All_GAMs[[paste0(spp,"_s1")]]
  # cat_swell_GAM <- All_GAMs[[paste0(spp,"_s2")]]
  
  # Create data slices for GAMs for plotting purposes
  # uni_wind_ds  <- data_slice(uni_wind_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100))
  # uni_swell_ds  <- data_slice(uni_swell_GAM, shts = evenly(shts, n=100))
  # cont_wind_ds <- data_slice(cont_wind_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
  #                            bird_wind_angle = evenly(bird_wind_angle, n=100))
  # cont_swell_ds <- data_slice(cont_swell_GAM, shts = evenly(shts, n=100),
  #                             bird_swell_angle = evenly(bird_swell_angle, n=100))
  # best_ds <- data_slice(best_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
  #                       shts = evenly(shts, n=100))
  best_ds_wind <- data_slice(best_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
                        shts = c(min(best_GAM$model$shts,na.rm=TRUE),mean(best_GAM$model$shts,na.rm=TRUE)))
  best_ds_swell <- data_slice(best_GAM, wind_vel_kmh = c(min(best_GAM$model$wind_vel_kmh,na.rm=TRUE),mean(best_GAM$model$wind_vel_kmh,na.rm=TRUE)),
                        shts = evenly(shts, n=100))
  # cat_wind_ds <- data_slice(cat_wind_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
  #                           bird_wind_angle_cat = c("head","cross","tail"))
  # cat_swell_ds <- data_slice(cat_swell_GAM, shts = evenly(shts, n=100),
  #                            bird_swell_angle_cat = c("head","cross","tail"))
  # Harris_wind_ds <- data_slice(cont_wind_GAM, wind_vel_kmh = seq(0, max(wind_vel_kmh, na.rm=TRUE), by = 5),
  #                            bird_wind_angle = evenly(bird_wind_angle, n=100))
  # Harris_swell_ds <- data_slice(cont_swell_GAM, shts = seq(0, max(shts, na.rm=TRUE), by = 1),
  #                             bird_swell_angle = evenly(bird_swell_angle, n=100))
  # Harris_best_ds <- data_slice(best_GAM, wind_vel_kmh = seq(0, max(wind_vel_kmh, na.rm=TRUE), by = 5),
  #                              shts = seq(0, max(shts, na.rm=TRUE), by = 1))
  
  # Find fitted values for given species
  # uni_wind_fv_spp <- cbind(uni_wind_ds$wind_vel_kmh, rep(spp,nrow(uni_wind_ds)),
  #                          fitted_values(uni_wind_GAM, data = uni_wind_ds, scale = "link",
  #                                        terms = c("(Intercept)","s(wind_vel_kmh)"))[,3:6])
  # uni_swell_fv_spp <- cbind(uni_swell_ds$shts, rep(spp,nrow(uni_swell_ds)),
  #                           fitted_values(uni_swell_GAM, data = uni_swell_ds, scale = "link",
  #                                         terms = c("(Intercept)","s(shts)"))[,3:6])
  # cont_wind_fv_spp <- cbind(cont_wind_ds[,1:2], rep(spp,nrow(cont_wind_ds)),
  #                           fitted_values(cont_wind_GAM, data = cont_wind_ds, scale = "link",
  #                                         terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)"))[,4:7])
  # cont_swell_fv_spp <- cbind(cont_swell_ds[,1:2], rep(spp,nrow(cont_swell_ds)),
  #                            fitted_values(cont_swell_GAM, data = cont_swell_ds, scale = "link",
  #                                          terms = c("(Intercept)","te(shts,bird_swell_angle)"))[,4:7])
  # best_fv_spp <- cbind(best_ds[,1:2], rep(spp,nrow(best_ds)),
  #                      fitted_values(best_GAM, data = best_ds, scale = "link",
  #                                    terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  best_fv_spp_wind <- cbind(best_ds_wind[,1:2], rep(spp,nrow(best_ds_wind)),
                       fitted_values(best_GAM, data = best_ds_wind, scale = "link",
                                     terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  best_fv_spp_swell <- cbind(best_ds_swell[,1:2], rep(spp,nrow(best_ds_swell)),
                                 fitted_values(best_GAM, data = best_ds_swell, scale = "link",
                                               terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  # cat_wind_fv_spp <- cbind(cat_wind_ds[,1:2],
  #                          rep(spp,nrow(cat_wind_ds)),
  #                          fitted_values(cat_wind_GAM, data = cat_wind_ds, scale = "link",
  #                                        terms = c("(Intercept)",smooths(cat_wind_GAM)[1:3]))[,4:7])
  # cat_swell_fv_spp <- cbind(cat_swell_ds[,1:2],
  #                           rep(spp,nrow(cat_swell_ds)),
  #                           fitted_values(cat_swell_GAM, data = cat_swell_ds, scale = "link",
  #                                         terms = c("(Intercept)",smooths(cat_swell_GAM)[1:3]))[,4:7])
  # Harris_wind_fv_spp <- cbind(Harris_wind_ds[,1:2], rep(spp,nrow(Harris_wind_ds)),
  #                           fitted_values(cont_wind_GAM, data = Harris_wind_ds, scale = "link",
  #                                         terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)"))[,4:7])
  # Harris_swell_fv_spp <- cbind(Harris_swell_ds[,1:2], rep(spp,nrow(Harris_swell_ds)),
  #                            fitted_values(cont_swell_GAM, data = Harris_swell_ds, scale = "link",
  #                                          terms = c("(Intercept)","te(shts,bird_swell_angle)"))[,4:7])
  # Harris_best_fv_spp <- cbind(Harris_best_ds[,1:2], rep(spp,nrow(Harris_best_ds)),
  #                      fitted_values(best_GAM, data = Harris_best_ds, scale = "link",
  #                                    terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  
  if (spp == "Black-browed") {
    # uni_wind_fv <- uni_wind_fv_spp
    # uni_swell_fv <- uni_swell_fv_spp
    # cont_wind_fv <- cont_wind_fv_spp
    # cont_swell_fv <- cont_swell_fv_spp
    # best_fv <- best_fv_spp
    best_fv_wind <- best_fv_spp_wind
    best_fv_swell <- best_fv_spp_swell
    # cat_wind_fv <- cat_wind_fv_spp
    # cat_swell_fv <- cat_swell_fv_spp
    # Harris_wind_fv <- Harris_wind_fv_spp
    # Harris_swell_fv <- Harris_swell_fv_spp
    # Harris_best_fv <- Harris_best_fv_spp
  } else {
    # uni_wind_fv <- rbind(uni_wind_fv,uni_wind_fv_spp)
    # uni_swell_fv <- rbind(uni_swell_fv,uni_swell_fv_spp)
    # cont_wind_fv <- rbind(cont_wind_fv,cont_wind_fv_spp)
    # cont_swell_fv <- rbind(cont_swell_fv,cont_swell_fv_spp)
    # best_fv <- rbind(best_fv,best_fv_spp)
    best_fv_wind <- rbind(best_fv_wind,best_fv_spp_wind)
    best_fv_swell <- rbind(best_fv_swell,best_fv_spp_swell)
    # cat_wind_fv <- rbind(cat_wind_fv,cat_wind_fv_spp)
    # cat_swell_fv <- rbind(cat_swell_fv,cat_swell_fv_spp)
    # Harris_wind_fv <- rbind(Harris_wind_fv,Harris_wind_fv_spp)
    # Harris_swell_fv <- rbind(Harris_swell_fv,Harris_swell_fv_spp)
    # Harris_best_fv <- rbind(Harris_best_fv,Harris_best_fv_spp)
  }
}

# Rename columns of fitted values
# colnames(uni_wind_fv) <- c("wind_vel_kmh","Species",
#                            "fitted_global","se_global","lower_global","upper_global")
# colnames(uni_swell_fv) <- c("shts","Species",
#                             "fitted_global","se_global","lower_global","upper_global")
# colnames(cont_wind_fv) <- c("wind_vel_kmh","bird_wind_angle","Species",
#                             "fitted_global","se_global","lower_global","upper_global")
# colnames(cont_swell_fv) <- c("shts","bird_swell_angle","Species",
#                              "fitted_global","se_global","lower_global","upper_global")
# colnames(best_fv) <- c("wind_vel_kmh","shts","Species",
#                        "fitted_global","se_global","lower_global","upper_global")
colnames(best_fv_wind) <- c("wind_vel_kmh","shts","Species",
                       "fitted_global","se_global","lower_global","upper_global")
colnames(best_fv_swell) <- c("wind_vel_kmh","shts","Species",
                                 "fitted_global","se_global","lower_global","upper_global")
# colnames(cat_wind_fv) <- c("wind_vel_kmh","bird_wind_angle_cat","Species",
#                            "fitted_global","se_global","lower_global","upper_global")
# colnames(cat_swell_fv) <- c("shts","bird_swell_angle_cat","Species",
#                             "fitted_global","se_global","lower_global","upper_global")
# colnames(Harris_wind_fv) <- c("wind_vel_kmh","bird_wind_angle","Species",
#                             "fitted_global","se_global","lower_global","upper_global")
# colnames(Harris_swell_fv) <- c("shts","bird_swell_angle","Species",
#                              "fitted_global","se_global","lower_global","upper_global")
# colnames(Harris_best_fv) <- c("wind_vel_kmh","shts","Species",
#                        "fitted_global","se_global","lower_global","upper_global")

# Convert species to a factor
# uni_wind_fv$Species <- factor(uni_wind_fv$Species, levels=spp_vec)
# uni_swell_fv$Species <- factor(uni_swell_fv$Species, levels=spp_vec)
# cont_wind_fv$Species <- factor(cont_wind_fv$Species, levels=spp_vec)
# cont_swell_fv$Species <- factor(cont_swell_fv$Species, levels=spp_vec)
# best_fv$Species <- factor(best_fv$Species, levels=spp_vec)
best_fv_wind$Species <- factor(best_fv_wind$Species, levels=spp_vec)
best_fv_swell$Species <- factor(best_fv_swell$Species, levels=spp_vec)
# cat_wind_fv$Species <- factor(cat_wind_fv$Species, levels=spp_vec)
# cat_swell_fv$Species <- factor(cat_swell_fv$Species, levels=spp_vec)
# Harris_wind_fv$Species <- factor(Harris_wind_fv$Species, levels=spp_vec)
# Harris_swell_fv$Species <- factor(Harris_swell_fv$Species, levels=spp_vec)
# Harris_best_fv$Species <- factor(Harris_best_fv$Species, levels=spp_vec)

################################################################################
# Uni-directional plots

fig_wind_simple <- ggplot(uni_wind_fv) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_simple <- ggplot(uni_swell_fv) +
  geom_line(aes(shts,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  labs(y="Flaps/hour",
       x="Significant height of total swell (m)") +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

wrap_elements(panel = fig_wind_simple / fig_shts_simple)
# 800 x 500

################################################################################
# Plot the flaps/hour * relative angle figures (NEW TO THE EDITS)

write.csv(best_fv_wind, "/Users/ian/Desktop/Manuscript_edits/Data/best_fv_wind.csv", row.names = FALSE)
write.csv(best_fv_swell, "/Users/ian/Desktop/Manuscript_edits/Data/best_fv_swell.csv", row.names = FALSE)

# Flapping rate vs. windspeed (using min shts)
best_fv_wind_min_shts <- best_fv_wind %>%
  group_by(Species) %>%
  filter(shts == min(shts, na.rm = TRUE)) %>%
  ungroup()

fig_wind_te_min_shts <- ggplot(best_fv_wind_min_shts) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  ylim(0,10000) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_te_min_shts

# Flapping rate vs. windspeed (using mean shts)
best_fv_wind_mean_shts <- best_fv_wind %>%
  group_by(Species) %>%
  filter(shts == max(shts, na.rm = TRUE)) %>%
  ungroup()

fig_wind_te_mean_shts <- ggplot(best_fv_wind_mean_shts) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_te_mean_shts

# Flapping rate vs. shts (using min windspeed)
best_fv_shts_min_wind <- best_fv_swell %>%
  group_by(Species) %>%
  filter(wind_vel_kmh == min(wind_vel_kmh, na.rm = TRUE)) %>%
  ungroup()

fig_shts_te_min_wind <- ggplot(best_fv_shts_min_wind) +
  geom_line(aes(shts,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Significant height of total swell (m)") +
  ylim(0,10000) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_te_min_wind

# Flapping rate vs. shts (using min windspeed)
best_fv_shts_mean_wind <- best_fv_swell %>%
  group_by(Species) %>%
  filter(wind_vel_kmh == max(wind_vel_kmh, na.rm = TRUE)) %>%
  ungroup()

fig_shts_te_mean_wind <- ggplot(best_fv_shts_mean_wind) +
  geom_line(aes(shts,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Significant height of total swell (m)") +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_te_mean_wind

# Plot both min figs (1000 x 300)
wrap_elements(panel = fig_wind_te_min_shts / fig_shts_te_min_wind)

# Plot both mean figs (1000 x 300)
wrap_elements(panel = fig_wind_te_mean_shts / fig_shts_te_mean_wind)

################################################################################
# Uni-directional plots using full tensor product (NEW TO THE EDITS)

# Save fv dataframes
write.csv(Harris_wind_fv, "/Users/ian/Desktop/Manuscript_edits/Data/Harris_wind_fv.csv", row.names = FALSE)
write.csv(Harris_swell_fv, "/Users/ian/Desktop/Manuscript_edits/Data/Harris_swell_fv.csv", row.names = FALSE)
write.csv(Harris_best_fv, "/Users/ian/Desktop/Manuscript_edits/Data/Harris_best_fv.csv", row.names = FALSE)

# Wind
Harris_wind_fv$Species <- factor(Harris_wind_fv$Species, levels=spp_vec)

fig_wind_Harris <- ggplot(Harris_wind_fv %>% filter(wind_vel_kmh %in% seq(10, max(wind_vel_kmh, na.rm = TRUE), by = 10),), 
                          aes(x = bird_wind_angle, y = exp(fitted_global), group = factor(wind_vel_kmh))) +
  geom_ribbon(aes(ymin = exp(lower_global), ymax = exp(upper_global), fill = factor(wind_vel_kmh)), alpha = 0.3) +
  geom_line(aes(color = factor(wind_vel_kmh)), size = 1) +
  scale_color_viridis_d(option = "rocket", direction = -1, name = "Wind Velocity (km/h)") +
  scale_fill_viridis_d(option = "rocket", direction = -1, name = "Wind Velocity (km/h)") +
  scale_x_continuous(breaks = seq(0, 180, by = 30)) +  # <- Custom x-axis ticks
  labs(
    x = "Bird-Wind Angle (°)",
    y = "Flaps/hour",
    color = "Wind Velocity (km/h)",
    fill = "Wind Velocity (km/h)",
    title = "Effect of Bird-Wind Angle at Different Wind Velocities"
  ) +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_Harris

# 1100 x 300

# Swell
Harris_swell_fv$Species <- factor(Harris_swell_fv$Species, levels=spp_vec)

fig_swell_Harris <- ggplot(Harris_swell_fv %>% filter(shts %in% seq(1, 6, by = 1)), 
                          aes(x = bird_swell_angle, y = exp(fitted_global), group = factor(shts))) +
  geom_ribbon(aes(ymin = exp(lower_global), ymax = exp(upper_global), fill = factor(shts)), alpha = 0.3) +
  geom_line(aes(color = factor(shts)), size = 1) +
  scale_color_viridis_d(option = "mako", direction = -1, name = "Swell height (m)") +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "Swell height (m)") +
  scale_x_continuous(breaks = seq(0, 180, by = 30)) +  # <- Custom x-axis ticks
  labs(
    x = "Bird-Swell Angle (°)",
    y = "Flaps/hour",
    color = "Swell height (m)",
    fill = "Swell height (m)",
    title = "Effect of Bird-Swell Angle at Different Wind Velocities"
  ) +
  ylim(0,1500) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swell_Harris

# 1100 x 300

################################################################################
# TRIM RESPONSES of te GAMs BASED ON EXPERIENCED VALUES

# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_wind_all <- list()
response_df_mask_swell_all <- list()
response_df_mask_best_all <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_wind <- ks::kde(m_model %>%
                       filter(Species == spp) %>%
                       dplyr::select(wind_vel_kmh,bird_wind_angle),
                     compute.cont=TRUE,gridsize = grid_size)
  kd_swell <- ks::kde(m_model %>%
                        filter(Species == spp) %>%
                        dplyr::select(shts,bird_swell_angle),
                      compute.cont=TRUE,gridsize = grid_size)
  kd_best <- ks::kde(m_model %>%
                       filter(Species == spp) %>%
                        dplyr::select(wind_vel_kmh,shts),
                      compute.cont=TRUE,gridsize = grid_size)

  contour_99_wind <- data.frame(with(kd_wind, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  contour_99_swell <- data.frame(with(kd_swell, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                           z=estimate, levels=cont["1%"])[[1]]))
  contour_99_best <- data.frame(with(kd_best, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["1%"])[[1]]))

  contour_99_wind_vect <- as.polygons(as.lines((contour_99_wind %>% vect(geom=c('x','y')))))
  contour_99_swell_vect <- as.polygons(as.lines((contour_99_swell %>% vect(geom=c('x','y')))))
  contour_99_best_vect <- as.polygons(as.lines((contour_99_best %>% vect(geom=c('x','y')))))

  # Mask GAM response values for plotting
  response_rast_wind <- terra::rast(cont_wind_fv %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, bird_wind_angle, fitted_global), type='xyz')
  response_rast_mask_wind = terra::mask(response_rast_wind, contour_99_wind_vect)
  response_df_mask_wind = as.data.frame(response_rast_mask_wind, xy=T)

  response_rast_swell <- terra::rast(cont_swell_fv %>% filter(Species == spp) %>% dplyr::select(shts, bird_swell_angle, fitted_global), type='xyz')
  response_rast_mask_swell = terra::mask(response_rast_swell, contour_99_swell_vect)
  response_df_mask_swell = as.data.frame(response_rast_mask_swell, xy=T)

  response_rast_best <- terra::rast(best_fv %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, shts, fitted_global), type='xyz')
  response_rast_mask_best = terra::mask(response_rast_best, contour_99_best_vect)
  response_df_mask_best = as.data.frame(response_rast_mask_best, xy=T)
  
  # # Create 95% KDEs
  # kd_wind <- ks::kde(m_model %>%
  #                      filter(Species == spp) %>%
  #                      dplyr::select(wind_vel_kmh,bird_wind_angle),
  #                    compute.cont=TRUE,gridsize = grid_size)
  # kd_swell <- ks::kde(m_model %>%
  #                       filter(Species == spp) %>%
  #                       dplyr::select(shts,bird_swell_angle),
  #                     compute.cont=TRUE,gridsize = grid_size)
  # kd_best <- ks::kde(m_model %>%
  #                      filter(Species == spp) %>%
  #                      dplyr::select(wind_vel_kmh,shts),
  #                    compute.cont=TRUE,gridsize = grid_size)
  # 
  # contour_95_wind <- data.frame(with(kd_wind, contourLines(x=eval.points[[1]], y=eval.points[[2]],
  #                                                          z=estimate, levels=cont["5%"])[[1]]))
  # contour_95_swell <- data.frame(with(kd_swell, contourLines(x=eval.points[[1]], y=eval.points[[2]],
  #                                                            z=estimate, levels=cont["5%"])[[1]]))
  # contour_95_best <- data.frame(with(kd_best, contourLines(x=eval.points[[1]], y=eval.points[[2]],
  #                                                          z=estimate, levels=cont["5%"])[[1]]))
  # 
  # contour_95_wind_vect <- as.polygons(as.lines((contour_95_wind %>% vect(geom=c('x','y')))))
  # contour_95_swell_vect <- as.polygons(as.lines((contour_95_swell %>% vect(geom=c('x','y')))))
  # contour_95_best_vect <- as.polygons(as.lines((contour_95_best %>% vect(geom=c('x','y')))))
  # 
  # # Mask GAM response values for plotting
  # response_rast_wind <- terra::rast(cont_wind_fv %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, bird_wind_angle, fitted_global), type='xyz')
  # response_rast_mask_wind = terra::mask(response_rast_wind, contour_95_wind_vect)
  # response_df_mask_wind = as.data.frame(response_rast_mask_wind, xy=T)
  # 
  # response_rast_swell <- terra::rast(cont_swell_fv %>% filter(Species == spp) %>% dplyr::select(shts, bird_swell_angle, fitted_global), type='xyz')
  # response_rast_mask_swell = terra::mask(response_rast_swell, contour_95_swell_vect)
  # response_df_mask_swell = as.data.frame(response_rast_mask_swell, xy=T)
  # 
  # response_rast_best <- terra::rast(best_fv %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, shts, fitted_global), type='xyz')
  # response_rast_mask_best = terra::mask(response_rast_best, contour_95_best_vect)
  # response_df_mask_best = as.data.frame(response_rast_mask_best, xy=T)
  
  # Save values for all spp
  response_df_mask_wind$Species <- spp
  response_df_mask_swell$Species <- spp
  response_df_mask_best$Species <- spp
  
  if (spp == "Black-browed") {
    response_df_mask_wind_all <- response_df_mask_wind
    response_df_mask_swell_all <- response_df_mask_swell
    response_df_mask_best_all <- response_df_mask_best
  } else {
    response_df_mask_wind_all <- rbind(response_df_mask_wind_all,response_df_mask_wind)
    response_df_mask_swell_all <- rbind(response_df_mask_swell_all,response_df_mask_swell)
    response_df_mask_best_all <- rbind(response_df_mask_best_all,response_df_mask_best)
  }
  
}

response_df_mask_wind_all$Species <- factor(response_df_mask_wind_all$Species, 
                                            levels=spp_vec)
response_df_mask_swell_all$Species <- factor(response_df_mask_swell_all$Species,
                                             levels=spp_vec)
response_df_mask_best_all$Species <- factor(response_df_mask_best_all$Species,
                                            levels=spp_vec)

################################################################################
# Plot the magnitude * relative angle figures

# Reds for winds
reds <- colorRampPalette(c("#E2E2E2FF","red4"))
fig_wave_cont_trim <- ggplot(response_df_mask_wind_all) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      # breaks=seq(from=0,to=1600,by=100)) +
                      breaks=getJenksBreaks(exp(response_df_mask_wind_all$fitted_global),11)) +
  scale_fill_manual(values=reds(10),drop=FALSE,guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Flaps/hour") +
  scale_y_continuous(breaks=seq(0,180,60)) +
  xlab("Windspeed (km/h)") +
  ylab("Bird-wind angle (degrees)") +
  facet_wrap(~Species,nrow=1) + 
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.text=element_text(size=8),
        legend.key.size = unit(.5, 'cm'))

# Blues for swells
blues <- colorRampPalette(c("#E2E2E2FF","navy"))
fig_swell_cont_trim <- ggplot(response_df_mask_swell_all) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      # breaks=seq(from=0,to=1600,by=100)) +
                      breaks=round(getJenksBreaks(exp(response_df_mask_swell_all$fitted_global),11))) +
  scale_fill_manual(values=blues(10),drop=FALSE,guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Flaps/hour") +
  scale_y_continuous(breaks=seq(0,180,60)) +
  xlab("Significant height of total swell (m)") +
  ylab("Bird-swell angle (degrees)") +
  facet_wrap(~Species,nrow=1) + 
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.text=element_text(size=8),
        legend.key.size = unit(.5, 'cm'))

wrap_elements(panel = fig_wave_cont_trim / fig_swell_cont_trim)
# Save as 700 x 485 figure

################################################################################
# Plot the wind * swell figure

ggplot(response_df_mask_best_all) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      # breaks=seq(from=0,to=4000,by=100)) +
                      breaks=round(getJenksBreaks(exp(response_df_mask_best_all$fitted_global),11))) +
  scale_fill_manual(values=magma(10),drop=FALSE,guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.text=element_text(size=8),
        legend.key.size = unit(.5, 'cm'))

# Save: 700 x 275

E_savings <- data.frame(spp=character(),
                        high=numeric(),
                        low=numeric())

for (spp in spp_vec) {
  
  # Find highest 5% of responses
  spp_response <- response_df_mask_best_all %>% filter(Species==spp)
  spp_response_highest <- spp_response %>% 
    filter(exp(fitted_global)>=quantile(exp(spp_response$fitted_global),probs=1-.05))
  
  # Find lowest 5% of responses
  spp_response_lowest <- spp_response %>% 
    filter(exp(fitted_global)<=quantile(exp(spp_response$fitted_global),probs=.05))
  
  spp_meta <- c(spp,
                mean(exp(spp_response_highest$fitted_global),na.rm=TRUE),
                mean(exp(spp_response_lowest$fitted_global),na.rm=TRUE))
  
  E_savings <- rbind(E_savings,spp_meta)
}

colnames(E_savings) <- c("spp","high","low")

E_savings$high <- as.numeric(E_savings$high)
E_savings$low <- as.numeric(E_savings$low)
E_savings$savings <- 100*((E_savings$high-E_savings$low)/E_savings$high)

E_savings

clipr::write_clip(E_savings)

# Energy savings

################################################################################
# Box of violin plots of wind and waves experienced by species 
# [this and the boxplot below should only represent complete trips; 
# all other analyses can use all available data]
#   2x5 plot

fig_winds <- m_poscomplete |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3,outliers=FALSE) +
  labs(y="Windspeed (km/h)") +
  ylim(0,95) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swells <- m_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3,outliers=FALSE) +
  labs(y="Significant height of total swell (m)") +
  theme_linedraw() +
  ylim(0,9.5) + 
  theme(axis.title.x = element_blank(),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        strip.text = element_blank())

wrap_elements(panel = fig_winds / fig_swells)
# 500 x 500

################################################################################
# Montly env plots: 2 5x1 figs

# Windspeed km/h
monthly_wind <- ggplot(compiled_KDE_df %>% filter(KDE_type=="all"), 
                       aes(x = Month, y = si10*3.6)) +
  geom_boxplot(width=0.6,outliers = FALSE) +
  labs(y = "Windspeed (km/h)") +
  scale_x_discrete(labels = c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.",
                              "Sep.","Oct.","Nov.","Dec.")) +
  theme_linedraw() +
  facet_wrap(~Species, nrow=5) + 
  theme(axis.title.x = element_blank(),
        strip.text = element_blank())

# Swell height m
monthly_swell <- ggplot(compiled_KDE_df %>% filter(KDE_type=="all"), 
                        aes(x = Month, y = shts)) +
  geom_boxplot(width=0.6,outliers = FALSE) +
  labs(y = "Significant height of total swell (m)") +
  scale_x_discrete(labels = c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.",
                              "Sep.","Oct.","Nov.","Dec.")) +
  theme_linedraw() +
  facet_wrap(~Species, nrow=5) + 
  theme(axis.title.x = element_blank(),
        strip.text = element_blank())

wrap_elements(panel = monthly_wind / monthly_swell)

# 700 x 700

################################################################################
# Box plots of prop. time spent in high/med/low winds AND wave heights

# Winds
bird_wind_angle_cat_hist_data <- as.data.frame(m_poscomplete %>%
                                                 drop_na(bird_wind_angle_cat) %>% 
                                                 group_by(Location,Species,id,bird_wind_angle_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count)))


# Randomly downsample m_poscomplete such that there are 34 individuals for each study site.
m_poscomplete_spp_ds <- rbind(
  m_poscomplete %>% filter(Species == "Black-browed") %>% filter(id %in% sample(unique(id), 11)),
  m_poscomplete %>% filter(Species == "Grey-headed") %>% filter(id %in% sample(unique(id), 12)),
  m_poscomplete %>% filter(Species == "Wandering") %>% filter(id %in% sample(unique(id), 11)),
  m_poscomplete %>% filter(Species == "Black-footed"),
  m_poscomplete %>% filter(Species == "Laysan") %>% filter(id %in% sample(unique(id), 17)))

wind_vel_kmh_breaks <- quantile(m_poscomplete_spp_ds$wind_vel_kmh, probs=c((1/3),(2/3)))
shts_cat_breaks <- quantile(m_poscomplete_spp_ds$shts, probs=c((1/3),(2/3)))

m_poscomplete <- m_poscomplete %>% 
  mutate(shts_cat = case_when(shts<shts_cat_breaks[[1]] ~ "low",
                              shts>=shts_cat_breaks[[1]] & shts<shts_cat_breaks[[2]] ~ "medium",
                              shts>=shts_cat_breaks[[2]] ~ "high"),
         wind_vel_kmh_cat = case_when(wind_vel_kmh<wind_vel_kmh_breaks[[1]] ~ "low",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[1]] & wind_vel_kmh<wind_vel_kmh_breaks[[2]] ~ "medium",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[2]] ~ "high"))

wind_vel_kmh_cat_density_data <- as.data.frame(m_poscomplete %>% group_by(Location,Species,id,wind_vel_kmh_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count),
                                                        wind_vel_kmh_cat = factor(wind_vel_kmh_cat,levels=c("low","medium","high"))))

shts_cat_density_data <- as.data.frame(m_poscomplete %>% group_by(Location,Species,id,shts_cat) %>% 
                                         summarize(count=n()) %>% 
                                         mutate(proportion = count/sum(count),
                                                shts_cat = factor(shts_cat,levels=c("low","medium","high"))))


windspeed_prop <- ggplot(wind_vel_kmh_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=wind_vel_kmh_cat),outliers = FALSE) +
  scale_fill_manual(values = rev(reds(3)),
                    labels = c(paste0("Low"), paste0("Medium"), paste0("High"))) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Windspeed category"))

wave_height_prop <- ggplot(shts_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=shts_cat),outliers = FALSE) +
  scale_fill_manual(values = rev(blues(3)),
                    labels = c(paste0("Low"), paste0("Medium"), paste0("High"))) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Swell height category"))


wrap_elements(panel = windspeed_prop / wave_height_prop)

# 1000 x 400



################################################################################
# Box or violin plots [or density plots?] of prop. time spent in 
# high/ side/ tail winds AND wave direction [with/  against/ across wave direction] 

# Winds
bird_wind_angle_cat_hist_data <- as.data.frame(m_poscomplete %>%
                                                 drop_na(bird_wind_angle_cat) %>% 
                                                 group_by(Location,Species,id,bird_wind_angle_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count)))

# Swells
swell_bird_angle_cat_density_data <- as.data.frame(m_poscomplete %>%
                                                     drop_na(bird_swell_angle_cat) %>% 
                                                     group_by(Location,Species,id,bird_swell_angle_cat) %>% 
                                                     summarize(count=n()) %>% 
                                                     mutate(proportion = count/sum(count)))


dir_cat_cols <- c("#9484B1FF", "#F1C100FF","#496849FF")
# Wind boxplot
wind_dir_prop <- ggplot(bird_wind_angle_cat_hist_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=bird_wind_angle_cat), outliers=FALSE) +
  scale_fill_manual(values = dir_cat_cols,labels = c("Head", "Cross", "Tail")) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="BWA category"))

# Wave boxplot
wave_dir_prop <- ggplot(swell_bird_angle_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=bird_swell_angle_cat), outliers=FALSE) +
  scale_fill_manual(values = dir_cat_cols, labels = c("Against", "Across", "With")) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() + 
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="BSA category"))

wrap_elements(panel = wind_dir_prop / wave_dir_prop)

# 1000 x 400

################################################################################
# Table of deployments based on field season

m_all %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

m_poscomplete %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

################################################################################
# Supplemental figures and tables
################################################################################

# s1 and s2 are showing how the LULU filter and flap detection works

# s3: relationship between windspeed and significant height of waves

# Swells
fig_ws_shts <- ggplot(m_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Total swell") +
  ylim(0,13) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

# Wind waves
fig_ws_shww <- ggplot(m_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel_kmh,y=shww),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Wind waves") +
  ylim(0,13) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

# Total waves
fig_ws_swh <- ggplot(m_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel_kmh,y=swh),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Surface sea waves") +
  ylim(0,13) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

# Wrap windspeed vs wave height figs
wrap_elements(panel = fig_ws_shts / fig_ws_shww / fig_ws_swh) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")


# s4: Comparing the significant heights of the 3 swell types -------------------

# Categorize wave heights
# Reshape data to long format
wave_height_long <- melt(m_poscomplete %>% dplyr::select(Species,shts,shww,swh), 
                         id.vars = "Species", variable.name = "Wave_type", value.name = "Wave_height")

# Create the boxplot
ggplot(wave_height_long) +
  geom_boxplot(aes(x=Species,y=Wave_height,fill=Wave_type),outliers = FALSE) +
  scale_fill_manual(values = c("#000080",'#FCA636FF','#6A00A8FF'),labels = c("Total swell", "Wind waves", "Surface sea waves")) +
  labs(y="Significant height of wave (m)") +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Wave type"))

# 


# s5: Pie graphic --------------------------------------------------------------

pie_colors <- c("#9484B1FF", "#F1C100FF","#496849FF", "#F1C100FF")
ggplot(data.frame(cat=factor(c("head","cross","tail","cross_2"),levels=c("head","cross","tail","cross_2")),
                  value=c(6,3,6,3)),
       aes(x="",y=value,fill=cat)) +
  geom_bar(width=1,stat="identity") +
  coord_polar(theta="y",start=60*(pi/180)) +
  scale_fill_manual(values=pie_colors) +
  theme_void() +
  guides(fill=FALSE)


################################################################################
# Plot categorical GAMs
# These were removed from the thesis and the manuscript

dir_cat_cols <- c("#9484B1FF", "#F1C100FF","#496849FF")
fig_wind_cat <- ggplot(cat_wind_fv) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global),color=bird_wind_angle_cat)) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wind_angle_cat,fill=bird_wind_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title = "BWA category",
                            override.aes = list(fill = dir_cat_cols)),
         fill="none") +
  scale_color_manual(values=dir_cat_cols,
                     labels=c("Head","Cross","Tail")) + 
  scale_fill_manual(values=dir_cat_cols,
                    labels=c("Head","Cross","Tail")) + 
  labs(y="Flaps/hour") +
  ylim(0,2500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swell_cat <- ggplot(cat_swell_fv) +
  geom_line(aes(shts,exp(fitted_global),color=bird_swell_angle_cat)) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_swell_angle_cat,fill=bird_swell_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title = "BSA category",
                            override.aes = list(fill = dir_cat_cols)),
         fill="none") +
  scale_color_manual(values=dir_cat_cols,
                     labels=c("Against","Across","With")) + 
  scale_fill_manual(values=dir_cat_cols,
                    labels=c("Against","Across","With")) + 
  labs(y="Flaps/hour") +
  ylim(0,2500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

wrap_elements(panel = fig_wind_cat / fig_swell_cat)

