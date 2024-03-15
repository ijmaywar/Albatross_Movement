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

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(readxl)
# library(Matrix)
library(lme4)
library(stringr)
library(dplyr)
# library(mgcv)
library(gamm4)
library(mgcViz)
library(gridExtra)
library(patchwork)
library(gratia)

# Set Environment ---------------------------------------------------------

fullmeta <- read_excel("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Full_Metadata.xlsx")
GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"

# Combine all files across field seasons and locations -------------------------
m_all <- 0
for (i_loc in 1:length(locations)) {
  loc = locations[i_loc]
  
  if (loc == "Bird_Island") {
    szns = c("2019_2020","2020_2021","2021_2022")
  } else if (loc == "Midway") {
    szns = c("2018_2019","2021_2022","2022_2023")
  }
  
  for (i_szn in 1:length(szns)) {
    szn = szns[i_szn]
    wind_L4_dir <- paste0(GD_dir,"L4/",loc,"/Wind_Data/",szn,"/")
    setwd(wind_L4_dir)
    files <- list.files(pattern='*.csv')
    
    # skip WAAL files
    indices_to_remove <- grep("^WAAL", files)
    if (length(indices_to_remove != 0)) {
      files <- files[-indices_to_remove]
    }
    
    # combine all files and label them with the szn
    for (i in 1:length(files)) {
      bird <- str_sub(files[i],1,-17)
      birdmeta <- fullmeta %>% dplyr::filter(Deployment_ID == bird)
      
      # only use the file if metadata indicates that the bird is usable
      if (birdmeta$Focus == 1) {
        m <- read.csv(files[i])
        m$szn = szn
        m$loc = loc
        m$Trip_Type = birdmeta$Trip_Type
        m$spp = birdmeta$Species
        
        if (length(m_all)==1) {
          m_all <- m
        } else {
          m_all <- rbind(m_all,m)
        }
      }
    }
  }
}

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$szn <- as.factor(m_all$szn)
m_all$loc <- as.factor(m_all$loc)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$spp <- as.factor(m_all$spp)

# Re-order spp groups
m_all$spp <- factor(m_all$spp , levels=c("BBAL", "GHAL", "BFAL", "LAAL"))

# Classify 2BEP, E_pip, Ep as BG
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","BG")))

# Datetime stuff
m_all$datetime <- as.POSIXlt(m_all$datetime,format="%Y-%m-%d %H:%M:%S")
m_all$julian <- m_all$datetime$yday + 1 
# plotday are the days since the beginning of the year (January 1 of the first
# year is 1)
m_all$plotday <- ifelse(m_all$julian > 200, m_all$julian, m_all$julian + 365)
# adjust for leap years
m_all$plotday <- ifelse(m_all$plotday > 365 & m_all$datetime$year+1900 == 2021, m_all$plotday+1, m_all$plotday)

# Remove unnecessary columns
# m_all <- m_all %>% dplyr::select(-lon,-lat,-u,-v,-datetime,-wind_dir,-bird_dir,-bird_vel)


# Split data between species
m_BBAL <- m_all %>% filter(spp=="BBAL")
m_GHAL <- m_all %>% filter(spp=="GHAL")
m_LAAL <- m_all %>% filter(spp=="LAAL")
m_BFAL <- m_all %>% filter(spp=="BFAL")



# Flap plots --------------------------------------------------------------

################################################################################

m_BBAL_filtered <- m_BBAL

# TRY CHANGING K

# GAMM_BBAL <- gamm4(formula = flaps ~ s(wind_vel,k=5), 
#                     random = ~(1|id),
#                     data = m_BBAL_filtered,
#                     family = poisson)
# 
# preds <- predict(GAMM_BBAL$gam,m_BBAL_filtered %>% select(wind_vel,bwa))
# preds_df <- data.frame(cbind(m_BBAL_filtered,preds))
# preds_df <- preds_df %>% mutate(est_trans = exp(preds))

# base model with individual randomness
GAM_BBAL_1 <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') + s(wind_vel,id,bs='re',k=5),
                    data = m_BBAL_filtered,
                    family = poisson(),
                    method = "REML")

summary(GAM_BBAL_1)
AIC(GAM_BBAL_1)

# Create dataframe to use predict() 
predict_df_base <- expand.grid(wind_vel=seq(min(m_BBAL_filtered$wind_vel),max(m_BBAL_filtered$wind_vel),length.out=100),
                          id = unique(m_BBAL_filtered$id)[1:10]
                          )
preds_base <- predict(GAM_BBAL_1,newdata=predict_df_base,type="terms",se.fit=TRUE)
preds_df_base <- data.frame(cbind(predict_df_base,preds_base$fit,preds_base$se.fit))
preds_df_base <- preds_df_base %>% mutate(global = exp(s.wind_vel. + attr(preds_base,"constant")[[1]]),
                                individual = exp(s.wind_vel. + s.wind_vel.id. + attr(preds_base,"constant")[[1]]),
                                global_up95 = exp(s.wind_vel. + attr(preds_base,"constant")[[1]] + 1.96*s.wind_vel..1),
                                global_low95 = exp(s.wind_vel. + attr(preds_base,"constant")[[1]] - 1.96*s.wind_vel..1)
                                )

# Plot individual smooths along with global smooth
ggplot(preds_df_base, aes(wind_vel,individual,color=id)) + 
  geom_line() +
  geom_line(data=preds_df_base, aes(wind_vel,global), color="black") +
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# Plot global smooth with 95 confidence intervals
ggplot(preds_df_base, aes(wind_vel,global)) + 
  geom_line() +
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_ribbon(data=preds_df_base,aes(ymin=global_low95,ymax=global_up95),alpha=0.2) +
  ggtitle("all BBAL")

# ti model with individual randomness
GAM_BBAL_2 <- gam(formula = flaps ~ ti(wind_vel,k=3,bs='tp') + 
                  ti(bwa,k=4,bs='tp') + 
                  ti(wind_vel,bwa,k=c(3, 4),bs=c('tp', 'tp')) + 
                  s(wind_vel,id,bs='re',k=5),
                data = m_BBAL_filtered,
                family = poisson(),
                method = "REML")

# s and ti model with individual randomness
GAM_BBAL_3 <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') + 
                  s(bwa,k=3,bs='tp') + 
                  ti(wind_vel,bwa,k=c(5, 3),bs=c('tp', 'tp')) + 
                  s(wind_vel,id,bs='re'),
                data = m_BBAL_filtered,
                family = poisson(),
                method = "REML")

# te model with individual randomness
GAM_BBAL_4 <- gam(formula = flaps ~ te(wind_vel,bwa,k=c(5, 5),bs=c('tp', 'tp')) + 
                    s(wind_vel,id,bs='re'),
                  data = m_BBAL_filtered,
                  family = poisson(),
                  method = "REML")

m_BBAL_filtered <- m_BBAL[1:990,]

# te model with individual randomness on the te term
GAM_BBAL_5 <- gam(formula = flaps ~ te(wind_vel,bwa,k=c(5,5),bs=c('tp','tp'),m=2) +
                    t2(wind_vel,bwa,id,k=c(5,5,length(unique(m_BBAL_filtered$id))),bs=c('tp','tp','re'),m=1,full=TRUE),
                  data = m_BBAL_filtered,
                  family = poisson(),
                  method = "REML")

GAM_BBAL_6 <- gam(formula = flaps ~ te(wind_vel,bwa,k=c(5,5),bs=c('tp','tp'),m=2),
                  data = m_BBAL_filtered,
                  family = poisson(),
                  method = "REML")

# Compare AIC scores
# AIC(GAM_BBAL_1,GAM_BBAL_2,GAM_BBAL_3,GAM_BBAL_4)

# Chooose a GAM_BBAL
GAM_BBAL <- GAM_BBAL_6
summary(GAM_BBAL)
AIC(GAM_BBAL)
plot(GAM_BBAL,scheme=2)

# gratia package geom_contour()

gratia_df <- smooth_estimates(GAM_BBAL)

gratia_global <- gratia_df
# gratia_global <- gratia_df %>% filter(is.na(id))
gratia_global <- gratia_global %>% add_confint()
gratia_global <- gratia_global %>% mutate(pred = exp(est+GAM_BBAL$coefficients[[1]]),
                                          pred_up95 = exp(upper_ci+GAM_BBAL$coefficients[[1]]),
                                          pred_low95 = exp(lower_ci+GAM_BBAL$coefficients[[1]]))

gratia_ind <- gratia_df %>% filter(id %in% unique(m_BBAL_filtered$id)[1:10])
gratia_ind <- merge(gratia_global,gratia_ind,by=c("wind_vel","bwa"))
gratia_ind <- gratia_ind %>% mutate(est = est.x + est.y,
                                          se = se.x + se.y)
gratia_ind_cleaned <- gratia_ind %>% select(id.y,wind_vel,bwa,est,se)
gratia_ind_cleaned <- gratia_ind_cleaned %>% add_confint()
gratia_ind_cleaned <- gratia_ind_cleaned %>% mutate(pred = exp(est+GAM_BBAL$coefficients[[1]]),
                                    pred_up95 = exp(upper_ci+GAM_BBAL$coefficients[[1]]),
                                    pred_low95 = exp(lower_ci+GAM_BBAL$coefficients[[1]]))

# Plot global output
ggplot(gratia_global, aes(wind_vel,bwa,z=est)) +
  geom_contour_filled()

# Plot estimates
ggplot(gratia_global, aes(wind_vel,bwa,z=pred)) +
  geom_contour_filled()

# plot estimates for individual 1
ggplot(gratia_ind_cleaned %>% filter(id.y==as.character(unique(m_BBAL_filtered$id)[1])), aes(wind_vel,bwa,z=est)) +
  geom_contour_filled()

# Plot individual smooths along with global smooth
ggplot(gratia_ind_cleaned, aes(wind_vel,pred,group=interaction(id,bwa),color=id)) + 
  geom_line() +
  geom_line(data=gratia_global, aes(wind_vel,exp.est), color="black") # +
  # geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# Plot global smooth with 95 confidence intervals
ggplot(gratia_global, aes(wind_vel,pred,group=bwa)) + 
  geom_line() +
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) # +
  # geom_ribbon(data=gratia_global,aes(ymin=pred_low95,ymax=pred_up95),alpha=0.2)



# Create dataframe to use predict() 
predict_df <- expand.grid(wind_vel=seq(min(m_BBAL_filtered$wind_vel),max(m_BBAL_filtered$wind_vel),length.out=100),
           id = unique(m_BBAL_filtered$id)[1:10],
           bwa=seq(min(m_BBAL_filtered$bwa),max(m_BBAL_filtered$bwa),length.out=10)
           )
preds <- predict(GAM_BBAL,newdata=predict_df,type="terms",se.fit=TRUE)
preds_df <- data.frame(cbind(predict_df,preds$fit,preds$se.fit))
# preds_df <- preds_df %>% mutate(global = exp(s.wind_vel. + attr(preds,"constant")[[1]]),
#                                 individual = exp(s.wind_vel. + s.wind_vel.id. + attr(preds,"constant")[[1]]))
preds_df <- preds_df %>% mutate(global = exp(te.wind_vel.bwa. + attr(preds,"constant")[[1]]),
                                individual = exp(te.wind_vel.bwa. + s.wind_vel.id. + attr(preds,"constant")[[1]]),
                                global_up95 = exp(te.wind_vel.bwa. + attr(preds,"constant")[[1]] + 1.96*te.wind_vel.bwa..1),
                                global_low95 = exp(te.wind_vel.bwa. + attr(preds,"constant")[[1]] - 1.96*te.wind_vel.bwa..1)
)

# Plot individual smooths along with global smooth
ggplot(preds_df, aes(wind_vel,individual,group=interaction(id,bwa),color=id)) + 
  geom_line() +
  geom_line(data=preds_df, aes(wind_vel,global), color="black") +
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# Plot global smooth with 95 confidence intervals
ggplot(preds_df, aes(wind_vel,global,group=bwa)) + 
  geom_line() +
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_ribbon(data=preds_df,aes(ymin=global_low95,ymax=global_up95),alpha=0.2) +
  ggtitle("headwinds")

# smooth for individual 1
ggplot(preds_df %>% filter(id == as.character(unique(m_BBAL_filtered$id)[1])), aes(wind_vel,individual,group=interaction(id,bwa))) + 
  geom_line(color='blue') +
  geom_line(data=preds_df, aes(wind_vel,global,group=bwa), color="black") +
  geom_point(m_BBAL_filtered %>% filter(id == as.character(unique(m_BBAL_filtered$id)[1])), mapping = aes(wind_vel,flaps),color='red') + 
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# smooth for individual 2
ggplot(preds_df %>% filter(id == as.character(unique(m_BBAL_filtered$id)[2])), aes(wind_vel,individual,group=interaction(id,bwa))) + 
  geom_line(color='blue') +
  geom_line(data=preds_df, aes(wind_vel,global,group=bwa), color="black") +
  geom_point(m_BBAL_filtered %>% filter(id == as.character(unique(m_BBAL_filtered$id)[2])), mapping = aes(wind_vel,flaps),color='red') + 
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# smooth for individual 3
ggplot(preds_df %>% filter(id == as.character(unique(m_BBAL_filtered$id)[3])), aes(wind_vel,individual,group=interaction(id,bwa))) + 
  geom_line(color='blue') +
  geom_line(data=preds_df, aes(wind_vel,global,group=bwa), color="black") +
  geom_point(m_BBAL_filtered %>% filter(id == as.character(unique(m_BBAL_filtered$id)[3])), mapping = aes(wind_vel,flaps),color='red') + 
  geom_point(m_BBAL_filtered,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

################################################################################
# Breaking up data into head, cross, and tail-winds

m_BBAL_head <- m_BBAL_filtered %>% filter(bwa<=45) # only head-winds
m_BBAL_cross <- m_BBAL_filtered %>% filter(bwa>45 & bwa<135) # only cross-winds
m_BBAL_tail <- m_BBAL_filtered %>% filter(bwa>=135) # only tail-winds

# base model with individual randomness for headwinds
GAM_BBAL_head <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') + s(wind_vel,id,bs='re',k=5),
                  data = m_BBAL_head,
                  family = poisson(),
                  method = "REML")

# Create dataframe to use predict() for headwinds
predict_df_head <- expand.grid(wind_vel=seq(min(m_BBAL_head$wind_vel),max(m_BBAL_head$wind_vel),length.out=100),
                               id = unique(m_BBAL_head$id)[1:10])
preds_head <- predict(GAM_BBAL_head,newdata=predict_df_head,type="terms",se.fit=TRUE)
preds_df_head <- data.frame(cbind(predict_df_head,preds_head$fit,preds_head$se.fit))
preds_df_head <- preds_df_head %>% mutate(global = exp(s.wind_vel. + attr(preds_head,"constant")[[1]]),
                                          individual = exp(s.wind_vel. + s.wind_vel.id. + attr(preds_head,"constant")[[1]]),
                                          global_up95 = exp(s.wind_vel. + attr(preds_head,"constant")[[1]] + 1.96*s.wind_vel..1),
                                          global_low95 = exp(s.wind_vel. + attr(preds_head,"constant")[[1]] - 1.96*s.wind_vel..1)
)

# Plot individual smooths along with global smooth for headwinds
ggplot(preds_df_head, aes(wind_vel,individual,color=id)) + 
  geom_point() +
  geom_point(data=preds_df_head, aes(wind_vel,global), color="black") +
  geom_point(m_BBAL_head,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# Plot global smooth with 95 confidence intervals for headwinds
ggplot(preds_df_head, aes(wind_vel,global)) + 
  geom_line() +
  geom_point(m_BBAL_head,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_ribbon(data=preds_df_head,aes(ymin=global_low95,ymax=global_up95),alpha=0.2) +
  ggtitle("headwinds")

# base model with individual randomness for crosswinds
GAM_BBAL_cross <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') + s(wind_vel,id,bs='re',k=5),
                     data = m_BBAL_cross,
                     family = poisson(),
                     method = "REML")

# Create dataframe to use predict() for crosswinds
predict_df_cross <- expand.grid(wind_vel=seq(min(m_BBAL_cross$wind_vel),max(m_BBAL_cross$wind_vel),length.out=100),
                               id = unique(m_BBAL_cross$id)[1:10])
preds_cross <- predict(GAM_BBAL_cross,newdata=predict_df_cross,type="terms",se.fit=TRUE)
preds_df_cross <- data.frame(cbind(predict_df_cross,preds_cross$fit,preds_cross$se.fit))
preds_df_cross <- preds_df_cross %>% mutate(global = exp(s.wind_vel. + attr(preds_cross,"constant")[[1]]),
                                          individual = exp(s.wind_vel. + s.wind_vel.id. + attr(preds_cross,"constant")[[1]]),
                                          global_up95 = exp(s.wind_vel. + attr(preds_cross,"constant")[[1]] + 1.96*s.wind_vel..1),
                                          global_low95 = exp(s.wind_vel. + attr(preds_cross,"constant")[[1]] - 1.96*s.wind_vel..1)
)

# Plot individual smooths along with global smooth for crosswinds
ggplot(preds_df_cross, aes(wind_vel,individual,color=id)) + 
  geom_point() +
  geom_point(data=preds_df_cross, aes(wind_vel,global), color="black") +
  geom_point(m_BBAL_cross,mapping=aes(wind_vel,flaps),color='black',alpha=0.1)

# Plot global smooth with 95 confidence intervals for crosswinds
ggplot(preds_df_cross, aes(wind_vel,global)) + 
  geom_line() +
  geom_point(m_BBAL_cross,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_ribbon(data=preds_df_cross,aes(ymin=global_low95,ymax=global_up95),alpha=0.2) +
  ggtitle("crosswinds")

# base model with individual randomness for tailwinds
GAM_BBAL_tail <- gam(formula = flaps ~ s(wind_vel,k=5,bs='tp') + s(wind_vel,id,bs='re',k=5),
                     data = m_BBAL_tail,
                     family = poisson(),
                     method = "REML")

# Create dataframe to use predict() for tailwinds
predict_df_tail <- expand.grid(wind_vel=seq(min(m_BBAL_tail$wind_vel),max(m_BBAL_tail$wind_vel),length.out=100),
                               id = unique(m_BBAL_tail$id)[1:10])
preds_tail <- predict(GAM_BBAL_tail,newdata=predict_df_tail,type="terms",se.fit=TRUE)
preds_df_tail <- data.frame(cbind(predict_df_tail,preds_tail$fit,preds_tail$se.fit))
preds_df_tail <- preds_df_tail %>% mutate(global = exp(s.wind_vel. + attr(preds_tail,"constant")[[1]]),
                                          individual = exp(s.wind_vel. + s.wind_vel.id. + attr(preds_tail,"constant")[[1]]),
                                          global_up95 = exp(s.wind_vel. + attr(preds_tail,"constant")[[1]] + 1.96*s.wind_vel..1),
                                          global_low95 = exp(s.wind_vel. + attr(preds_tail,"constant")[[1]] - 1.96*s.wind_vel..1)
                                          )


# Plot individual smooths along with global smooth for tailwinds
ggplot(preds_df_tail, aes(wind_vel,individual,color=id)) + 
  geom_point() +
  geom_point(data=preds_df_tail, aes(wind_vel,global), color="black") +
  geom_point(m_BBAL_tail,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  ggtitle("Tailwinds")

# Plot global smooth with 95 confidence intervals for tailwinds
ggplot(preds_df_tail, aes(wind_vel,global)) + 
  geom_line() +
  geom_point(m_BBAL_tail,mapping=aes(wind_vel,flaps),color='black',alpha=0.1) +
  geom_ribbon(aes(ymin=global_low95,ymax=global_up95),alpha=0.2) +
  ggtitle("Tailwinds")

plot(GAM_BBAL_tail,pages=1)
################################################################################
# Messing with a subset of data



################################################################################
# For BBAL
fiveP_BBAL <- quantile(m_BBAL$wind_vel,probs=(c(.05,.95)))
m_BBAL_filtered <- m_BBAL
# m_BBAL_filtered <- m_BBAL %>% filter(wind_vel>fiveP_BBAL[[1]] & wind_vel<fiveP_BBAL[[2]])
# m_BBAL_filtered <- m_BBAL_filtered %>% filter(bwa<=45) # only head-winds
# m_BBAL_filtered <- m_BBAL_filtered %>% filter(bwa>45 & bwa<135) # only cross-winds
# m_BBAL_filtered <- m_BBAL_filtered %>% filter(bwa>=135) # only tail-winds
ggplot(m_BBAL_filtered, aes(wind_vel,flaps)) + 
  geom_point(color='black')

GAM_BBAL <- gam(formula = flaps ~ s(wind_vel,k=4,bs='tp') + s(bwa,bs='tp',k=4) + te(wind_vel, bwa, k = c(4, 4), bs = c('tp', 'tp')),
                data = m_BBAL_filtered,
                family = poisson(),
                method = "REML")

preds <- predict.gam(GAM_BBAL,m_BBAL_filtered %>% select(wind_vel,bwa))
preds_df <- data.frame(cbind(m_BBAL_filtered,preds))
preds_df <- preds_df %>% mutate(est_trans = exp(preds))

ggplot(m_BBAL_filtered, aes(wind_vel,flaps)) +
  geom_point(color='red') +
  geom_point(data=preds_df, aes(wind_vel,est_trans), color="black")

summary(GAM_BBAL)

plot(GAM_BBAL,scheme=2,pages=1)

################################################################################
# For GHAL
fiveP_GHAL <- quantile(m_GHAL$wind_vel,probs=(c(.05,.95)))
m_GHAL_filtered <- m_GHAL
m_GHAL_filtered <- m_GHAL %>% filter(wind_vel>fiveP_GHAL[[1]] & wind_vel<fiveP_GHAL[[2]])
# m_GHAL_filtered <- m_GHAL_filtered %>% filter(bwa<=45) # only head-winds
# m_GHAL_filtered <- m_GHAL_filtered %>% filter(bwa>45 & bwa<135) # only cross-winds
# m_GHAL_filtered <- m_GHAL_filtered %>% filter(bwa>=135) # only tail-winds
ggplot(m_GHAL_filtered, aes(wind_vel,flaps)) + 
  geom_point(color='black')

GAM_GHAL <- gam(formula = flaps ~ te(wind_vel, bwa, k = c(5, 5), bs = c('tp', 'tp')),
                 data = m_GHAL_filtered,
                 family = poisson(),
                 method = "REML")

preds <- predict.gam(GAM_GHAL,m_GHAL_filtered %>% select(wind_vel,bwa))
preds_df <- data.frame(cbind(m_GHAL_filtered,preds))
preds_df <- preds_df %>% mutate(est_trans = exp(preds))

ggplot(m_GHAL_filtered, aes(wind_vel,flaps)) +
  geom_point(color='red') +
  geom_point(data=preds_df, aes(wind_vel,est_trans), color="black")

summary(GAM_GHAL)

################################################################################
# For BFAL
fiveP_BFAL <- quantile(m_BFAL$wind_vel,probs=(c(.05,.95)))
m_BFAL_filtered <- m_BFAL
m_BFAL_filtered <- m_BFAL %>% filter(wind_vel>fiveP_BFAL[[1]] & wind_vel<fiveP_BFAL[[2]])
# m_BFAL_filtered <- m_BFAL_filtered %>% filter(bwa<=45) # only head-winds
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
# m_LAAL_filtered <- m_LAAL_filtered %>% filter(bwa<=45) # only head-winds
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
# Facet wrap by species, split bwa into head, cross, and tail:







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
#   labs(title = "GAM + scatter plots split by spp (tail-winds)", x="Wind Velocity", y="Flaps per hour")
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




# this gets rid of headwinds (setting bwa>60)
m_selectedWinds <- filter(m_all,bwa>60)

# do i separate into inc and BG? 

lme1<-lmer(flaps~wind_vel+(1+wind_vel|spp)+(1+wind_vel|id),data=m_selectedWinds)

# Ok, so including both spp and id leads to singularity
# lme1<-lmer(flaps~wind_vel + (1|spp) + (1|id),data=m_selectedWinds)

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






