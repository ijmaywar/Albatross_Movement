library(bbmle)

# run all GAMs -----------------------------------------------------------------

fac_k <- 3
pois_GAMs <- list()
nbinom_GAMs <- list()

for (dist in c("poisson","negbin")) {
  for (spp in spp_vec) {
    
    m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
      drop_na(flaps,wind_vel_kmh,shts,bird_wind_angle,bird_swell_angle,bird_wind_angle_cat,bird_swell_angle_cat)
    
    null_GAM <- gam(formula = flaps ~ s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    uni_wind_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    uni_swell_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    cat_wind_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,bird_wind_angle_cat,bs='fs',k=fac_k) +
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    cat_swell_GAM <- gam(formula = flaps ~ s(shts,bird_swell_angle_cat,bs='fs',k=fac_k) +
                           s(id,k=length(unique(m_current$id)),bs="re"),
                         data = m_current,
                         family = dist,
                         method = "REML")
    
    cont_wind_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bird_wind_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    cont_swell_GAM <- gam(formula = flaps ~ te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    te_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = dist,
                       method = "REML")
    
    if (dist == "poisson") {
      
      pois_GAMs[[paste0(spp,"_0")]] <- null_GAM
      pois_GAMs[[paste0(spp,"_1")]] <- uni_wind_GAM
      pois_GAMs[[paste0(spp,"_2")]] <- cont_wind_GAM
      pois_GAMs[[paste0(spp,"_3")]] <- uni_swell_GAM
      pois_GAMs[[paste0(spp,"_4")]] <- cont_swell_GAM
      pois_GAMs[[paste0(spp,"_5")]] <- te_GAM
      pois_GAMs[[paste0(spp,"_s1")]] <- cat_wind_GAM
      pois_GAMs[[paste0(spp,"_s2")]] <- cat_swell_GAM
      
    } else if (dist == "negbin") {
      
      nbinom_GAMs[[paste0(spp,"_0")]] <- null_GAM
      nbinom_GAMs[[paste0(spp,"_1")]] <- uni_wind_GAM
      nbinom_GAMs[[paste0(spp,"_2")]] <- cont_wind_GAM
      nbinom_GAMs[[paste0(spp,"_3")]] <- uni_swell_GAM
      nbinom_GAMs[[paste0(spp,"_4")]] <- cont_swell_GAM
      nbinom_GAMs[[paste0(spp,"_5")]] <- te_GAM
      nbinom_GAMs[[paste0(spp,"_s1")]] <- cat_wind_GAM
      nbinom_GAMs[[paste0(spp,"_s2")]] <- cat_swell_GAM
      
    }
  }
}

# Find metrics -----------------------------------------------------------------

for (spp in spp_vec) {
  spp_AICs <- AICctab(All_GAMs[[paste0(spp,"_0")]], All_GAMs[[paste0(spp,"_1")]], 
          All_GAMs[[paste0(spp,"_2")]], All_GAMs[[paste0(spp,"_3")]],
          All_GAMs[[paste0(spp,"_4")]], All_GAMs[[paste0(spp,"_5")]],
          All_GAMs[[paste0(spp,"_s1")]], All_GAMs[[paste0(spp,"_s2")]],
          weights = TRUE, delta = TRUE, base=TRUE, sort = FALSE)
  
  spp_r.sq <- c()
  spp_dev.expl <- c()
  for (model in c(0:5,"s1","s2")) {
    spp_r.sq <- c(spp_r.sq,summary(All_GAMs[[paste0(spp,"_",model)]])$r.sq)
    spp_dev.expl <- c(spp_dev.expl,summary(All_GAMs[[paste0(spp,"_",model)]])$dev.expl)
  }
  
  # Keep the tiny digits for weight bc they are disspearing when converting to a df.
  spp_metrics <- as.data.frame(spp_AICs)
  spp_metrics$r.sq <- spp_r.sq
  spp_metrics$dev.expl <- spp_dev.expl
}



################################################################################
# Testing variance inflation to test poisson distribution 

library("fitdistrplus")

descdist(m_current$flaps, discrete = TRUE)

fit.nbinom <- fitdist(m_current$flaps, "nbinom")
fit.pois <- fitdist(m_current$flaps, "pois")

fit.nbinom$aic
fit.pois$aic
