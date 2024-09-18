library(bbmle)

# run all GAMs -----------------------------------------------------------------

fac_k <- 3
pois_GAMs <- list()
nbinom_GAMs <- list()

# POISSON IS WORSE - USE NEGATIVE BINOMIAL DISTRIBUTION
# for (dist in c("poisson","nb")) {
for (dist in c("nb")) {
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
      
    } else if (dist == "nb") {
      
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
  
  spp_pois_AICs <- AICctab(pois_GAMs[[paste0(spp,"_0")]], pois_GAMs[[paste0(spp,"_1")]], 
          pois_GAMs[[paste0(spp,"_2")]], pois_GAMs[[paste0(spp,"_3")]],
          pois_GAMs[[paste0(spp,"_4")]], pois_GAMs[[paste0(spp,"_5")]],
          pois_GAMs[[paste0(spp,"_s1")]], pois_GAMs[[paste0(spp,"_s2")]],
          weights = TRUE, delta = TRUE, base=TRUE, sort = FALSE)
  
  spp_nbinom_AICs <- AICctab(nbinom_GAMs[[paste0(spp,"_0")]], nbinom_GAMs[[paste0(spp,"_1")]], 
                           nbinom_GAMs[[paste0(spp,"_2")]], nbinom_GAMs[[paste0(spp,"_3")]],
                           nbinom_GAMs[[paste0(spp,"_4")]], nbinom_GAMs[[paste0(spp,"_5")]],
                           nbinom_GAMs[[paste0(spp,"_s1")]], nbinom_GAMs[[paste0(spp,"_s2")]],
                           weights = TRUE, delta = TRUE, base=TRUE, sort = FALSE)
  
  spp_pois_r.sq <- c()
  spp_pois_dev.expl <- c()
  
  spp_nbinom_r.sq <- c()
  spp_nbinom_dev.expl <- c()
  
  for (model in c(0:5,"s1","s2")) {
    
    spp_pois_r.sq <- c(spp_pois_r.sq,summary(pois_GAMs[[paste0(spp,"_",model)]])$r.sq)
    spp_pois_dev.expl <- c(spp_pois_dev.expl,summary(pois_GAMs[[paste0(spp,"_",model)]])$dev.expl)
    
    spp_nbinom_r.sq <- c(spp_nbinom_r.sq,summary(nbinom_GAMs[[paste0(spp,"_",model)]])$r.sq)
    spp_nbinom_dev.expl <- c(spp_nbinom_dev.expl,summary(nbinom_GAMs[[paste0(spp,"_",model)]])$dev.expl)
    
  }
  
  spp_pois_metrics <- as.data.frame(spp_pois_AICs)
  spp_pois_metrics$r.sq <- spp_pois_r.sq
  spp_pois_metrics$dev.expl <- spp_pois_dev.expl
  
  spp_nbinom_metrics <- as.data.frame(spp_nbinom_AICs)
  spp_nbinom_metrics$r.sq <- spp_nbinom_r.sq
  spp_nbinom_metrics$dev.expl <- spp_nbinom_dev.expl
  
}

spp_pois_metrics
spp_nbinom_metrics

################################################################################
# Testing variance inflation to test poisson distribution 

library("fitdistrplus")

descdist(m_current$flaps, discrete = TRUE)

fit.nbinom <- fitdist(m_current$flaps, "nbinom")
fit.pois <- fitdist(m_current$flaps, "pois")

fit.nbinom$aic
fit.pois$aic

################################################################################
# Plotting all models

# unidirectional
for (spp in spp_vec) {
  
  spp_pois_AICs <- AICctab(pois_GAMs[[paste0(spp,"_0")]]

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
  fv_df_shts <- link_df
} else {
  fv_df_shts <- rbind(fv_df_shts,link_df)
}
