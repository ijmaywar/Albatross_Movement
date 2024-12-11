################################################################################
#
# Downsampling GAMs with no relative climate variable direction component
#
################################################################################

# s(wind_vel_kmh), s(id) --------------------------------------

fac_k <- 3
BIG_GAM_list_wind_vel <- list()

for (iter in 1:101) {

GAM_list_wind_vel <- list()

  for (spp in spp_vec) {
    
    # the 101st iter. is not downsampled 
    if (iter==101) {
      m_current <- m_model %>% filter(Species == spp)
    } else {
      # There are 18 black-footed albatross for m_model
      #   13 BGs and 5 Incs
      m_current <- rbind((m_model %>% filter(Species == spp, Trip_Type=="BG") %>% 
                         filter(id %in% sample(unique(id), 12))),
                         (m_model %>% filter(Species == spp, Trip_Type=="Inc") %>% 
                            filter(id %in% sample(unique(id), 6))))
    }
      
    current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = "nb",
                       method = "REML")
    
    GAM_list_wind_vel[[spp]] <- current_GAM
    
    current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                              id = unique(m_current$id)[1:10])
    
    link_df <- cbind(current_ds,
                     rep(spp,nrow(current_ds)),rep(iter,nrow(current_ds)),
                     fitted_values(current_GAM, data = current_ds, scale = "link",
                                   terms = c("(Intercept)","s(wind_vel_kmh)"))[,3:6])
    
    colnames(link_df) <- c("wind_vel_kmh","id","Species","iteration",
                           "fitted_global","se_global","lower_global","upper_global")
    
    if (spp == "Black-browed" && iter == 1) {
      fv_df_wind_vel <- link_df
    } else {
      fv_df_wind_vel <- rbind(fv_df_wind_vel,link_df)
    }
  }
  
  BIG_GAM_list_wind_vel[[iter]] <- GAM_list_wind_vel

}

fv_df_wind_vel$Species <- factor(fv_df_wind_vel$Species, levels=spp_vec)
fv_df_wind_vel$iter <- factor(fv_df_wind_vel$iter)

fig_wind_simple <- ggplot() +
  geom_line(fv_df_wind_vel %>% filter(iter %in% 1:100),mapping=aes(wind_vel_kmh,exp(fitted_global),group=iter),
            alpha=0.1) +
  geom_line(fv_df_wind_vel %>% filter(iter==101),mapping=aes(wind_vel_kmh,exp(fitted_global))) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  ylim(0,2500) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_simple

# s(shts), s(id) --------------------------------------

fac_k <- 3
BIG_GAM_list_shts <- list()

for (iter in 1:101) {
  
  GAM_list_shts <- list()
  
  for (spp in spp_vec) {
    
    # the 101st iter. is not downsampled 
    if (iter==101) {
      m_current <- m_model %>% filter(Species == spp)
    } else {
      # There are 18 black-footed albatross for m_all
      m_current <- rbind((m_model %>% filter(Species == spp, Trip_Type=="BG") %>% 
                            filter(id %in% sample(unique(id), 12))),
                         (m_model %>% filter(Species == spp, Trip_Type=="Inc") %>% 
                            filter(id %in% sample(unique(id), 6))))
    }
    
    current_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
                         s(id,k=length(unique(m_current$id)),bs="re"),
                       data = m_current,
                       family = "nb",
                       method = "REML")
    
    GAM_list_shts[[spp]] <- current_GAM
    
    current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                              id = unique(m_current$id)[1:10])
    
    link_df <- cbind(current_ds,
                     rep(spp,nrow(current_ds)),rep(iter,nrow(current_ds)),
                     fitted_values(current_GAM, data = current_ds, scale = "link",
                                   terms = c("(Intercept)","s(shts)"))[,3:6])
    
    colnames(link_df) <- c("shts","id","Species","iteration",
                           "fitted_global","se_global","lower_global","upper_global")
    
    if (spp == "Black-browed" && iter == 1) {
      fv_df_shts <- link_df
    } else {
      fv_df_shts <- rbind(fv_df_shts,link_df)
    }
  }
  
  BIG_GAM_list_shts[[iter]] <- GAM_list_shts
  
}

fv_df_shts$Species <- factor(fv_df_shts$Species, levels=spp_vec)
fv_df_shts$iter <- factor(fv_df_shts$iter)

fig_shts_simple <- ggplot() +
  geom_line(fv_df_shts %>% filter(iter %in% 1:100),mapping=aes(shts,exp(fitted_global),group=iter),
            alpha=0.1) +
  geom_line(fv_df_shts %>% filter(iter==101),mapping=aes(shts,exp(fitted_global))) +
  labs(y="Flaps/hour",
       x="Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  ylim(0,2500) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_simple

# Wrap elements ----------------------------------------------------------------

# Use the tag label as an x-axis label
fig_wind_simple / fig_shts_simple

# 800 x 500

