################################################################################
#
# GAMs with categorical relative climate variable direction
#
################################################################################

# s(wind_vel_kmh,bird_wind_angle_cat), s(id) ----------------------------------------------

GAM_list_wind_vel_kmh_cat <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,bird_wind_angle_cat)
  
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
  
  colnames(link_df) <- c("wind_vel_kmh","id","bird_wind_angle_cat","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_wind_vel_kmh_cat <- link_df
  } else {
    fv_df_wind_vel_kmh_cat <- rbind(fv_df_wind_vel_kmh_cat,link_df)
  }
}

fv_df_wind_vel_kmh_cat$Species <- factor(fv_df_wind_vel_kmh_cat$Species,
                                         levels=spp_vec)

fig_wind_cat <- ggplot(fv_df_wind_vel_kmh_cat) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global),color=bird_wind_angle_cat)) +
  # geom_line(aes(wind_vel_kmh,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wind_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme(axis.title.x = element_blank())

fig_wind_cat

# s(shts,bird_swell_angle_cat), s(id) ------------------------------------------

GAM_list_shts_cat <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(shts,bird_swell_angle_cat)
  
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

fv_df_shts_cat$Species <- factor(fv_df_shts_cat$Species,
                                 levels=spp_vec)

fig_swells_cat <- ggplot(fv_df_shts_cat) +
  geom_line(aes(shts,exp(fitted_global),color=bird_swell_angle_cat)) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_swell_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative swell angle")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme(axis.title.x = element_blank())

fig_swells_cat

for (i in 1:5) {
  summary_i <- summary(GAM_list_shts_cat[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df
