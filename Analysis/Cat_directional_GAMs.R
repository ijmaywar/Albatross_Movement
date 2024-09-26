################################################################################
#
# GAMs with categorical relative climate variable direction
#
################################################################################

# s(wind_vel_kmh,bird_wind_angle_cat), s(id) ----------------------------------------------

GAM_list_wind_vel_kmh_cat <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(flaps,wind_vel_kmh,shts,bird_wind_angle,bird_swell_angle,bird_wind_angle_cat,bird_swell_angle_cat)
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,bs='tp',k=3) +
                       bird_wind_angle_cat + 
                       s(wind_vel_kmh,by=bird_wind_angle_cat,bs='tp',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "nb",
                     method = "REML")
  
  GAM_list_wind_vel_kmh_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bird_wind_angle_cat = unique(m_current$bird_wind_angle_cat))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)",smooths(current_GAM)[1:4]))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","id","bird_wind_angle_cat","Species",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_wind_vel_kmh_cat <- link_df
  } else {
    fv_df_wind_vel_kmh_cat <- rbind(fv_df_wind_vel_kmh_cat,link_df)
  }
}

fv_df_wind_vel_kmh_cat$Species <- factor(fv_df_wind_vel_kmh_cat$Species,
                                         levels=spp_vec)
fv_df_wind_vel_kmh_cat$bird_wind_angle_cat

dir_cat_cols <- c("#9484B1FF", "#F1C100FF","#496849FF")
fig_wind_cat <- ggplot(fv_df_wind_vel_kmh_cat) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global),color=bird_wind_angle_cat)) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wind_angle_cat,fill=bird_wind_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title = "Wind direction category",
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

fig_wind_cat


# s(shts,bird_swell_angle_cat), s(id) ------------------------------------------

GAM_list_shts_cat <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(flaps,wind_vel_kmh,shts,bird_wind_angle,bird_swell_angle,bird_wind_angle_cat,bird_swell_angle_cat)
  
  current_GAM <- gam(formula = flaps ~ s(shts,bs='tp',k=3) +
                       bird_swell_angle_cat + 
                       s(shts,by=bird_swell_angle_cat,bs='tp',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "nb",
                     method = "REML")
  
  GAM_list_shts_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bird_swell_angle_cat = unique(m_current$bird_swell_angle_cat)[1:3])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)",smooths(current_GAM)[1:4]))[,4:7])
  
  colnames(link_df) <- c("shts","id","bird_swell_angle_cat","Species",
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
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_swell_angle_cat,fill=bird_swell_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title = "Swell direction category",
                            override.aes = list(fill = dir_cat_cols)),
         fill="none") +
  scale_color_manual(values=dir_cat_cols,
                     labels=c("Against","Across","With")) + 
  scale_fill_manual(values=dir_cat_cols,
                    labels=c("Against","Across","With")) + 
  labs(y="Flaps/hour") +
  # xlim(0,25) + 
  ylim(0,2500) +
  facet_wrap(~Species,nrow = 1) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swells_cat

wrap_elements(panel = fig_wind_cat / fig_swells_cat)
# 1000 x 500

# summaries
for (spp in spp_vec) {
  
  spp_AICs <- AICctab(GAM_list_wind_vel_kmh_cat[[spp]],GAM_list_shts_cat[[spp]],
                          weights = TRUE, delta = TRUE, base=TRUE, sort = FALSE)
  
  spp_r.sq <- c(summary(GAM_list_wind_vel_kmh_cat[[spp]])$r.sq,summary(GAM_list_shts_cat[[spp]])$r.sq)
  spp_dev.expl <- c(summary(GAM_list_wind_vel_kmh_cat[[spp]])$dev.expl,summary(GAM_list_shts_cat[[spp]])$dev.expl)
  
  spp_metrics <- as.data.frame(spp_AICs)
  spp_metrics$r.sq <- spp_r.sq
  spp_metrics$dev.expl <- spp_dev.expl
  spp_metrics$spp <- spp
  
  if (spp == "Black-browed") {
    All_metrics <- spp_metrics
  } else {
    All_metrics <- rbind(All_metrics,spp_metrics)
  }
  
}

All_metrics

clipr::write_clip(All_metrics)



