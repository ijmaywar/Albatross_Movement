################################################################################
#
# GAMs with continuous relative climate variable direction
#
################################################################################

# te(wind_vel_kmh,bird_wind_angle), s(id) --------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_angle <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,bird_wind_angle)
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,bird_wind_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_wind_vel_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            bird_wind_angle = evenly(bird_wind_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,bird_wind_angle)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","bird_wind_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_wind_vel_angle <- link_df
  } else {
    fv_df_te_wind_vel_angle <- rbind(fv_df_te_wind_vel_angle,link_df)
  }
}

fv_df_te_wind_vel_angle$Species <- factor(fv_df_te_wind_vel_angle$Species , 
                                          levels=spp_vec)

fv_df_te_wind_vel_angle_trim <- distinct(fv_df_te_wind_vel_angle %>% 
                                           dplyr::select(wind_vel_kmh,bird_wind_angle,fitted_global,Species))

# for (i in 1:5) {
#   summary_i <- summary(GAM_list_te_wind_vel_angle[[i]])
#   summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
#   summary_i_df$Species <- spp_vec[i]
#   if (i==1) {
#     summary_df <- summary_i_df
#   } else {
#     summary_df <- rbind(summary_df,summary_i_df)
#   }
# }
# 
# summary_df

# Reds for winds
reds <- colorRampPalette(c("red4", "#FCFDBFFF"))
fig_wind_cont <- ggplot(fv_df_te_wind_vel_angle_trim) +
  geom_contour_filled(aes(wind_vel_kmh,bird_wind_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_wind_vel_angle_trim$fitted_global),11)) +
  scale_fill_manual(values=reds(10),drop=FALSE) +
  labs(y="Bird-wind-angle (degrees)") +
  labs(fill = "Flaps/hour") +
  scale_y_continuous(breaks=seq(0,180,60)) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_cont


# te(shts,bird_swell_angle), s(id) ---------------------------------------------

fac_k <- 3
GAM_list_te_shts_angle <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(shts,bird_swell_angle)
  
  current_GAM <- gam(formula = flaps ~ te(shts,bird_swell_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_shts_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, shts = evenly(shts, n = 100),
                            bird_swell_angle = evenly(bird_swell_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(shts,bird_swell_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(shts,bird_swell_angle)"))[,4:7])
  
  colnames(link_df) <- c("shts","bird_swell_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_shts_angle <- link_df
  } else {
    fv_df_te_shts_angle <- rbind(fv_df_te_shts_angle,link_df)
  }
}

fv_df_te_shts_angle$Species <- factor(fv_df_te_shts_angle$Species, 
                                      levels=spp_vec)

fv_df_te_shts_angle_trim <- distinct(fv_df_te_shts_angle %>% 
                                       dplyr::select(shts,bird_swell_angle,fitted_global,Species))

# for (i in 1:5) {
#   summary_i <- summary(GAM_list_te_shts_angle[[i]])
#   summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
#   summary_i_df$Species <- spp_vec[i]
#   if (i==1) {
#     summary_df <- summary_i_df
#   } else {
#     summary_df <- rbind(summary_df,summary_i_df)
#   }
# }
# 
# summary_df

# Blues for swells
blues <- colorRampPalette(c("navy", "#FCFDBFFF"))
fig_swells_cont <- ggplot(fv_df_te_shts_angle_trim) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),11)) +
  scale_fill_manual(values=blues(10),drop=FALSE) +
  labs(y="Bird-swell-angle (degrees)") +
  labs(fill = "Flaps/hour") +
  scale_y_continuous(breaks=seq(0,180,60)) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swells_cont

# Legend, separately
legend <- cowplot::get_legend(ggplot(fv_df_te_shts_angle_trim) +
            geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
              breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),11)) +
            scale_fill_manual(values=blues(10),drop=FALSE) +
            labs(fill = "Flaps/hour") +
            facet_wrap(~Species,nrow=1))

grid.newpage()
grid.draw(legend)

