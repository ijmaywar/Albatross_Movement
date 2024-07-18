# wind angle comparison --------------------------------------------------------
# How does angle change when looking at combined waves vs. wind waves vs. swells 

ggplot(m_all) +
  geom_point(aes(x=mwd,y=mdts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# WHEN WINDSPEEDS ARE LOW SWELL DIRECTION IS COMBINED WIND DIRECTION
ggplot(m_all %>% filter(wind_vel_kmh<25)) +
  geom_point(aes(x=mwd,y=mdts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

# WHEN WINDSPEEDS ARE HIGH COMBINED WIND DIRECTION IS LESS CORRELATED TO 
# SWELL DIRECTION
ggplot(m_all %>% filter(wind_vel_kmh>40)) +
  geom_point(aes(x=mwd,y=mdts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

ggplot(m_all) +
  geom_point(aes(x=mwd,y=mdww),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()

ggplot(m_all) +
  geom_point(aes(x=mdts, y=mdww),size=0.001,alpha=1,color="black") +
  facet_wrap(~Species,nrow = 1) +
  theme_bw()

# te(swh,bird_wave_angle), s(id) -----------------------------------------------

fac_k <- 3
GAM_list_te_swh_angle <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(swh,bird_wave_angle)
  
  current_GAM <- gam(formula = flaps ~ te(swh,bird_wave_angle,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_swh_angle[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, swh = evenly(swh, n = 100),
                            bird_wave_angle = evenly(bird_wave_angle, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(swh,bird_wave_angle)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(swh,bird_wave_angle)"))[,4:7])
  
  colnames(link_df) <- c("swh","bird_wave_angle","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_swh_angle <- link_df
  } else {
    fv_df_te_swh_angle <- rbind(fv_df_te_swh_angle,link_df)
  }
}

fv_df_te_swh_angle$Species <- factor(fv_df_te_swh_angle$Species, 
                                      levels=spp_vec)

# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_te_swh_angle <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_all %>% 
                          filter(HMM_3S_state != 1, Species == spp) %>% 
                          dplyr::select(swh,bird_wave_angle) %>% 
                          drop_na(swh,bird_wave_angle), 
                        compute.cont=TRUE,gridsize = grid_size)
  
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_current <- rast(fv_df_te_swh_angle %>% filter(Species == spp) %>% select(swh, bird_wave_angle, fitted_global), type='xyz')
  response_rast_mask_current <- terra::mask(response_rast_current, contour_99_current_vect)
  response_df_mask_current <- as.data.frame(response_rast_mask_current, xy=T)
  colnames(response_df_mask_current) <- c("swh","bird_wave_angle", "fitted_global")
  
  # Save values for all spp
  response_df_mask_current$Species <- spp
  
  if (spp == "Black-browed") {
    response_df_mask_te_swh_angle <- response_df_mask_current
  } else {
    response_df_mask_te_swh_angle <- rbind(response_df_mask_te_swh_angle,response_df_mask_current)
  }
  
}

response_df_mask_te_swh_angle$Species <- factor(response_df_mask_te_swh_angle$Species, 
                                                levels=spp_vec)

response_df_mask_te_swh_angle <- response_df_mask_te_swh_angle %>% 
  mutate(bird_wave_angle_cat = case_when(bird_wave_angle<60 ~ "tail",
                                         bird_wave_angle>=60 & bird_wave_angle<120 ~ "cross",
                                         bird_wave_angle>=120 ~ "head"))

response_df_mask_te_swh_angle$bird_wave_angle_cat <-
  as.factor(response_df_mask_te_swh_angle$bird_wave_angle_cat)

response_df_mask_te_swh_angle <- response_df_mask_te_swh_angle %>%
  mutate(swh = round(swh, 6))

blues <- colorRampPalette(c("white", "navy"))
fig_waves_cont <- ggplot(response_df_mask_te_swh_angle) +
  geom_contour_filled(aes(swh,bird_wave_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_te_swh_angle$fitted_global),11)) +
  scale_fill_manual(values=blues(10),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  # xlim(0,90) +
  # ylim(0,7.75) +
  xlab("Significant height of combined waves (m)") +
  ylab("Bird-wave-angle (degrees)") +
  facet_wrap(~Species,nrow=1) +
  theme(axis.title.x = element_blank())

fig_waves_cont

# s(swh,bird_wave_angle_cat), s(id) ------------------------------------------

GAM_list_swh_cat <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(swh,bird_wave_angle_cat)
  
  current_GAM <- gam(formula = flaps ~ s(swh,bird_wave_angle_cat,bs='fs',k=3) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_swh_cat[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, swh = evenly(swh, n = 100),
                           id = unique(m_current$id)[1:10],
                           bird_wave_angle_cat = unique(m_current$bird_wave_angle_cat)[1:3])
  
  current_ds <- current_ds %>%
    mutate(swh = round(swh, 6)) %>% 
    semi_join(response_df_mask_te_swh_angle, by=c("swh","bird_wave_angle_cat"))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(swh,bird_wave_angle_cat)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(swh,bird_wave_angle_cat)"))[,4:7])
  
  colnames(link_df) <- c("swh","id","bird_wave_angle_cat","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_swh_cat <- link_df
  } else {
    fv_df_swh_cat <- rbind(fv_df_swh_cat,link_df)
  }
}

fv_df_swh_cat$Species <- factor(fv_df_swh_cat$Species,
                                 levels=spp_vec)

fig_waves_cat <- ggplot(fv_df_swh_cat) +
  geom_line(aes(swh,exp(fitted_global),color=bird_wave_angle_cat)) +
  geom_ribbon(mapping=aes(x=swh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL,color=bird_wave_angle_cat),alpha=0.2) +
  guides(color=guide_legend(title="Relative wave angle")) +
  scale_color_manual(values=c("head" = "#440154FF",
                              "cross" = "#1F968BFF",
                              "tail" = "#FDE725FF")) + 
  labs(y="Flaps/hour") +
  # xlim(0,25) + 
  # ylim(0,1500) +
  facet_wrap(~Species,nrow = 1) + 
  theme(axis.title.x = element_blank())

fig_waves_cat

# Wrap figures for display -----------------------------------------------------
wrap_elements(panel = fig_waves_cont / fig_waves_cat) +
  labs(tag = "Significant height of combined waves (m)") +
  theme(plot.tag.position = "bottom")


# te(wind_vel_kmh,swh), s(id) -------------------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_swh <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,swh)
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,swh,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_wind_vel_swh[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            swh = evenly(swh, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,swh)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,swh)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","swh","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_wind_vel_swh <- link_df
  } else {
    fv_df_te_wind_vel_swh <- rbind(fv_df_te_wind_vel_swh,link_df)
  }
}

fv_df_te_wind_vel_swh$Species <- factor(fv_df_te_wind_vel_swh$Species, 
                                         levels=spp_vec)

# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_all <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_all %>% 
                          filter(HMM_3S_state != 1, Species == spp) %>% 
                          dplyr::select(wind_vel_kmh,swh), 
                        compute.cont=TRUE,gridsize = grid_size)
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_current <- rast(fv_df_te_wind_vel_swh %>% filter(Species == spp) %>% select(wind_vel_kmh, swh, fitted_global), type='xyz')
  response_rast_mask_current = terra::mask(response_rast_current, contour_99_current_vect)
  response_df_mask_current = as.data.frame(response_rast_mask_current, xy=T)
  
  # Save values for all spp
  contour_99_current$Species <- spp
  response_df_mask_current$Species <- spp
  
  if (spp == "Black-browed") {
    contour_99_all <- contour_99_current
    response_df_mask_all <- response_df_mask_current
  } else {
    contour_99_all <- rbind(contour_99_all,contour_99_current)
    response_df_mask_all <- rbind(response_df_mask_all,response_df_mask_current)
  }
  
}

response_df_mask_all$Species <- factor(response_df_mask_all$Species, levels=spp_vec)

# Lower res figure
ggplot(response_df_mask_all) +
  # geom_contour_filled(aes(x,y,z=exp(fitted_global)),binwidth=100) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_all$fitted_global),11)) +
  scale_fill_manual(values=inferno(10),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of combined waves (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

