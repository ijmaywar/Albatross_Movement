# Figures for thesis

################################################################################
# Figure of mean wind speed during breeding season relative to study site location 
m_all_poscomplete |>
  ggplot(aes(Location,wind_vel_kmh)) +
  geom_boxplot() +
  theme_bw()

################################################################################
# Figures of wind responses by species (as for conference talk)
#   2x5 plot
#   Continuous tensor product model of flapping response ( wind*waves)
#   Categorical response of flaps to wind for head / side/ tail (for illustration purposes)

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

# Reds for winds
reds <- colorRampPalette(c("white", "red4"))
fig_wind_cont <- ggplot(fv_df_te_wind_vel_angle_trim) +
  geom_contour_filled(aes(wind_vel_kmh,bird_wind_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_wind_vel_angle_trim$fitted_global),11)) +
  scale_fill_manual(values=reds(10),drop=FALSE) +
  labs(y="Bird-wind-angle (degrees)") +
  labs(fill = "Flaps/hour") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(axis.title.x=element_blank())

fig_wind_cont

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_angle[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df

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
  theme_bw() + 
  theme(axis.title.x = element_blank())

fig_wind_cat

# Wrap figures for display -----------------------------------------------------
wrap_elements(panel = fig_wind_cont / fig_wind_cat) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")

for (i in 1:5) {
  summary_i <- summary(GAM_list_wind_vel_kmh_cat[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df


################################################################################
# Figures of wave responses by species (as for wind)
#   2 x 5 plot
#   Continuous tensor product model of flapping response ( wave height * direction)
#   Categorical response of flaps to wind for along vs. perpendicular vs. against waves (for illustration purposes; along= 0-60, perp=60-120, against=120-180)

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

# Blues for swells
blues <- colorRampPalette(c("white", "navy"))
fig_swells_cont <- ggplot(fv_df_te_shts_angle_trim) +
  geom_contour_filled(aes(shts,bird_swell_angle,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(fv_df_te_shts_angle_trim$fitted_global),11)) +
  scale_fill_manual(values=blues(10),drop=FALSE) +
  labs(y="Bird-swell-angle (degrees)") +
  labs(fill = "Flaps/hour") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw() +
  theme(axis.title.x=element_blank())

fig_swells_cont

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_shts_angle[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df

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
  theme_bw() + 
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

# Wrap figures for display -----------------------------------------------------
wrap_elements(panel = fig_swells_cont / fig_swells_cat) +
  labs(tag = "Significant height of total swells (m)") +
  theme(plot.tag.position = "bottom")


################################################################################
# Figures of wind and wave responses together
#   1x 5 plot
#   Continuous tensor product model of flapping response (wave height * wind speed)

# te(wind_vel_kmh,shts), s(id) --------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_shts <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,shts)
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_current %>% dplyr::select(wind_vel_kmh,shts), 
                        compute.cont=TRUE,gridsize = grid_size)
  
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  ds_rast_current <- rast(m_current %>% filter(Species == spp) %>% select(wind_vel_kmh, shts), type='xyz')
  ds_rast_mask_current = terra::mask(ds_rast_current, contour_99_current_vect)
  ds_df_mask_current = as.data.frame(ds_rast_mask_current, xy=T)
  
  
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_te_wind_vel_shts[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            shts = evenly(shts, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
  
  colnames(link_df) <- c("wind_vel_kmh","shts","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_te_wind_vel_shts <- link_df
  } else {
    fv_df_te_wind_vel_shts <- rbind(fv_df_te_wind_vel_shts,link_df)
  }
}

fv_df_te_wind_vel_shts$Species <- factor(fv_df_te_wind_vel_shts$Species, 
 levels=spp_vec)

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_shts[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df


# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_all <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_all %>% 
                          filter(HMM_3S_state != 1, Species == spp) %>% 
                          dplyr::select(wind_vel_kmh,shts), 
                        compute.cont=TRUE,gridsize = grid_size)
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_current <- rast(fv_df_te_wind_vel_shts %>% filter(Species == spp) %>% select(wind_vel_kmh, shts, fitted_global), type='xyz')
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

response_df_mask_all$Species <- factor(response_df_mask_all$Species, 
 levels=spp_vec)

# Higher res figure
ggplot(response_df_mask_all) +
  # geom_contour_filled(aes(x,y,z=exp(fitted_global)),binwidth=100) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_all$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

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
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

################################################################################
# Box of violin plots of wind and waves experienced by species 
# [this and the boxplot below should only represent complete trips; 
# all other analyses can use all available data]
#   2x5 plot

fig_winds <- m_all_poscomplete |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3) +
  theme_bw() +
  labs(y="Windspeeds (km/h)") +
  theme(axis.title.x = element_blank())

fig_swells <- m_all_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3) +
  theme_bw() +
  labs(y="Significant height of total swell (m)") +
  theme(axis.title.x = element_blank())

wrap_elements(panel = fig_winds / fig_swells) +
  labs(tag = "Species") +
  theme(plot.tag.position = "bottom")

################################################################################
# Box or violin plots [or density plots?] of prop. time spent in 
# low/ medium/ high wind and waves (using same breakpoints across species; 
# earlier we discussed randomly sampling the same no. of BBAL, GHAL and 
# WAAL points as BFAL and LAAL points to generate an overall distribution of 
# wave heights experienced across all species, and picking, say the 33 and 66 
# quantiles to distinguish between low/ med/ high)
#   2x5 plot

# Randomly downsample m_all_poscomplete such that there are 17 individuals for each species.
m_all_poscomplete_spp_ds <- rbind(
  m_all_poscomplete %>% filter(Species == "Black-browed") %>% filter(id %in% sample(unique(id), 17)),
  m_all_poscomplete %>% filter(Species == "Grey-headed") %>% filter(id %in% sample(unique(id), 17)),
  m_all_poscomplete %>% filter(Species == "Wandering") %>% filter(id %in% sample(unique(id), 17)),
  m_all_poscomplete %>% filter(Species == "Black-footed"),
  m_all_poscomplete %>% filter(Species == "Laysan") %>% filter(id %in% sample(unique(id), 17)))

wind_vel_kmh_breaks <- quantile(m_all_poscomplete_spp_ds$wind_vel_kmh, probs=c((1/3),(2/3)))
shts_cat_breaks <- quantile(m_all_poscomplete_spp_ds$shts, probs=c((1/3),(2/3)))

m_all_poscomplete <- m_all_poscomplete %>% 
  mutate(shts_cat = case_when(shts<shts_cat_breaks[[1]] ~ "low",
                               shts>=shts_cat_breaks[[1]] & shts<shts_cat_breaks[[2]] ~ "medium",
                               shts>=shts_cat_breaks[[2]] ~ "high"),
         wind_vel_kmh_cat = case_when(wind_vel_kmh<wind_vel_kmh_breaks[[1]] ~ "low",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[1]] & wind_vel_kmh<wind_vel_kmh_breaks[[2]] ~ "medium",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[2]] ~ "high"))

wind_vel_kmh_cat_density_data <- as.data.frame(m_all_poscomplete %>% group_by(Location,Species,id,wind_vel_kmh_cat) %>% 
                                      summarize(count=n()) %>% 
                                      mutate(proportion = count/sum(count),
                                             wind_vel_kmh_cat = factor(wind_vel_kmh_cat,levels=c("low","medium","high"))))

shts_cat_density_data <- as.data.frame(m_all_poscomplete %>% group_by(Location,Species,id,shts_cat) %>% 
                                      summarize(count=n()) %>% 
                                      mutate(proportion = count/sum(count),
                                             shts_cat = factor(shts_cat,levels=c("low","medium","high"))))

ggplot(wind_vel_kmh_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=wind_vel_kmh_cat),alpha=0.5) +
  # scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Windspeed")) +
  theme(text = element_text(size = 24))

ggplot(wind_vel_kmh_cat_density_data) +
  geom_boxplot(aes(x=wind_vel_kmh_cat,y=proportion)) +
  labs(y="Proportion of time") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

ggplot(shts_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=shts_cat),alpha=0.5) +
  # scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Swell height")) +
  theme(text = element_text(size = 24))

ggplot(shts_cat_density_data) +
  geom_boxplot(aes(x=shts_cat,y=proportion)) +
  labs(y="Proportion of time") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()


# # Randomly downsample m_all_poscomplete such that there are 48 individuals for each location.
# m_all_poscomplete_loc_ds <- rbind(
#   m_all_poscomplete %>% filter(Location == "Bird_Island") %>% filter(id %in% sample(unique(id), 48)),
#   m_all_poscomplete %>% filter(Location == "Midway"))
# 
# quantile(m_all_poscomplete_loc_ds$shts, probs=c((1/3),(2/3)))


################################################################################
# Box or violin plots [or density plots?] of prop. time spent in 
# high/ side/ tail winds AND wave direction [with/  against/ across wave direction] 

# Winds
bird_wind_angle_cat_hist_data <- as.data.frame(m_all_poscomplete %>%
                                                 drop_na(bird_wind_angle_cat) %>% 
                                                 group_by(Location,Species,id,bird_wind_angle_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count)))

# Density figure
ggplot(bird_wind_angle_cat_hist_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=bird_wind_angle_cat),alpha=0.5) +
  scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Relative wind")) +
  theme(text = element_text(size = 24))

# Boxplot
ggplot(bird_wind_angle_cat_hist_data) +
  geom_boxplot(aes(x=bird_wind_angle_cat,y=proportion)) +
  labs(y="Proportion of time") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

# Swells
swell_bird_angle_cat_density_data <- as.data.frame(m_all_poscomplete %>%
                                     drop_na(bird_swell_angle_cat) %>% 
                                     group_by(Location,Species,id,bird_swell_angle_cat) %>% 
                                     summarize(count=n()) %>% 
                                     mutate(proportion = count/sum(count)))

# Density plot
ggplot(swell_bird_angle_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=bird_swell_angle_cat),alpha=0.5) +
  scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Relative swell")) +
  theme(text = element_text(size = 24))

# Boxplot
ggplot(swell_bird_angle_cat_density_data) +
  geom_boxplot(aes(x=bird_swell_angle_cat,y=proportion)) +
  labs(y="Proportion of time") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

################################################################################
# Table of sample size - no. accel + GPS deployments by species 

# poscomplete are when GPS tag writes for the entire trip, ignores acc data.
# This is for summarizing IDs
merge(as.data.frame(m_all %>% group_by(Species) %>% summarize(IDs=n_distinct(id))),
               as.data.frame(m_all_poscomplete %>% group_by(Species) %>% summarize(IDs_poscomplete=n_distinct(id))),
               by = "Species")

# This is for summarizing tripIDs
merge(as.data.frame(m_all %>% group_by(Species) %>% summarize(tripIDs=n_distinct(tripID))),
               as.data.frame(m_all_poscomplete %>% group_by(Species) %>% summarize(tripIDs_poscomplete=n_distinct(tripID))),
               by = "Species")


################################################################################
# Table of deployments based on tag combinations

m_all %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

temp <- m_all %>% 
  filter(HMM_3S_state!=1) %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

m_all_poscomplete %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

m_all_poscomplete %>% 
  filter(HMM_3S_state!=1) %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

################################################################################
# Table of deployments based on field season

temp <- m_all %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

temp <- m_all_poscomplete %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

clipr::write_clip(temp)

