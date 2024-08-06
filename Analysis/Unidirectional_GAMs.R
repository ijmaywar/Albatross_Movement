################################################################################
#
# GAMs with no relative climate variable direction component
#
################################################################################

# s(wind_vel_kmh), s(id) --------------------------------------

fac_k <- 3
GAM_list_wind_vel <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh)
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel_kmh,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_wind_vel[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)","s(id)"))[,3:6],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel_kmh)"))[,3:6])
  
  colnames(link_df) <- c("wind_vel_kmh","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
  
  if (spp == "Black-browed") {
    fv_df_wind_vel <- link_df
  } else {
    fv_df_wind_vel <- rbind(fv_df_wind_vel,link_df)
  }
}

fv_df_wind_vel$Species <- factor(fv_df_wind_vel$Species, levels=spp_vec)

fig_wind_simple <- ggplot(fv_df_wind_vel) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_simple


# Summary ----------------------------------------------------------------------

for (i in 1:5) {
  summary_i <- summary(GAM_list_wind_vel[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df

# s(shts), s(id) --------------------------------------

fac_k <- 3
GAM_list_shts <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(shts)
  
  current_GAM <- gam(formula = flaps ~ s(shts,k=fac_k,bs='tp') + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_shts[[spp]] <- current_GAM
  
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
}

fv_df_shts$Species <- factor(fv_df_shts$Species, levels=spp_vec)

fig_shts_simple <- ggplot(fv_df_shts) +
  geom_line(aes(shts,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=shts,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  guides(color=guide_legend(title="Relative wind")) +
  labs(y="Flaps/hour",
       x="Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_simple

# Summary

for (i in 1:5) {
  summary_i <- summary(GAM_list_shts[[i]])
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
# Wrap GAM figure with env boxplot ---------------------------------------------

# Windspeed box plot
ws_boxplot <- ggplot(m_all_poscomplete, aes(y = wind_vel_kmh)) +
  geom_boxplot(width=1) +
  theme_void() +
  xlim(-1,.5) +
  coord_flip() +
  facet_wrap(~Species,nrow=1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())

fig_wind_simple + ws_boxplot +
  plot_layout(heights=c(5,1)) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")


# Windspeed box plot
shts_boxplot <- ggplot(m_all_poscomplete, aes(y = shts)) +
  geom_boxplot(width=1) +
  theme_void() +
  xlim(-1,.5) +
  coord_flip() +
  facet_wrap(~Species,nrow=1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())

fig_shts_simple + shts_boxplot +
  plot_layout(heights=c(5,1)) +
  labs(tag = "Significant height of total swell (m)") +
  theme(plot.tag.position = "bottom")

################################################################################
# Wrap GAM figure with env density plot ----------------------------------------

# Windspeed density plot
ws_density <- m_all_poscomplete |>
  ggplot(aes(x=wind_vel_kmh)) +
  geom_density(aes(y=after_stat(density))) +
  theme_void() +
  # xlim(-1,.5) +
  scale_y_reverse() +
  facet_wrap(~Species,nrow=1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())

fig_wind_simple + ws_density +
  plot_layout(heights=c(5,1)) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")

# shts density plot
shts_density <- m_all_poscomplete |>
  ggplot(aes(x=shts)) +
  geom_density(aes(y=after_stat(density))) +
  theme_void() +
  scale_y_reverse() +
  facet_wrap(~Species,nrow=1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())

fig_shts_simple + shts_density +
  plot_layout(heights=c(5,1)) +
  labs(tag = "Significant height of total swell (m)") +
  theme(plot.tag.position = "bottom")

