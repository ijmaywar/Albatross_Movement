################################################################################
#
# Downsampling GAMs with no relative climate variable direction component
#
################################################################################

# set up models --------------------------------------

fac_k <- 3
dist <- "nb"
BIG_GAM_list <- list()

for (iter in 1:101) {

GAM_list <- list()

  for (spp in spp_vec) {
    
    # the 101st iter. is not downsampled 
    if (iter==101) {
      m_current <- m_model %>% filter(Species == spp)
    } else {
      # There are 18 black-footed albatross for m_model
      #   13 BGs and 5 Incs
      m_current <- rbind((m_model %>% filter(Species == spp, Trip_Type=="BG") %>% 
                         filter(id %in% sample(unique(id), 13))),
                         (m_model %>% filter(Species == spp, Trip_Type=="Inc") %>% 
                            filter(id %in% sample(unique(id), 5))))
    }
      
    te_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                    s(id,k=length(unique(m_current$id)),bs="re"),
                  data = m_current,
                  family = dist,
                  method = "REML")
    
    GAM_list[[spp]] <- current_GAM
        
    
    best_ds_wind <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
                               shts = c(min(current_GAM$model$shts,na.rm=TRUE),mean(current_GAM$model$shts,na.rm=TRUE)))
    best_ds_swell <- data_slice(current_GAM, wind_vel_kmh = c(min(current_GAM$model$wind_vel_kmh,na.rm=TRUE),mean(current_GAM$model$wind_vel_kmh,na.rm=TRUE)),
                                shts = evenly(shts, n=100))
    
    
    best_fv_spp_wind <- cbind(best_ds_wind[,1:2], rep(spp,nrow(best_ds_wind)),
                              fitted_values(best_GAM, data = best_ds_wind, scale = "link",
                                            terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
    best_fv_spp_swell <- cbind(best_ds_swell[,1:2], rep(spp,nrow(best_ds_swell)),
                               fitted_values(best_GAM, data = best_ds_swell, scale = "link",
                                             terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])
    
    
    colnames(best_fv_wind) <- c("wind_vel_kmh","shts","Species",
                                "fitted_global","se_global","lower_global","upper_global")
    colnames(best_fv_swell) <- c("wind_vel_kmh","shts","Species",
                                 "fitted_global","se_global","lower_global","upper_global")
    
    
    if (spp == "Black-browed" && iter == 1) {
      best_fv_wind <- best_fv_spp_wind
      best_fv_swell <- best_fv_spp_swell
    } else {
      best_fv_wind <- rbind(best_fv_wind,best_fv_spp_wind)
      best_fv_swell <- rbind(best_fv_swell,best_fv_spp_swell)
    }
  }
  
  BIG_GAM_list[[iter]] <- GAM_list

}

best_fv_wind$Species <- factor(best_fv_wind$Species, levels=spp_vec)
best_fv_swell$Species <- factor(best_fv_swell$Species, levels=spp_vec)
best_fv_wind$iter <- factor(best_fv_wind$iter)
best_fv_swell$iter <- factor(best_fv_swell$iter)

write.csv(best_fv_wind, "/Users/ian/Desktop/Manuscript_edits/Data/best_fv_wind.csv", row.names = FALSE)
write.csv(best_fv_swell, "/Users/ian/Desktop/Manuscript_edits/Data/best_fv_swell.csv", row.names = FALSE)


# plot models --------------------------------------

# fig_wind_simple <- ggplot() +
#   geom_line(fv_df_wind_vel %>% filter(iter %in% 1:100),mapping=aes(wind_vel_kmh,exp(fitted_global),group=iter),
#             alpha=0.1) +
#   geom_line(fv_df_wind_vel %>% filter(iter==101),mapping=aes(wind_vel_kmh,exp(fitted_global))) +
#   labs(y="Flaps/hour",
#        x="Windspeed (km/h)") +
#   facet_wrap(~Species,nrow=1) +
#   theme_linedraw() +
#   ylim(0,2500) + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         strip.text = element_blank())
# 
# fig_wind_simple



# Plot the flaps/hour * relative angle figures (NEW TO THE EDITS)

best_fv_wind <- read_csv("/Users/ian/Desktop/Manuscript_edits/Data/best_fv_wind.csv")
best_fv_swell <- read_csv("/Users/ian/Desktop/Manuscript_edits/Data/best_fv_swell.csv")

y_lim_max <- 2000

# Get min and max windspeed based on 99% KDEs
min_max_99_wind <- read_csv("/Users/ian/Desktop/Manuscript_edits/Data/min_max_99_wind.csv")
print(min_max_99_wind)

# Flapping rate vs. windspeed (using mean shts)
best_fv_wind_mean_shts <- best_fv_wind %>%
  group_by(Species) %>%
  filter(shts == max(shts, na.rm = TRUE)) %>%
  ungroup()

# Trim fv based on min and max values from 99% KDEs
best_fv_wind_mean_shts_trimmed <- best_fv_wind_mean_shts %>%
  left_join(min_max_99_wind, by = "Species") %>%
  filter(wind_vel_kmh >= min_val & wind_vel_kmh <= max_val)

# Make sure the spp get plotted in the correct order
best_fv_wind_mean_shts$Species <- factor(best_fv_wind_mean_shts$Species, levels=spp_vec)
best_fv_wind_mean_shts_trimmed$Species <- factor(best_fv_wind_mean_shts_trimmed$Species, levels=spp_vec)

# Plot figure
fig_wind_te_mean_shts <- ggplot(best_fv_wind_mean_shts_trimmed) +
  geom_line(aes(wind_vel_kmh,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=wind_vel_kmh,ymin=exp(lower_global),ymax=exp(upper_global),y=NULL),alpha=0.2) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  ylim(0,y_lim_max) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_te_mean_shts

# Get min and max swell height based on 99% KDEs
min_max_99_swell <- read_csv("/Users/ian/Desktop/Manuscript_edits/Data/min_max_99_swell.csv")
print(min_max_99_swell)

# Flapping rate vs. shts (using mean windspeed)
best_fv_shts_mean_wind <- best_fv_swell %>%
  group_by(Species) %>%
  filter(wind_vel_kmh == max(wind_vel_kmh, na.rm = TRUE)) %>%
  ungroup()

# Trim fv based on min and max values from 99% KDEs
best_fv_shts_mean_wind_trimmed <- best_fv_shts_mean_wind %>%
  left_join(min_max_99_swell, by = "Species") %>%
  filter(shts >= min_val & shts <= max_val)

# Make sure the spp get plotted in the correct order
best_fv_shts_mean_wind$Species <- factor(best_fv_shts_mean_wind$Species, levels=spp_vec)
best_fv_shts_mean_wind_trimmed$Species <- factor(best_fv_shts_mean_wind_trimmed$Species, levels=spp_vec)

# Plot figure
fig_shts_te_mean_wind <- ggplot(best_fv_shts_mean_wind_trimmed) +
  geom_line(aes(shts,exp(fitted_global))) +
  geom_ribbon(mapping=aes(x=shts,
                          ymin=exp(lower_global), 
                          ymax = ifelse(exp(upper_global)>y_lim_max, y_lim_max, exp(upper_global)),
                          y=NULL),
              alpha=0.2) +
  labs(y="Flaps/hour",
       x="Swell height (m)") +
  ylim(0,y_lim_max) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_shts_te_mean_wind

wrap_elements(panel = fig_wind_te_mean_shts / fig_shts_te_mean_wind)

ggsave("/Users/ian/Desktop/Manuscript_edits/Figures/R_outputs/Tensor_using_mean.png",
       wrap_elements(panel = fig_wind_te_mean_shts / fig_shts_te_mean_wind),
       width = 8,
       height = 4,
       dpi = 300)