################################################################################
#
# Downsampling GAMs with no relative climate variable direction component
#
################################################################################

# First, run everything from Manuscript_edits.R until "Sample stats"

# set up models --------------------------------------

fac_k <- 3
dist <- "nb"
BIG_GAM_list <- list()
n_iter=101 # This should be the number of downsampled simulations plus one 
          # for the regular (not downsampled) model 

for (iter in 1:n_iter) {

GAM_list <- list()

  for (spp in spp_vec) {
    
    # the last iter is not downsampled 
    if (iter==n_iter) {
      m_current <- m_model %>% filter(Species == spp)
    } else {
      # There are 18 black-footed albatross for m_model
      #   12 BGs and 6 Incs
      if (spp!="Laysan") {
      m_current <- rbind((m_model %>% filter(Species == spp, Trip_Type=="BG") %>% 
                         filter(id %in% sample(unique(id), 12))),
                         (m_model %>% filter(Species == spp, Trip_Type=="Inc") %>% 
                            filter(id %in% sample(unique(id), 6))))
      } else { # We cannot recreate the sample BG/Inc ratio for Laysan because there are only 10 BG Laysan individuals. We get as close to it 
        # as possible
        m_current <- rbind((m_model %>% filter(Species == spp, Trip_Type=="BG") %>% 
                              filter(id %in% sample(unique(id), 10))),
                           (m_model %>% filter(Species == spp, Trip_Type=="Inc") %>% 
                              filter(id %in% sample(unique(id), 8))))
      }
    }
      
    current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
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
                              fitted_values(current_GAM, data = best_ds_wind, scale = "link",
                                            terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,5:8], iter)
    best_fv_spp_swell <- cbind(best_ds_swell[,1:2], rep(spp,nrow(best_ds_swell)),
                               fitted_values(current_GAM, data = best_ds_swell, scale = "link",
                                             terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,5:8], iter)
    
    
    colnames(best_fv_spp_wind) <- c("wind_vel_kmh","shts","Species",
                                "fitted_global","se_global","lower_global","upper_global","iter")
    colnames(best_fv_spp_swell) <- c("wind_vel_kmh","shts","Species",
                                 "fitted_global","se_global","lower_global","upper_global","iter")
    
    
    if (spp == "Black-browed" && iter == 1) {
      best_fv_wind <- best_fv_spp_wind
      best_fv_swell <- best_fv_spp_swell
    } else {
      best_fv_wind <- rbind(best_fv_wind,best_fv_spp_wind)
      best_fv_swell <- rbind(best_fv_swell,best_fv_spp_swell)
    }
  }
  
  BIG_GAM_list[[iter]] <- GAM_list
  print(paste0("Iter: ",iter))

}

best_fv_wind$Species <- factor(best_fv_wind$Species, levels=spp_vec)
best_fv_swell$Species <- factor(best_fv_swell$Species, levels=spp_vec)
best_fv_wind$iter <- factor(best_fv_wind$iter)
best_fv_swell$iter <- factor(best_fv_swell$iter)


################################################################################
# Create Model V to get trim values
fac_k <- 3
dist <- "nb"
for (spp in spp_vec) {
  
  m_current <- m_model %>% filter(Species == spp)
  
  te_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                  s(id,k=length(unique(m_current$id)),bs="re"),
                data = m_current,
                family = dist,
                method = "REML")
  
  best_ds <- data_slice(te_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n=100),
                        shts = evenly(shts, n=100))

  best_fv_spp <- cbind(best_ds[,1:2], rep(spp,nrow(best_ds)),
                       fitted_values(te_GAM, data = best_ds, scale = "link",
                                     terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,5:8])
  
  if (spp == "Black-browed") {
    best_fv <- best_fv_spp
  } else {
    best_fv <- rbind(best_fv,best_fv_spp)
  }
  print(paste0(spp,": done."))
}

colnames(best_fv) <- c("wind_vel_kmh","shts","Species",
                       "fitted_global","se_global","lower_global","upper_global")

best_fv$Species <- factor(best_fv$Species, levels=spp_vec)


################################################################################
# TRIM RESPONSES of te GAMs BASED ON EXPERIENCED VALUES

# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_best_all <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_best <- ks::kde(m_model %>%
                       filter(Species == spp) %>%
                       dplyr::select(wind_vel_kmh,shts),
                     compute.cont=TRUE,gridsize = grid_size)
  
  contour_99_best <- data.frame(with(kd_best, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                           z=estimate, levels=cont["1%"])[[1]]))
  
  contour_99_best_vect <- as.polygons(as.lines((contour_99_best %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_best <- terra::rast(best_fv %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, shts, fitted_global), type='xyz')
  response_rast_mask_best = terra::mask(response_rast_best, contour_99_best_vect)
  response_df_mask_best = as.data.frame(response_rast_mask_best, xy=T)
  
  # Save values for all spp
  response_df_mask_best$Species <- spp
  
  if (spp == "Black-browed") {
    response_df_mask_best_all <- response_df_mask_best
  } else {
    response_df_mask_best_all <- rbind(response_df_mask_best_all,response_df_mask_best)
  }
  print(paste0(spp,": done."))
}

response_df_mask_best_all$Species <- factor(response_df_mask_best_all$Species,
                                            levels=spp_vec)

# Min and Max vals for windspeed
min_max_99_wind <- response_df_mask_best_all %>% group_by(Species) %>% summarise(min_val = min(x, na.rm = TRUE), max_val = max(x, na.rm = TRUE))
print(min_max_99_wind)

# Min and Max vals for swell height
min_max_99_swell <- response_df_mask_best_all %>% group_by(Species) %>% summarise(min_val = min(y, na.rm = TRUE), max_val = max(y, na.rm = TRUE))
print(min_max_99_swell)



################################################################################
# plot models ------------------------------------------------------------------

y_lim_max <- 2000

# Flapping rate vs. windspeed (using mean shts)
best_fv_wind_mean_shts <- best_fv_wind %>%
  group_by(Species,iter) %>%
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
fig_wind_te_mean_shts <- ggplot() +
  geom_line(best_fv_wind_mean_shts_trimmed %>% filter(iter %in% 1:(n_iter-1)),
            mapping = aes(wind_vel_kmh,exp(fitted_global),group=iter),
            alpha=0.1) +
  geom_line(best_fv_wind_mean_shts_trimmed %>% filter(iter==n_iter),
            mapping = aes(wind_vel_kmh,exp(fitted_global))) +
  labs(y="Flaps/hour",
       x="Windspeed (km/h)") +
  ylim(0,y_lim_max) +
  facet_wrap(~Species,nrow=1) +
  theme_linedraw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_wind_te_mean_shts

###############################################
# Flapping rate vs. shts (using mean windspeed)
best_fv_shts_mean_wind <- best_fv_swell %>%
  group_by(Species,iter) %>%
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
fig_shts_te_mean_wind <- ggplot() +
  geom_line(best_fv_shts_mean_wind_trimmed %>% filter(iter %in% 1:(n_iter-1)),
            mapping = aes(shts,exp(fitted_global),group=iter),
            alpha=0.1) +
  geom_line(best_fv_shts_mean_wind_trimmed %>% filter(iter==n_iter),
            mapping = aes(shts,exp(fitted_global))) +
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

ggsave("/Users/imaywar/Desktop/Downsampling.png",
       wrap_elements(panel = fig_wind_te_mean_shts / fig_shts_te_mean_wind),
       width = 8,
       height = 4,
       dpi = 300)
