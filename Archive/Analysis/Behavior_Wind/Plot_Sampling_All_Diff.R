library(scico)

# Find domain of windvel and bwa from both sets of models
spp_vec <- c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")
GAM_diff_df <- data.frame(Species = spp_vec,
                          min_windvel = numeric(5),
                          max_windvel = numeric(5),
                          min_bwa = numeric(5),
                          max_bwa = numeric(5))
GAM_diff_df$ids <- list(NA)

for (i in 1:5) {
  spp <- spp_vec[i]
  current_GAM <- GAM_list[[spp]]
  current_GAM_ds <- GAM_list_ds[[spp]]
  min_windvel <- max(c(min(current_GAM$model$wind_vel),min(current_GAM_ds$model$wind_vel)))
  max_windvel <- min(c(max(current_GAM$model$wind_vel),max(current_GAM_ds$model$wind_vel)))
  min_bwa <- max(c(min(current_GAM$model$bwa),min(current_GAM_ds$model$bwa)))
  max_bwa <- min(c(max(current_GAM$model$bwa),max(current_GAM_ds$model$bwa)))
  ids <- intersect(current_GAM$model$id,current_GAM_ds$model$id)
  
  GAM_diff_df$min_windvel[i] <- min_windvel
  GAM_diff_df$max_windvel[i] <- max_windvel
  GAM_diff_df$min_bwa[i] <- min_bwa
  GAM_diff_df$max_bwa[i] <- max_bwa
  GAM_diff_df$ids[[i]] <- ids
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))
  
  current_ds  <- expand_grid(wind_vel = seq(min_windvel,max_windvel,length.out=100), 
                             id = ids[1:10],
                             bwa = seq(min_bwa,max_bwa,length.out=100))
  
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(wind_vel)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("ti(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7])
                   
   link_df_ds <- cbind(current_ds,
                    rep(spp,nrow(current_ds)),
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)","s(id)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("(Intercept)","ti(wind_vel)","ti(bwa)","ti(wind_vel,bwa)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("ti(wind_vel)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("ti(bwa)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("ti(wind_vel,bwa)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("(Intercept)"))[,4:7],
                    fitted_values(current_GAM_ds, data = current_ds, scale = "link",
                                  terms = c("s(id)"))[,4:7])
   
   colnames(link_df) <- c("wind_vel","id","bwa","Species",
                          "fitted_all","se_all","lower_all","upper_all",
                          "fitted_global","se_global","lower_global","upper_global",
                          "fitted_windvel","se_windvel","lower_windvel","upper_windvel",
                          "fitted_bwa","se_bwa","lower_bwa","upper_bwa",
                          "fitted_intrxn","se_intrxn","lower_intrxn","upper_intrxn",
                          "fitted_int","se_int","lower_int","upper_int",
                          "fitted_id","se_id","lower_id","upper_id")
   
   colnames(link_df_ds) <- colnames(link_df)
   
   if (spp == "BBAL") {
     ds_df_diff <- current_ds
     fv_df_link_diff <- link_df
     fv_df_link_diff_ds <- link_df_ds
   } else {
     ds_df_diff <- rbind(ds_df_diff,current_ds)
     fv_df_link_diff <- rbind(fv_df_link_diff,link_df)
     fv_df_link_diff_ds <- rbind(fv_df_link_diff_ds,link_df_ds)
   }
}

# Make a dataframe of the difference between the estimates created by the two GAMs
fv_GAM_diff_df <- fv_df_link_diff[,1:4]
fv_GAM_diff_df$diff_global <- exp(fv_df_link_diff$fitted_global) - exp(fv_df_link_diff_ds$fitted_global)
fv_GAM_diff_df$perc_chng_global <- 100*fv_GAM_diff_df$diff_global/exp(fv_df_link_diff$fitted_global)
fv_GAM_diff_df$Species <- factor(fv_GAM_diff_df$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

ggplot(fv_GAM_diff_df) +
  scale_fill_manual(values=colorRampPalette(c('blue','white','red'))(19),drop=FALSE) +
  geom_contour_filled(aes(wind_vel,bwa,z=diff_global),breaks=seq(-800,800,length.out=20)) +
  # geom_contour_filled(aes(wind_vel,bwa,z=fitted_global),breaks=floor(seq(-4,4,length.out=20))) +
  labs(fill = "Difference in flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
  facet_wrap(~Species)

ggplot(fv_GAM_diff_df) +
  scale_fill_manual(values=colorRampPalette(c('blue','white','red'))(19),drop=FALSE) +
  geom_contour_filled(aes(wind_vel,bwa,z=diff_global),breaks=seq(-500,500,length.out=20)) +
  # geom_contour_filled(aes(wind_vel,bwa,z=fitted_global),breaks=floor(seq(-4,4,length.out=20))) +
  labs(fill = "Percentage change in flaps/hour", x="Wind Velocity (m/s)", y="Bird-wind angle (degrees)") +
  facet_wrap(~Species)


# Blue: ds > all
# Red: all > ds


