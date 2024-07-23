
# GAM to predict resting based on wind_vel -------------------------------------

main_k <- 3
fac_k <- 3
GAM_resting_list <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- ds_m_all_nonaflaps %>% filter((HMM_3S_state == 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_resting_list[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10])
  
  link_df <- cbind(current_ds,
                       rep(spp,nrow(current_ds)),
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("(Intercept)","s(wind_vel)","s(id)"))[,3:6],
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("(Intercept)","s(wind_vel)"))[,3:6],
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("s(wind_vel)"))[,3:6],
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("(Intercept)"))[,3:6],
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("s(id)"))[,3:6])
  
  colnames(link_df) <- c("wind_vel","id","Species",
                             "fitted_all","se_all","lower_all","upper_all",
                             "fitted_global","se_global","lower_global","upper_global",
                             "fitted_wind","se_wind","lower_wind","upper_wind",
                             "fitted_int","se_int","lower_int","upper_int",
                             "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_resting <- current_ds
    fv_df_resting <- link_df
  } else {
    ds_df_resting <- rbind(ds_df_resting,current_ds)
    fv_df_resting <- rbind(fv_df_resting,link_df)
  }
}

fv_df_resting$Species <- factor(fv_df_resting$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

y_max <- 1050
# Link: Wind + Intercept
fv_df_resting |>
  ggplot(aes(wind_vel,exp(fitted_global))) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_int+lower_wind),
                          # ymax=ifelse(exp(upper_global)>y_max,y_max,exp(upper_global)),
                          ymax=exp(upper_global),
                          y=NULL),alpha=0.3) +
  labs(title="Predicting for flaps") +
  # xlim(0,25) + 
  # ylim(0,1050) +
  facet_wrap(~Species)




