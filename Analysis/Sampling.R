m_all_nonaflaps <- m_all %>% drop_na(flaps)
m_BBAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BBAL")
m_GHAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="GHAL")
m_WAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="WAAL")
m_BFAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="BFAL")
m_LAAL_nonaflaps <- m_all_nonaflaps %>% filter(Species=="LAAL")

# Downsampling Bird_Island to Midway --------------------------------------

downsampled_ids <- c(sample(unique(m_BBAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_GHAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_WAAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     sample(unique(m_LAAL_nonaflaps$id),
                           size=length(unique(m_BFAL_nonaflaps$id)),
                           replace=FALSE),
                     unique(m_BFAL_nonaflaps$id))
ds_m_all_nonaflaps <- m_all_nonaflaps %>% filter(id %in% downsampled_ids)


# Flaps/hour vs wind_vel (continuous) after removing HMM_3S_state == 1 ----------------------

main_k <- 3
fac_k <- 3
GAM_continuous_list <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- ds_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~ s(wind_vel,bs='tp',k=main_k,m=2) +
                       te(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp'),m=2) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_continuous_list[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))
  response_df <- cbind(current_ds,
                       rep(spp,nrow(current_ds)),
                       fitted_values(current_GAM, data = current_ds, scale = "response",
                                     terms = c("(Intercept)","s(wind_vel)","te(wind_vel,bwa)","s(id)"))[,4:7],
                       fitted_values(current_GAM, data = current_ds, scale = "response",
                                     terms = c("(Intercept)","s(wind_vel)","te(wind_vel,bwa)"))[,4:7],
                       fitted_values(current_GAM, data = current_ds, scale = "response",
                                     terms = c("s(wind_vel)"))[,4:7],
                       fitted_values(current_GAM, data = current_ds, scale = "response",
                                     terms = c("te(wind_vel,bwa)"))[,4:7],
                       fitted_values(current_GAM, data = current_ds, scale = "response",
                                     terms = c("(Intercept)"))[,4:7],
                       fitted_values(current_GAM, data = current_ds, scale = "link",
                                     terms = c("s(id)"))[,4:7]
  )
  colnames(response_df) <- c("wind_vel","id","bwa","Species",
                             "fitted_all","se_all","lower_all","upper_all",
                             "fitted_global","se_global","lower_global","upper_global",
                             "fitted_wind","se_wind","lower_wind","upper_wind",
                             "fitted_te","se_te","lower_te","upper_te",
                             "fitted_int","se_int","lower_int","upper_int",
                             "fitted_id","se_id","lower_id","upper_id")
  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel)","te(wind_vel,bwa)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","s(wind_vel)","te(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(wind_vel)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("te(wind_vel,bwa)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel","id","bwa","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_wind","se_wind","lower_wind","upper_wind",
                         "fitted_te","se_te","lower_te","upper_te",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_cont <- current_ds
    fv_df_cont_response <- response_df
    fv_df_cont_link <- link_df
  } else {
    ds_df_cont <- rbind(ds_df_cont,current_ds)
    fv_df_cont_response <- rbind(fv_df_cont_response,response_df)
    fv_df_cont_link <- rbind(fv_df_cont_link,link_df)
  }
}

fv_df_cont_response$Species <- factor(fv_df_cont_response$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))
fv_df_cont_link$Species <- factor(fv_df_cont_link$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Response (flaps/hour)
fv_df_cont_response |>
  ggplot(aes(wind_vel,bwa,z=fitted_global)) +
  geom_contour_filled() +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1000) +
  facet_wrap(~Species)

# Link
fv_df_cont_link |>
  ggplot(aes(wind_vel,bwa,z=exp(fitted_te))) +
  geom_contour_filled() +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1000) +
  facet_wrap(~Species)

# Response: Wind + Intercept
fv_df_cont_response |>
  ggplot(aes(wind_vel,fitted_wind+fitted_int)) +
  geom_line() +
  geom_ribbon(mapping=aes(ymin = lower_wind + lower_int, ymax = upper_wind + upper_int, y = NULL),alpha = 0.3) +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1050) +
  facet_wrap(~Species)


y_max <- 1050
# Link: Wind + Intercept
fv_df_cont_link |>
  ggplot(aes(wind_vel,exp(fitted_int+fitted_wind))) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_int+lower_wind),
                          ymax=ifelse(exp(upper_int+upper_wind)>y_max,y_max,exp(upper_int+upper_wind)),
                          y=NULL),alpha=0.3) +
  labs(title="continuous bwa") +
  xlim(0,25) + 
  ylim(0,1050) +
  facet_wrap(~Species)




