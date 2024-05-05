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


# Flaps/hour vs wind_vel (continuous) after removing HMM_3S_state == 1
# WITHOUT A WIND TERM ON ITS OWN.

main_k <- 3
fac_k <- 3
GAM_list_cont_ds <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- ds_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_cont_ds[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))
  
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
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel","id","bwa","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_windvel","se_windvel","lower_windvel","upper_windvel",
                         "fitted_bwa","se_bwa","lower_bwa","upper_bwa",
                         "fitted_intrxn","se_intrxn","lower_intrxn","upper_intrxn",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_cont_ds <- current_ds
    fv_df_cont_link_ds <- link_df
  } else {
    ds_df_cont_ds <- rbind(ds_df_cont_ds,current_ds)
    fv_df_cont_link_ds <- rbind(fv_df_cont_link_ds,link_df)
  }
}

fv_df_cont_link_ds$Species <- factor(fv_df_cont_link$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Link
fv_df_cont_link_ds |>
  ggplot(aes(wind_vel,bwa,z=exp(fitted_global))) +
  geom_contour_filled(breaks=seq(0,2000,by=100)) +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1000) +
  facet_wrap(~Species)

# Link: Wind + Intercept
fv_df_cont_link_ds |>
  ggplot(aes(wind_vel,exp(fitted_int+fitted_windvel))) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_int+lower_windvel),ymax=exp(upper_int+upper_windvel),y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="Flaps/hour",x="Wind velocity (m/s)")

# Link: Wind + Intercept
fv_df_cont_link_ds |>
  ggplot(aes(wind_vel,fitted_windvel)) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=lower_windvel,ymax=upper_windvel,y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="GAM wind effect",x="Wind velocity (m/s)")




# Upsampling Midway to Bird_Island  --------------------------------------------

upsampled_rows <- c(sample(which(m_all_nonaflaps$Species=="BBAL"),
                            size=nrow(m_GHAL_nonaflaps),
                            replace=TRUE),
                    which(m_all_nonaflaps$Species=="GHAL"),
                    sample(which(m_all_nonaflaps$Species=="WAAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE),
                    sample(which(m_all_nonaflaps$Species=="BFAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE),
                    sample(which(m_all_nonaflaps$Species=="LAAL"),
                           size=nrow(m_GHAL_nonaflaps),
                           replace=TRUE))
us_m_all_nonaflaps <- m_all_nonaflaps[upsampled_rows,]


# Flaps/hour vs wind_vel (continuous) after removing HMM_3S_state == 1
# WITHOUT A WIND TERM ON ITS OWN.

main_k <- 3
fac_k <- 3
GAM_list_cont_us <- list()

for (spp in c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL")) {
  
  m_current <- us_m_all_nonaflaps %>% filter((HMM_3S_state != 1) & (Species == spp))
  
  current_GAM <- gam(formula = flaps ~
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,k=fac_k,bs='tp') +
                       ti(wind_vel,bwa,k=c(fac_k,fac_k),bs=c('tp','tp')) + 
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")
  
  GAM_list_cont_us[[spp]] <- current_GAM
  
  current_ds  <- data_slice(current_GAM, wind_vel = evenly(wind_vel, n = 100), 
                            id = unique(m_current$id)[1:10],
                            bwa = evenly(bwa,n=100))
  
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
                                 terms = c("s(id)"))[,4:7]
  )
  colnames(link_df) <- c("wind_vel","id","bwa","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global",
                         "fitted_windvel","se_windvel","lower_windvel","upper_windvel",
                         "fitted_bwa","se_bwa","lower_bwa","upper_bwa",
                         "fitted_intrxn","se_intrxn","lower_intrxn","upper_intrxn",
                         "fitted_int","se_int","lower_int","upper_int",
                         "fitted_id","se_id","lower_id","upper_id")
  
  if (spp == "BBAL") {
    ds_df_cont_us <- current_ds
    fv_df_cont_link_us <- link_df
  } else {
    ds_df_cont_us <- rbind(ds_df_cont_us,current_ds)
    fv_df_cont_link_us <- rbind(fv_df_cont_link_us,link_df)
  }
}

fv_df_cont_link_us$Species <- factor(fv_df_cont_link$Species , levels=c("BBAL", "GHAL", "WAAL", "BFAL", "LAAL"))

# Link
fv_df_cont_link_us |>
  ggplot(aes(wind_vel,bwa,z=exp(fitted_global))) +
  geom_contour_filled(breaks=seq(0,2000,by=100)) +
  # labs(title="BBAL") +
  # xlim(0,25) + 
  # ylim(0,1000) +
  facet_wrap(~Species)

# Link: Wind + Intercept
fv_df_cont_link_us |>
  ggplot(aes(wind_vel,exp(fitted_int+fitted_windvel))) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=exp(lower_int+lower_windvel),ymax=exp(upper_int+upper_windvel),y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="Flaps/hour",x="Wind velocity (m/s)")

# Link: Wind + Intercept
fv_df_cont_link_us |>
  ggplot(aes(wind_vel,fitted_windvel)) +
  geom_line() +
  # geom_line(aes(wind_vel,exp(fitted_int+fitted_wind+fitted_id),color=id)) +
  geom_ribbon(mapping=aes(ymin=lower_windvel,ymax=upper_windvel,y=NULL),alpha=0.3) +
  # labs(title="continuous bwa") +
  xlim(0,25) + 
  # ylim(0,1050) +
  theme_minimal() +
  facet_wrap(~Species) +
  labs(y="GAM wind effect",x="Wind velocity (m/s)")


