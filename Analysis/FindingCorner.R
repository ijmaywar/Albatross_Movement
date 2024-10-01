cutoff <- .05

temp <- m_all_nonaflaps %>% filter(shts<=quantile(m_all_nonaflaps$shts,probs=cutoff,na.rm=TRUE),
                         wind_vel_kmh<=quantile(m_all_nonaflaps$wind_vel_kmh,probs=cutoff,na.rm=TRUE))

m_lowflaps <- m_all_nonaflaps %>% filter(flaps<=quantile(m_all_nonaflaps$flaps,probs=cutoff,na.rm=TRUE))

m_hiflaps <- m_all_nonaflaps %>% filter(flaps>=quantile(m_all_nonaflaps$flaps,probs=1-cutoff,na.rm=TRUE))

# All data for GAMs
ggplot(m_all_nonaflaps,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  ylim(0,100) + 
  xlim(0,8)

# Filtered data
ggplot(temp,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  ylim(0,100) + 
  xlim(0,8)

# How many points
nrow(temp)

# Flap rate distribution of all data
hist(m_all_nonaflaps$flaps)
mean(m_all_nonaflaps$flaps)

# Flap rate distribution of all data
hist(temp$flaps)
mean(temp$flaps)

################################################################################

# All data for GAMs
ggplot(m_all_nonaflaps,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  ylim(0,100) + 
  xlim(0,8)

# Where do the env predictors fall for the lowest flap rates
ggplot(m_lowflaps,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  ylim(0,100) + 
  xlim(0,8)

# Where do the env predictors fall for the highest flap rates
ggplot(m_hiflaps,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  ylim(0,100) + 
  xlim(0,8)

# Where do the env predictors fall for the lowest flap rates for each species individually
ggplot(m_all_nonaflaps,aes(shts,wind_vel_kmh)) +
  geom_point() + 
  geom_point(m_lowflaps,mapping=aes(shts,wind_vel_kmh),color='red') +
  ylim(0,100) + 
  xlim(0,8) +
  facet_wrap(~Species,nrow = 1)

mean(m_him_all_nonaflapsmean(m_hiflaps$flaps))
mean(m_all_nonaflaps$flaps)

################################################################################
# Systematically find theoretical max wingbeats for all species

cutoff <- .05

meta_corner <- data.frame(spp = character(),
                          num_rows = numeric(),
                          num_rows_corner = numeric(),
                          perc_rows_corner = numeric(),
                          avg_flaps = numeric(),
                          avg_flaps_corner = numeric())

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,shts)
  
  # Normalize wind_vel_kmh and shts
  m_current$norm_wind_vel_kmh <- (m_current$wind_vel_kmh - min(m_current$wind_vel_kmh)) /  
    (max(m_current$wind_vel_kmh) - min(m_current$wind_vel_kmh))
  
  m_current$norm_shts <- (m_current$shts - min(m_current$shts)) /  
    (max(m_current$shts) - min(m_current$shts))
  
  # Create score of both magnitudes
  m_current$norm_score <- m_current$norm_wind_vel_kmh + m_current$norm_shts
  
  # m_corner <- m_current %>% filter(norm_score<=quantile(m_current$norm_score,probs=cutoff))
  m_corner <- m_current %>% filter(norm_score<=quantile(m_current$norm_score,probs=cutoff))
  
  spp_meta <- c(spp,
                nrow(m_current),
                nrow(m_corner),
                100*(nrow(m_corner)/nrow(m_current)),
                mean(m_current$flaps,na.rm=TRUE),
                mean(m_corner$flaps,na.rm=TRUE))
  
  
  meta_corner <- rbind(meta_corner,spp_meta)

}

colnames(meta_corner) <- c("spp","num_rows","num_rows_corner",
                           "perc_rows_corner","avg_flaps","avg_flaps_corner")
  
meta_corner

# This does a pretty solid job of finding max flap rate for each species but NOT FOR BFAL

# We can't check GLS == wet for BFAL because they weren't deployed

# Find the quantile values for windspeed and shts

  