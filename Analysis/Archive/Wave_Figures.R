################################################################################
# How does wind velocity compare to swell and wave height? ---------------------

# Swells
fig_ws_shts <- ggplot(m_all_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel_kmh,y=shts),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Total swell (m)") +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Wind waves
fig_ws_shww <- ggplot(m_all_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=shww),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Wind waves (m)") +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Total waves
fig_ws_swh <- ggplot(m_all_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=wind_vel,y=swh),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  labs(x="Windspeed (km/h)", y="Surface sea waves (m)") +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Wrap windspeed vs wave height figs
wrap_elements(panel = fig_ws_shts / fig_ws_shww / fig_ws_swh) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")

# Total waves vs. swell + ww:
# It looks like total waves isn't exactly the addition of swell and wind components.
# This is probably because of different periods and directions
ggplot(m_all_poscomplete %>% filter((HMM_3S_state != 1))) +
  geom_point(aes(x=shww+shts,y=swh),size=0.001,alpha=1,color="black") + 
  facet_wrap(~Species,nrow = 1) + 
  theme_bw()


# Use histograms to show that shts > shww
# Try to justify from the literature that waves need to be a certain height to be useful
# for wave slope soaring

# Categorize wave heights
# Reshape data to long format
wave_height_long <- melt(m_all_poscomplete %>% dplyr::select(Species,shts,shww,swh), 
                  id.vars = "Species", variable.name = "Wave_type", value.name = "Wave_height")

# Create the boxplot
ggplot(wave_height_long, aes(x = Species, y = Wave_height, fill = Wave_type)) +
  scale_fill_brewer(palette = "Set1", labels = c("Total swell", "Wind waves", "Surface sea waves")) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Species",
       y = "Significant wave height (m)") +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Wave type"))


# VERY HIGH correlation between wind_vel and shww because wind cause wind waves.
# Pretty high correlation between wind_vel and swh because wind waves are a component of total waves.
# No/low correlation between wind_vel and shts because wind_vel doesn't directly effect swell.

wind_vel_kmh_box <- m_all_poscomplete |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_boxplot(width=0.4) +
  theme_bw()

swh_box <- m_all_poscomplete |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) + 
  theme_bw()

shts_box <- m_all_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) +
  theme_bw()

shww_box <- m_all_poscomplete |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,13) +
  theme_bw()

# Display all 4 figs
wrap_elements(panel = swh_box | shts_box | shww_box | wind_vel_kmh_box)


mwp_box <- m_all_poscomplete |>
  ggplot(aes(Species,mwp)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

mpts_box <- m_all_poscomplete |>
  ggplot(aes(Species,mpts)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

mpww_box <- m_all_poscomplete |>
  ggplot(aes(Species,mpww)) +
  geom_boxplot(width=0.4) +
  ylim(0,17) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = mwp_box | mpts_box | mpww_box)




# Flapping rate is generally lower for NP species
flap_box_all_winds <- m_all %>% filter((HMM_3S_state != 1)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="All winds") + 
  theme_bw()
# At low windspeeds, NP birds are flapping less
flap_box_low_winds <- m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="Low winds") + 
  theme_bw()
# At high windspeeds, all birds are flapping at a similar rate
flap_box_high_winds <- m_all %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,flaps)) +
  geom_boxplot(width=0.4) +
  ylim(0,1000) +
  labs(main="High winds") + 
  theme_bw()

# Display all 3 figs
wrap_elements(panel = flap_box_all_winds | flap_box_low_winds | flap_box_high_winds)


# At low windspeeds, the height of swells == total wave height and are 
# greater for NP spp
low_winds_swh <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
low_winds_shts <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
low_winds_shww <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh<25)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = low_winds_swh | low_winds_shts | low_winds_shww)

# At high windspeeds, swh is less similar to shts and 
high_winds_swh <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,swh)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
high_winds_shts <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shts)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()
high_winds_shww <- m_all_poscomplete %>% filter((HMM_3S_state != 1) & (wind_vel_kmh>50)) |>
  ggplot(aes(Species,shww)) +
  geom_boxplot(width=0.4) +
  ylim(0,12.5) +
  theme_bw()

# Display all 3 figs
wrap_elements(panel = high_winds_swh | high_winds_shts | high_winds_shww)
