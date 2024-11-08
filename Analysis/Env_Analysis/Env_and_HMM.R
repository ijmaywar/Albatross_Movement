m_all |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3,outliers=FALSE) +
  labs(y="Windspeed (km/h)") +
  ylim(0,95) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~HMM_3S_state)
