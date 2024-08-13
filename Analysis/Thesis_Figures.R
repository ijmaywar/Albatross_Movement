# Figures for thesis

################################################################################
# Figure of mean wind speed during breeding season relative to study site location 

m_all_poscomplete |>
  ggplot(aes(Location,wind_vel_kmh)) +
  geom_boxplot() +
  theme_bw()

################################################################################
# Figure of mean wind speed during breeding season relative to study site location 

wave_height_long <- melt(m_all_poscomplete %>% dplyr::select(Species,shts,shww,swh), 
                         id.vars = "Species", variable.name = "Wave_type", value.name = "Wave_height")

# Create the boxplot
ggplot(data_long, aes(x = Species, y = Wave_height, fill = Wave_type)) +
  # scale_fill_brewer(palette = "Set1", labels = c("Swell", "Wind wave", "Combined")) +
  scale_fill_manual(values = c(paletteer_d("nationalparkcolors::Arches")[1],
                               paletteer_d("nationalparkcolors::Arches")[2],
                               paletteer_d("nationalparkcolors::Arches")[4]),
                    labels = c("Swell", "Wind wave", "Combined")) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Species",
       y = "Significant wave height (m)") +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Wave type"))

################################################################################
# Figures of wind responses by species (as for conference talk)
#   2x5 plot
#   Continuous tensor product model of flapping response (wind*waves)
#   Simple (no direction component) response of flaps to wind

wrap_elements(panel = fig_wind_cont / fig_wind_simple) +
  labs(tag = "Windspeed (km/h)") +
  theme(plot.tag.position = "bottom")


################################################################################
# Figures of swell responses by species (as for wind)
#   2 x 5 plot
#   Continuous tensor product model of flapping response (swell height * direction)
#   Simple (no direction component) response of flaps to swell

wrap_elements(panel = fig_swells_cont / fig_shts_simple) +
  labs(tag = "Swell height (m)") +
  theme(plot.tag.position = "bottom")


################################################################################
# Figures of wind and wave responses together
#   1x 5 plot
#   Continuous tensor product model of flapping response (wave height * wind speed)

# te(wind_vel_kmh,shts), s(id) --------------------------------------

fac_k <- 3
GAM_list_te_wind_vel_shts <- list()

for (spp in spp_vec) {
  
  m_current <- m_all %>% filter((HMM_3S_state != 1) & (Species == spp)) %>% 
    drop_na(wind_vel_kmh,shts)
  
  current_GAM <- gam(formula = flaps ~ te(wind_vel_kmh,shts,k=c(fac_k,fac_k),bs=c('tp','tp')) +
                       s(id,k=length(unique(m_current$id)),bs="re"),
                     data = m_current,
                     family = "poisson",
                     method = "REML")

  GAM_list_te_wind_vel_shts[[spp]] <- current_GAM

  current_ds  <- data_slice(current_GAM, wind_vel_kmh = evenly(wind_vel_kmh, n = 100),
                            shts = evenly(shts, n = 100),
                            id = unique(m_current$id)[1:10])

  link_df <- cbind(current_ds,
                   rep(spp,nrow(current_ds)),
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)","s(id)"))[,4:7],
                   fitted_values(current_GAM, data = current_ds, scale = "link",
                                 terms = c("(Intercept)","te(wind_vel_kmh,shts)"))[,4:7])

  colnames(link_df) <- c("wind_vel_kmh","shts","id","Species",
                         "fitted_all","se_all","lower_all","upper_all",
                         "fitted_global","se_global","lower_global","upper_global")
 
  if (spp == "Black-browed") {
    fv_df_te_wind_vel_shts <- link_df
  } else {
    fv_df_te_wind_vel_shts <- rbind(fv_df_te_wind_vel_shts,link_df)
  }
}

fv_df_te_wind_vel_shts$Species <- factor(fv_df_te_wind_vel_shts$Species, 
 levels=spp_vec)

for (i in 1:5) {
  summary_i <- summary(GAM_list_te_wind_vel_shts[[i]])
  summary_i_df <- as.data.frame(summary_i$s.table,row.names = FALSE)
  summary_i_df$Species <- spp_vec[i]
  if (i==1) {
    summary_df <- summary_i_df
  } else {
    summary_df <- rbind(summary_df,summary_i_df)
  }
}

summary_df


# Create geom_contours with terra wraps
grid_size <- 1000
response_df_mask_all <- list()

for (spp in spp_vec) {
  
  # Create 99% KDEs
  kd_current <- ks::kde(m_all %>% 
                          filter(HMM_3S_state != 1, Species == spp) %>% 
                          dplyr::select(wind_vel_kmh,shts), 
                        compute.cont=TRUE,gridsize = grid_size)
  contour_99_current <- data.frame(with(kd_current, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                 z=estimate, levels=cont["1%"])[[1]]))
  contour_99_current_vect <- as.polygons(as.lines((contour_99_current %>% vect(geom=c('x','y')))))
  
  # Mask GAM response values for plotting
  response_rast_current <- terra::rast(fv_df_te_wind_vel_shts %>% filter(Species == spp) %>% dplyr::select(wind_vel_kmh, shts, fitted_global), type='xyz')
  response_rast_mask_current = terra::mask(response_rast_current, contour_99_current_vect)
  response_df_mask_current = as.data.frame(response_rast_mask_current, xy=T)
  
  # Save values for all spp
  contour_99_current$Species <- spp
  response_df_mask_current$Species <- spp
  
  if (spp == "Black-browed") {
    contour_99_all <- contour_99_current
    response_df_mask_all <- response_df_mask_current
  } else {
    contour_99_all <- rbind(contour_99_all,contour_99_current)
    response_df_mask_all <- rbind(response_df_mask_all,response_df_mask_current)
  }
  
}

response_df_mask_all$Species <- factor(response_df_mask_all$Species, 
 levels=spp_vec)

# Higher res figure
ggplot(response_df_mask_all) +
  # geom_contour_filled(aes(x,y,z=exp(fitted_global)),binwidth=100) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_all$fitted_global),21)) +
  scale_fill_manual(values=inferno(20),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_bw()

# Lower res figure
# purples <- colorRampPalette(c("cornsilk", "darkmagenta"))
ggplot(response_df_mask_all) +
  # geom_contour_filled(aes(x,y,z=exp(fitted_global)),binwidth=100) +
  geom_contour_filled(aes(x,y,z=exp(fitted_global)),
                      breaks=getJenksBreaks(exp(response_df_mask_all$fitted_global),11)) +
  scale_fill_manual(values=magma(10),drop=FALSE) +
  labs(fill = "Flaps/hour") +
  xlim(0,90) +
  ylim(0,7.75) +
  xlab("Windspeed (km/h)") +
  ylab("Significant height of total swell (m)") +
  facet_wrap(~Species,nrow=1) + 
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

################################################################################
# Box of violin plots of wind and waves experienced by species 
# [this and the boxplot below should only represent complete trips; 
# all other analyses can use all available data]
#   2x5 plot

fig_winds <- m_all_poscomplete |>
  ggplot(aes(Species,wind_vel_kmh)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3,outliers=FALSE) +
  labs(y="Windspeed (km/h)") +
  ylim(0,95) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

fig_swells <- m_all_poscomplete |>
  ggplot(aes(Species,shts)) +
  geom_violinhalf(width=1.1,flip=TRUE) + 
  geom_boxplot(width=0.3,outliers=FALSE) +
  labs(y="Significant height of total swell (m)") +
  theme_linedraw() +
  ylim(0,9.5) + 
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

wrap_elements(panel = fig_winds / fig_swells)

################################################################################
# Montly env plots: 2 5x1 figs

# Windspeed km/h
ggplot(compiled_df %>% filter(KDE_type=="all"), 
                            aes(x = Month, y = si10*3.6)) +
  geom_boxplot(width=0.6,outliers = FALSE) +
  labs(y = "Windspeed (km/h)") +
  scale_x_discrete(labels = c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.",
                              "Sep.","Oct.","Nov.","Dec.")) +
  theme_linedraw() +
  facet_wrap(~Species, nrow=5) + 
  theme(axis.title.x = element_blank(),
        strip.text = element_blank())

# Swell height m
ggplot(compiled_df %>% filter(KDE_type=="all"), 
       aes(x = Month, y = shts)) +
  geom_boxplot(width=0.6,outliers = FALSE) +
  labs(y = "Significant height of total swell (m)") +
  scale_x_discrete(labels = c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.",
                              "Sep.","Oct.","Nov.","Dec.")) +
  theme_linedraw() +
  facet_wrap(~Species, nrow=5) + 
  theme(axis.title.x = element_blank(),
        strip.text = element_blank())

################################################################################
# Box plots of prop. time spent in high/med/low winds AND wave heights

# Winds
bird_wind_angle_cat_hist_data <- as.data.frame(m_all_poscomplete %>%
                                                 drop_na(bird_wind_angle_cat) %>% 
                                                 group_by(Location,Species,id,bird_wind_angle_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count)))


# Randomly downsample m_all_poscomplete such that there are 34 individuals for each study site.
m_all_poscomplete_spp_ds <- rbind(
  m_all_poscomplete %>% filter(Species == "Black-browed") %>% filter(id %in% sample(unique(id), 11)),
  m_all_poscomplete %>% filter(Species == "Grey-headed") %>% filter(id %in% sample(unique(id), 12)),
  m_all_poscomplete %>% filter(Species == "Wandering") %>% filter(id %in% sample(unique(id), 11)),
  m_all_poscomplete %>% filter(Species == "Black-footed"),
  m_all_poscomplete %>% filter(Species == "Laysan") %>% filter(id %in% sample(unique(id), 17)))

wind_vel_kmh_breaks <- quantile(m_all_poscomplete_spp_ds$wind_vel_kmh, probs=c((1/3),(2/3)))
shts_cat_breaks <- quantile(m_all_poscomplete_spp_ds$shts, probs=c((1/3),(2/3)))

m_all_poscomplete <- m_all_poscomplete %>% 
  mutate(shts_cat = case_when(shts<shts_cat_breaks[[1]] ~ "low",
                              shts>=shts_cat_breaks[[1]] & shts<shts_cat_breaks[[2]] ~ "medium",
                              shts>=shts_cat_breaks[[2]] ~ "high"),
         wind_vel_kmh_cat = case_when(wind_vel_kmh<wind_vel_kmh_breaks[[1]] ~ "low",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[1]] & wind_vel_kmh<wind_vel_kmh_breaks[[2]] ~ "medium",
                                      wind_vel_kmh>=wind_vel_kmh_breaks[[2]] ~ "high"))

wind_vel_kmh_cat_density_data <- as.data.frame(m_all_poscomplete %>% group_by(Location,Species,id,wind_vel_kmh_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count),
                                                        wind_vel_kmh_cat = factor(wind_vel_kmh_cat,levels=c("low","medium","high"))))

shts_cat_density_data <- as.data.frame(m_all_poscomplete %>% group_by(Location,Species,id,shts_cat) %>% 
                                         summarize(count=n()) %>% 
                                         mutate(proportion = count/sum(count),
                                                shts_cat = factor(shts_cat,levels=c("low","medium","high"))))


windspeed_prop <- ggplot(wind_vel_kmh_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=wind_vel_kmh_cat),outliers = FALSE) +
  scale_fill_manual(values = rev(reds(3)),labels = c(paste0("Low: (0,",round(wind_vel_kmh_breaks[[1]],3),") km/h"), 
                                                paste0("Medium: [",round(wind_vel_kmh_breaks[[1]],3),",",round(wind_vel_kmh_breaks[[2]],3),") km/h"),
                                                paste0("High: [",round(wind_vel_kmh_breaks[[2]],3),",",round(max(m_all_poscomplete$wind_vel_kmh),3),"] km/h"))) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Windspeed category"))

wave_height_prop <- ggplot(shts_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=shts_cat),outliers = FALSE) +
  scale_fill_manual(values = rev(blues(3)),labels = c(paste0("Low: (0,",round(shts_cat_breaks[[1]],3),") m"), 
                                                     paste0("Medium: [",round(shts_cat_breaks[[1]],3),",",round(shts_cat_breaks[[2]],3),") m"),
                                                     paste0("High: [",round(shts_cat_breaks[[2]],3),",",round(max(m_all_poscomplete$shts),3),"] m"))) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Swell height category"))


wrap_elements(panel = windspeed_prop / wave_height_prop)


ggplot(wind_vel_kmh_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=wind_vel_kmh_cat),alpha=0.5) +
  # scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Windspeed")) +
  theme(text = element_text(size = 24))

ggplot(shts_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=shts_cat),alpha=0.5) +
  # scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Swell height")) +
  theme(text = element_text(size = 24))





################################################################################
# Box or violin plots [or density plots?] of prop. time spent in 
# high/ side/ tail winds AND wave direction [with/  against/ across wave direction] 

# Winds
bird_wind_angle_cat_hist_data <- as.data.frame(m_all_poscomplete %>%
                                                 drop_na(bird_wind_angle_cat) %>% 
                                                 group_by(Location,Species,id,bird_wind_angle_cat) %>% 
                                                 summarize(count=n()) %>% 
                                                 mutate(proportion = count/sum(count)))

# Swells
swell_bird_angle_cat_density_data <- as.data.frame(m_all_poscomplete %>%
                                                     drop_na(bird_swell_angle_cat) %>% 
                                                     group_by(Location,Species,id,bird_swell_angle_cat) %>% 
                                                     summarize(count=n()) %>% 
                                                     mutate(proportion = count/sum(count)))


dir_cat_cols <- c("#9484B1FF", "#F1C100FF","#496849FF")
# Wind boxplot
wind_dir_prop <- ggplot(bird_wind_angle_cat_hist_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=bird_wind_angle_cat), outliers=FALSE) +
  scale_fill_manual(values = dir_cat_cols,labels = c("Head", "Cross", "Tail")) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Wind direction category"))

# Wave boxplot
wave_dir_prop <- ggplot(swell_bird_angle_cat_density_data) +
  geom_boxplot(aes(x=Species,y=proportion,fill=bird_swell_angle_cat), outliers=FALSE) +
  scale_fill_manual(values = dir_cat_cols, labels = c("Against", "Across", "With")) +
  labs(y="Proportion of time") +
  ylim(0,1) +
  theme_linedraw() + 
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  guides(fill=guide_legend(title="Swell direction category"))

wrap_elements(panel = wind_dir_prop / wave_dir_prop)


# Density figure
ggplot(bird_wind_angle_cat_hist_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=bird_wind_angle_cat),alpha=0.5) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia") + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Relative wind")) +
  theme(text = element_text(size = 24))

# Density plot
ggplot(swell_bird_angle_cat_density_data) +
  facet_wrap(~Species) +
  geom_density(aes(x=proportion,fill=bird_swell_angle_cat),alpha=0.5) +
  scale_fill_manual(values=cat_hist_colors) + 
  labs(y="Density") +
  theme_bw() +
  scale_x_continuous(name ="Proportion of time", 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".5",".75","1"),
                     limits = c(0,1)) + 
  theme(legend.position = c(0.85, 0.2), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = NA, colour = NA)) +
  guides(fill=guide_legend(title="Relative swell")) +
  theme(text = element_text(size = 24))

################################################################################
# Table of sample size - no. accel + GPS deployments by species 

# poscomplete are when GPS tag writes for the entire trip, ignores acc data.
# This is for summarizing IDs
merge(as.data.frame(m_all %>% group_by(Species) %>% summarize(IDs=n_distinct(id))),
               as.data.frame(m_all_poscomplete %>% group_by(Species) %>% summarize(IDs_poscomplete=n_distinct(id))),
               by = "Species")

# This is for summarizing tripIDs
merge(as.data.frame(m_all %>% group_by(Species) %>% summarize(tripIDs=n_distinct(tripID))),
               as.data.frame(m_all_poscomplete %>% group_by(Species) %>% summarize(tripIDs_poscomplete=n_distinct(tripID))),
               by = "Species")


################################################################################
# Table of deployments based on tag combinations

m_all %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

temp <- m_all %>% 
  filter(HMM_3S_state!=1) %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

m_all_poscomplete %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

m_all_poscomplete %>% 
  filter(HMM_3S_state!=1) %>% 
  group_by(Species,Location,Pos_TagType,Aux_TagType) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Location,Species,Pos_TagType,Aux_TagType)

################################################################################
# Table of deployments based on field season

temp <- m_all %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

temp <- m_all_poscomplete %>% 
  group_by(Species,Field_Season) %>% 
  summarize(unique_IDs=n_distinct(id),n=n()) %>% 
  arrange(Species)

clipr::write_clip(temp)

################################################################################
# Supplemental figures and tables
################################################################################

################################################################################
# Pie graphic ------------------------------------------------------------------

pie_colors <- c("#9484B1FF", "#F1C100FF","#496849FF", "#F1C100FF")
ggplot(data.frame(cat=factor(c("head","cross","tail","cross_2"),levels=c("head","cross","tail","cross_2")),
                  value=c(6,3,6,3)),
       aes(x="",y=value,fill=cat)) +
  geom_bar(width=1,stat="identity") +
  coord_polar(theta="y",start=60*(pi/180)) +
  scale_fill_manual(values=pie_colors) +
  theme_void() +
  guides(fill=FALSE)


################################################################################
# Categorical GAMs
fig_wind_cat
fig_swells_cat
################################################################################

################################################################################
# Box or violin plots [or density plots?] of prop. time spent in 
# low/ medium/ high wind and waves (using same breakpoints across species; 
# earlier we discussed randomly sampling the same no. of BBAL, GHAL and 
# WAAL points as BFAL and LAAL points to generate an overall distribution of 
# wave heights experienced across all species, and picking, say the 33 and 66 
# quantiles to distinguish between low/ med/ high)
#   2x5 plot


# # Randomly downsample m_all_poscomplete such that there are 48 individuals for each location.
# m_all_poscomplete_loc_ds <- rbind(
#   m_all_poscomplete %>% filter(Location == "Bird_Island") %>% filter(id %in% sample(unique(id), 48)),
#   m_all_poscomplete %>% filter(Location == "Midway"))
# 
# quantile(m_all_poscomplete_loc_ds$shts, probs=c((1/3),(2/3)))

