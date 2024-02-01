################################################################################
#
# Wind data: create plots with wind data.
# This will need to be edited in order to account for times removed by on-water
# and foraging/resting (HMM)
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

locations = c('Bird_Island','Midway')

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(readxl)
library(Matrix)
library(lme4)
library(stringr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(gridExtra)
library(patchwork)

# Set Environment ---------------------------------------------------------

fullmeta <- read_excel("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Full_Metadata.xlsx")
GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"

# Combine all files across field seasons and locations -------------------------
m_all <- 0
for (i_loc in 1:length(locations)) {
  loc = locations[i_loc]
  
  if (loc == "Bird_Island") {
    szns = c("2019_2020","2020_2021","2021_2022")
  } else if (loc == "Midway") {
    szns = c("2018_2019","2021_2022","2022_2023")
  }
  
  for (i_szn in 1:length(szns)) {
    szn = szns[i_szn]
    wind_L4_dir <- paste0(GD_dir,"L4/",loc,"/Wind_Data/",szn,"/")
    setwd(wind_L4_dir)
    files <- list.files(pattern='*.csv')
    
    # skip WAAL files
    indices_to_remove <- grep("^WAAL", files)
    if (length(indices_to_remove != 0)) {
      files <- files[-indices_to_remove]
    }
    
    # combine all files and label them with the szn
    for (i in 1:length(files)) {
      bird <- str_sub(files[i],1,-17)
      birdmeta <- fullmeta %>% dplyr::filter(Deployment_ID == bird)
      
      # only use the file if metadata indicates that the bird is usable
      if (birdmeta$Focus == 1) {
        m <- read.csv(files[i])
        m$szn = szn
        m$loc = loc
        m$Trip_Type = birdmeta$Trip_Type
        m$spp = birdmeta$Species
        
        if (length(m_all)==1) {
          m_all <- m
        } else {
          m_all <- rbind(m_all,m)
        }
      }
    }
  }
}

# Turn variables into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 
m_all$szn <- as.factor(m_all$szn)
m_all$loc <- as.factor(m_all$loc)
m_all$Trip_Type <- as.factor(m_all$Trip_Type)
m_all$spp <- as.factor(m_all$spp)

# Re-order spp groups
m_all$spp <- factor(m_all$spp , levels=c("BBAL", "GHAL", "BFAL", "LAAL"))

# Classify 2BEP, E_pip, Ep as BG
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="2BEP","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="E_pip","BG")))
m_all <- m_all %>% mutate(Trip_Type = factor(replace(as.character(Trip_Type),Trip_Type=="Ep","BG")))

# Datetime stuff
m_all$datetime <- as.POSIXlt(m_all$datetime,format="%Y-%m-%d %H:%M:%S")
m_all$julian <- m_all$datetime$yday + 1 
# plotday are the days since the beginning of the year (January 1 of the first
# year is 1)
m_all$plotday <- ifelse(m_all$julian > 200, m_all$julian, m_all$julian + 365)
# adjust for leap years
m_all$plotday <- ifelse(m_all$plotday > 365 & m_all$datetime$year+1900 == 2021, m_all$plotday+1, m_all$plotday)

# Remove unnecessary columns
# m_all <- m_all %>% dplyr::select(-lon,-lat,-u,-v,-datetime,-wind_dir,-bird_dir,-bird_vel)


# Split data between species
m_BBAL <- m_all %>% filter(spp=="BBAL")
m_GHAL <- m_all %>% filter(spp=="GHAL")
m_LAAL <- m_all %>% filter(spp=="LAAL")
m_BFAL <- m_all %>% filter(spp=="BFAL")



# Flap plots --------------------------------------------------------------

# Scatter + LOESS
scatter_plot <- ggplot(m_all, aes(x=wind_vel,y=flaps)) + 
  geom_point() +
  geom_smooth(method="loess") + 
  labs(title = "Scatter plots split by spp", x="Wind Velocity", y="Flaps per hour") +
  theme_minimal() +
  facet_grid(.~spp,scales="free",space="free_x")

scatter_plot


gam_plot <- ggplot(m_all, aes(x=wind_vel,y=flaps)) + 
  geom_point() +
  geom_smooth(method="gam",formula=y~s(x))+ #s(id,bs='re')) + 
  labs(title = "GAM + scatter plots split by spp", x="Wind Velocity", y="Flaps per hour") +
  theme_minimal() +
  facet_grid(.~spp,scales="free",space="free_x")

gam_plot


# Cross-winds
gam_plot_cross <- ggplot(m_all %>% filter(bwa>45 & bwa<135), aes(x=wind_vel,y=flaps)) + 
  geom_point() +
  geom_smooth(method="gam",formula=y~s(x))+ #s(id,bs='re')) + 
  labs(title = "GAM + scatter plots split by spp (cross-winds)", x="Wind Velocity", y="Flaps per hour") +
  theme_minimal() +
  facet_grid(.~spp,scales="free",space="free_x")

gam_plot_cross

gam_plot_head <- ggplot(m_all %>% filter(bwa<=45), aes(x=wind_vel,y=flaps)) + 
  geom_point() +
  geom_smooth(method="gam",formula=y~s(x))+ #s(id,bs='re')) + 
  labs(title = "GAM + scatter plots split by spp (head-winds)", x="Wind Velocity", y="Flaps per hour") +
  theme_minimal() +
  facet_grid(.~spp,scales="free",space="free_x")

gam_plot_head


gam_plot_tail <- ggplot(m_all %>% filter(bwa>=135), aes(x=wind_vel,y=flaps)) + 
  geom_point() +
  geom_smooth(method="gam",formula=y~s(x))+ #s(id,bs='re')) + 
  labs(title = "GAM + scatter plots split by spp (tail-winds)", x="Wind Velocity", y="Flaps per hour") +
  theme_minimal() +
  facet_grid(.~spp,scales="free",space="free_x")

gam_plot_tail

# BBAL models
GAM_BBAL <- gam(flaps ~ s(wind_vel), data=m_BBAL)
tempdata <- data.frame(wind_vel=seq(0,25,0.5))
predict.gam(GAM_BBAL,tempdata,se.fit=TRUE)


plot(GAM_BBAL)

GAM_BBAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'),data=m_BBAL)
plot(GAM_BBAL_rnd)
summary(GAM_BBAL_rnd)

GAM_LAAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_LAAL)
plot(GAM_LAAL_rnd)
summary(GAM_LAAL_rnd)

GAM_BFAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_BFAL)
plot(GAM_BFAL_rnd)
summary(GAM_BFAL_rnd)

GAM_LAAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_LAAL)
plot(GAM_LAAL_rnd)
summary(GAM_LAAL_rnd)

#gamm() to allow for indivudal as a random effect






GAM_BBAL <- getViz(GAM_BBAL)
plot_GAM_BBAL <- plot( sm(GAM_BBAL, 1) )
plot_GAM_BBAL <- plot_GAM_BBAL + l_fitLine(colour = "red") + 
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BBAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")
plot_GAM_BBAL



## why are there so many scatter plot points in the negatives? ? 


GAM_BBAL_rnd <- gam(flaps ~ s(wind_vel) + s(id, bs = 're'), data=m_BBAL)
GAM_BBAL_rnd <- getViz(GAM_BBAL_rnd)
plot_GAM_BBAL_rnd <- plot( sm(GAM_BBAL_rnd, 1) )
plot_GAM_BBAL_rnd <- plot_GAM_BBAL_rnd + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BBAL (rnd)", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")
plot_GAM_BBAL_rnd


# LAAL model
GAM_LAAL <- gam(flaps ~ s(wind_vel), data=m_LAAL)
GAM_LAAL <- getViz(GAM_LAAL)
plot_GAM_LAAL <- plot( sm(GAM_LAAL, 1) )
plot_GAM_LAAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "LAAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")

# BFAL model
GAM_BFAL <- gam(flaps ~ s(wind_vel), data=m_BFAL)
GAM_BFAL <- getViz(GAM_BFAL)
plot_GAM_BFAL <- plot( sm(GAM_BFAL, 1) )
plot_GAM_BFAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "BFAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")

# LAAL model
GAM_LAAL <- gam(flaps ~ s(wind_vel), data=m_LAAL)
GAM_LAAL <- getViz(GAM_LAAL)
plot_GAM_LAAL <- plot( sm(GAM_LAAL, 1) )
plot_GAM_LAAL + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  labs(title = "LAAL", x="Wind Velocity", y="Flaps per hour") +
  theme_classic() +
  geom_hline(yintercept = 0,linetype="solid",color="black")




# Wind angle plots -------------------------------------------------------------

# set breaks
breaks <- seq(0,360,by=5)
categories <- cut(m_all$w_rel,breaks,include.lowest=TRUE,right=FALSE)
frequency_table <- table(categories)
data <- as.data.frame(frequency_table)
data$categories <- breaks[-length(breaks)]

polar_plot <- ggplot(data, aes(x = categories, y = Freq)) + #, group=3)) +  #, group = day, color = day)) +
  geom_line() +
  coord_polar(start=0) 
  # + scale_y_continuous(limits=c(0,75))
             
polar_plot

polar_plot_bar <- ggplot(data, aes(x = categories, y = Freq)) +
  # geom_bar(stat = "identity", width = 1, fill = "skyblue") +
  geom_bar(stat="identity", alpha=0.5, fill="blue") + 
  coord_polar(start = 0)  # Adjust the starting angle if needed
  # scale_y_continuous(limits=c(0,1200))
  
polar_plot_bar


# Flap plots --------------------------------------------------------------

scatter_plot <- ggplot(m_all, aes(x=wind_vel,y=flaps)) +
  geom_point()

scatter_plot




# this gets rid of headwinds (setting bwa>60)
m_selectedWinds <- filter(m_all,bwa>60)

# do i separate into inc and BG? 

lme1<-lmer(flaps~wind_vel+(1+wind_vel|spp)+(1+wind_vel|id),data=m_selectedWinds)

# Ok, so including both spp and id leads to singularity
# lme1<-lmer(flaps~wind_vel + (1|spp) + (1|id),data=m_selectedWinds)

lme1 <- lmer(flaps~wind_vel + (1|id),data=m_selectedWinds)

predicted_values <- predict(lme1, newdata = m_selectedWinds, re.form = NULL)

sjPlot::plot_model(lme1, type = "pred", terms = c("wind_vel"), show.ci = TRUE)

combined_plot <- scatter_plot +
  geom_line(aes(y = predicted_values), color = "red", linewidth = 1) +
  labs(title = "Scatter Plot with LME Model")

combined_plot













WS_breaks <- seq(0,max(m_selectedWinds$wind_vel))
WS_categories <- cut(m_selectedWinds$wind_vel,WS_breaks,include.lowest=TRUE,right=FALSE)

# count flaps within these categories!
WS_freqtable <- table(WS_categories)
WS_data <- as.data.frame(WS_freqtable)

## I THINK I NEED TO MAKE A GAM TO FIT THIS......NICE 

# chunk wind vel and average flaps?






