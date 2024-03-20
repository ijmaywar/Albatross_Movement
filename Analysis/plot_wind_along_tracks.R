################################################################################
#
# Create plots from L2 wind data that show the winds experienced by albatrosses
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputted Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(readxl)
library(Matrix)
library(lme4)
library(stringr)
library(dplyr)
library(sjPlot)

# Set Environment ---------------------------------------------------------

fullmeta <- read_excel("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Full_Metadata.xlsx")

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/"
wind_L4_dir <- paste0(GD_dir,"L4/",location,"/Wind_Data/",szn,"/")

setwd(wind_L4_dir)
files <- list.files(pattern='*.csv')
all_trips <- sub("_wind_and_flaps.csv$","",files)

for (i in 1:length(files)) {
  m <- read.csv(files[i])
  if (i==1) {
    m_all <- m
  } else {
    m_all <- rbind(m_all,m)
  }
  
}

# Add metadata to all rows and remove some extra columns -----------------------

m_all <- m_all %>% dplyr::select(-lon,-lat,-u,-v,-datetime,-wind_dir,-bird_dir,-bird_vel)
m_all$triptype <- character(nrow(m_all))

all_birds <- unique(m_all$id)

for (i in 1:length(all_birds)) {
  birdmeta <- fullmeta %>% dplyr::filter(Deployment_ID == all_birds[i])
  m_all <- m_all %>% mutate(triptype = if_else(id == all_birds[i],birdmeta$Trip_Type,triptype))  
}

# Add a species column and make it a factor
m_all$spp <- str_sub(m_all$id,1,4)
m_all$spp <- as.factor(m_all$spp)

# Turn individual and trip ID into factors
m_all$id <- as.factor(m_all$id)
m_all$tripID <- as.factor(m_all$tripID) 



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

# this gets rid of headwinds (setting bwa>60)
m_selectedWinds <- filter(m_all,bwa>60)

# do i separate into inc and BG? 

lme1<-lmer(flaps~wind_vel+(1+wind_vel|spp)+(1+wind_vel|id),data=m_selectedWinds)

# Ok, so including both spp and id leads to singularity
# lme1<-lmer(flaps~wind_vel + (1|spp) + (1|id),data=m_selectedWinds)

lme1 <- lmer(flaps~wind_vel + (1|id),data=m_selectedWinds)

predicted_values <- predict(lme1, newdata = m_selectedWinds, re.form = NULL)

sjPlot::plot_model(lme1, type = "pred", terms = c("wind_vel"), show.ci = TRUE)

scatter_plot <- ggplot(m_selectedWinds, aes(x=wind_vel,y=flaps)) +
  geom_point()

scatter_plot

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






