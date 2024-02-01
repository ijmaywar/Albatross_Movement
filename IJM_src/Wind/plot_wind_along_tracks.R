################################################################################
#
# Create plots from L2 wind data that show the winds experienced by albatrosses
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
library(sjPlot)
library(cowplot)

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
    wind_L2_dir <- paste0(GD_dir,"L2/",loc,"/Wind_Data/",szn,"/")
    setwd(wind_L2_dir)
    files <- list.files(pattern='*.csv')
    
    # skip WAAL files
    indices_to_remove <- grep("^WAAL", files)
    if (length(indices_to_remove != 0)) {
      files <- files[-indices_to_remove]
    }
    
    # combine all files and label them with the szn
    for (i in 1:length(files)) {
      bird <- str_sub(files[i],1,-11)
      birdmeta <- fullmeta %>% dplyr::filter(Deployment_ID == bird)
      
      # only use the file if metadata indicates that the bird is usable and that
      # GPS recorded the entire trip
      if (birdmeta$Focus == 1 && birdmeta$Pos_complete == "TRUE") {
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






# box plot of experienced winds
ggplot(m_all, aes(x = spp, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "All birds", x = "Species", y = "Wind velocity") +
  theme_minimal()

# Midway by seasons
ggplot(m_all %>% filter(spp=="BFAL"|spp=="LAAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "Midway birds", x = "Field season", y = "Wind velocity") +
  theme_minimal()

# Bird_Island by seasons
ggplot(m_all %>% filter(spp=="BBAL"|spp=="GHAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "Bird Island birds", x = "Field season", y = "Wind velocity") +
  theme_minimal()




# By spp and szn ----------------------------------------------------------

# BFAL by season
plot_BFAL <- ggplot(m_all %>% filter(spp=="BFAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "BFAL", x = "Field season", y = "Wind velocity") +
  theme_minimal() +
  ylim(0,20)

# LAAL by season
plot_LAAL <- ggplot(m_all %>% filter(spp=="LAAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "LAAL", x = "Field season", y = "Wind velocity") +
  theme_minimal() +
  ylim(0,20)

# BBAL by season
plot_BBAL <- ggplot(m_all %>% filter(spp=="BBAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "BBAL", x = "Field season", y = "Wind velocity") +
  theme_minimal() +
  ylim(0,20)

# GHAL by season
plot_GHAL <- ggplot(m_all %>% filter(spp=="GHAL"), aes(x = szn, y = wind_vel, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "GHAL", x = "Field season", y = "Wind velocity") +
  theme_minimal() +
  ylim(0,20)

# Arrange into a row
plot_grid(plot_BBAL, plot_GHAL, plot_BFAL, plot_LAAL, nrow = 1)



# By spp and szn (DATES) --------------------------------------------------

# DATE: BFAL by season
plot_BFAL_dates <- ggplot(m_all %>% filter(spp=="BFAL"), aes(x = szn, y = plotday, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "BFAL", x = "Field season", y = "Days since beginning of the calendar year") +
  theme_minimal() +
  ylim(320,410)

# DATES: LAAL by season
plot_LAAL_dates <- ggplot(m_all %>% filter(spp=="LAAL"), aes(x = szn, y = plotday, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "LAAL", x = "Field season", y = "Days since beginning of the calendar year") +
  theme_minimal() +
  ylim(320,410)

# BBAL by season
plot_BBAL_dates <- ggplot(m_all %>% filter(spp=="BBAL"), aes(x = szn, y = plotday, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "BBAL", x = "Field season", y = "Days since beginning of the calendar year") +
  theme_minimal() +
  ylim(320,410)

# GHAL by season
plot_GHAL_dates <- ggplot(m_all %>% filter(spp=="GHAL"), aes(x = szn, y = plotday, fill=Trip_Type)) +
  geom_boxplot() +
  labs(title = "GHAL", x = "Field season", y = "Days since beginning of the calendar year") +
  theme_minimal() +
  ylim(320,410)

# Arrange into a row
plot_grid(plot_BBAL_dates, plot_GHAL_dates, plot_BFAL_dates, plot_LAAL_dates, nrow = 1)







# Look thru and filter via species
m_BBAL <- m_all %>% filter(spp=="BBAL")
m_GHAL <- m_all %>% filter(spp=="GHAL")
m_BFAL <- m_all %>% filter(spp=="BFAL")
m_LAAL <- m_all %>% filter(spp=="LAAL")

# Midway by seasons
m_Midway_18_19 <- m_all %>% filter((spp=="BFAL"|spp=="LAAL") & (szn=="2018_2019"))
mean(m_Midway_18_19$wind_vel)



