# Takes L1_2 GPS data and converts it to the standard format for BirdLife

# Clear environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# Set environment --------------------------------------------------------------

fullmeta <- read_excel("/Users/ian/Desktop/Full_Metadata.xlsx")
dep_data <- read_csv("/Users/ian/Desktop/BirdIsland2022_Deployment_Data.csv")
GPS_dir <- "/Users/ian/Desktop/2022_WAAL/original/"
setwd(GPS_dir)
files <- list.files(pattern='*.csv')
write_dir <- "/Users/ian/Desktop/2022_WAAL/formatted/"

# Loop thru files --------------------------------------------------------------

for (i in 1:length(files)) {
  
  # Read file and find metadata
  m <- read_csv(files[i])
  
  # There are some individuals that were misnamed 
  if (m$id[1] == "WAAL_20220129_WB25") {
    birdname <- "WAAL_20220129_WB24"
  } else if (m$id[1] == "WAAL_20220310_BP45") {
    birdname <- "WAAL_20220310_BP43"
  } else {
    birdname <- m$id[1]
  }
  
  birdmeta <- fullmeta %>% filter(Deployment_ID == birdname)
  bird_dep_data <- dep_data %>% filter(Bird_Num == birdmeta$Bird_Num)
  
  # Create dataframe
  m <- m %>% mutate(sex = case_when(bird_dep_data$Sex == "F" ~ "female",
                                    bird_dep_data$Sex == "M" ~ "male"))
  # m$age <- bird_dep_data$Age # I gotta convert these to strings
  m$age <- "adult"
  m <- m %>% mutate(breed_stage = case_when(birdmeta$Trip_Type == "Inc" ~ "incubation",
                                            birdmeta$Trip_Type == "BG" ~ "brood-guard"))
  m$bird_id <- m$id
  m$track_id <- m$tripID
  m$date_gmt_dash <- strptime(date(m$datetime),'%Y-%m-%d',tz="UTC")
  m$date_gmt <- format(m$date_gmt_dash,'%d/%m/%Y')
  m$time_gmt <- as_hms(m$datetime)
  m$latitude <- m$lat
  m$longitude <- m$lon
  m$equinox <- NA
  m$argos_quality <- NA
    
  m_formatted <- m %>% select(bird_id,sex,age,breed_stage,track_id,date_gmt,
                              time_gmt,latitude,longitude,equinox,argos_quality)
  
  # write file
  write_csv(m_formatted,paste0(write_dir,birdname,".csv"))
  
  if (i==1) {
    m_f_compiled <- m_formatted
  } else {
    m_f_compiled <- rbind(m_f_compiled,m_formatted)
  }         
}

# save compiled version
write_csv(m_f_compiled,paste0(write_dir,"all_2022_WAAL.csv"))

sum(is.na(m_f_compiled[,1:9]))
