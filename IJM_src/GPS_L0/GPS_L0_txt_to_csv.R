# convert unusable txt gps files to usable csv files

library(dplyr)
library(readr)
setwd('/Volumes/LaCie/L0/Bird_Island/Tag_Data/2021_2022/Catlog/still_txt/')
gpsfiles<-list.files(pattern='*.txt')
dropdir <- "/Volumes/LaCie/L0/Bird_Island/Tag_Data/2021_2022/Catlog/still_txt/"
for (i in 1:length(gpsfiles))
{
  # m_txt <- read_delim(gpsfiles[i],show_col_types = FALSE)
  # colnames(m_txt) <- trimws(colnames(m_txt))
  
  
  m_txt <- read.csv(gpsfiles[i],row.names=NULL,header=FALSE)
  colnames(m_txt) <- m_txt[1,]
  m_txt <- m_txt[-1,-14]
  rownames(m_txt) <- NULL
  colnames(m_txt) <- trimws(colnames(m_txt))
  m_txt$Time <- trimws(m_txt$Time)
  
  
  for (col in 3:length(m_txt))
  {
    m_txt[col] = as.numeric(m_txt[[col]])
  }
  m.csv <- na.omit(m_txt) 
  write.csv(m.csv,file=paste0(dropdir,substr(gpsfiles[i], 1, nchar(gpsfiles[i])-4),".csv"),row.names=FALSE)
}





