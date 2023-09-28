library(ggplot2)


setwd("/Volumes/LaCie/Flap_Summaries")
files<-list.files(pattern='*.csv')

for (i in 1:length(files)) {
  FS <- read.csv(files[i])
  FS$state <- factor(FS$state)
  p <- ggplot(FS, aes(state, nflaps))
  p + geom_boxplot()
  
  
  
}