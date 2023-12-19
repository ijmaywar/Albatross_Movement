setwd("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/Bird_Island/Tag_Data/Acc/2019_2020/")
acc_files <- list.files(pattern='*.csv')

for (i in 1:length(acc_files)) {
  
  # Read flap file
  m <- read.csv(acc_files[i])
  
  # Explore histogram of flap heights
  hist(m$pks, breaks=seq(-10,0,by=0.2))
  
  
  
  # quantiles
  quantile(m$pks,probs=c(0.05,0.95))
  
  # This might not be too useful because this is filtered data.
  # I should be looking at raw data to remove single point errors right? 
  
  # use matlab to look histogram of raw data points. 
  
  
}