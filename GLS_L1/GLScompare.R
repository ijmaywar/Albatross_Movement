################################################################################
#
# Comparing GLS on-water to HMM state 1
#
################################################################################

# Clear environment -------------------------------------------------------

rm(list = ls())

# User Inputed Values -----------------------------------------------------

location = 'Bird_Island' # Options: 'Bird_Island', 'Midway'
szn = "2019_2020"
spp = "BBAL"
interp = "600s"

# Set Environment ---------------------------------------------------------

library(dplyr)
library(ggplot2)

GD_dir <- "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/"
GLS_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GLS/", szn, "/")
HMM_dir <- paste0(GD_dir, "THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L3/Bird_Island/Tag_Data/GPS/", interp, "/3_states/", spp, "/")

setwd(HMM_dir)
allGPSfiles <- list.files(pattern='*.csv')

setwd(GLS_dir)
allGLSfiles <- list.files(pattern='*.csv')
GLSfiles <- allGLSfiles[grep(paste0("^", spp), allGLSfiles)]
birdnames <- sub("_GLS_L1.csv$","",GLSfiles)

# User functions ----------------------------------------------------------


# Loop thru and process ---------------------------------------------------
confusion_matrix_list <- list()
for (i in 1:length(GLSfiles)) {
  # find GLS and HMM data for current bird
  setwd(GLS_dir)
  GLS_data <- read.csv(GLSfiles[i])
  GLS_data$starttime <- as.POSIXct(GLS_data$starttime, format = "%d-%b-%Y %H:%M:%S")
  GLS_data$endtime <- as.POSIXct(GLS_data$endtime, format = "%d-%b-%Y %H:%M:%S")
  
  setwd(HMM_dir)
  HMM_filename <- allGPSfiles[grep(birdnames[i], allGPSfiles)]
  if (length(HMM_filename)==0) {
    # There is no HMM data and this file should be skipped
    next
  } else{
    HMM_data <- read.csv(allGPSfiles[grep(birdnames[i], allGPSfiles)])
  }
  
  HMM_data$datetime <- as.POSIXct(HMM_data$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  m <- HMM_data
  m$GLS <- "dry"
  
  GLS_wet <- filter(GLS_data,state=="wet")
  for (j in 1:nrow(GLS_wet)) {
    # for each "wet" the starttime and endtime must span certain intervals in m.
    # all of those intervals must be marked as wet in m.
    start <- GLS_wet$starttime[j]
    end <- GLS_wet$endtime[j]
    
    diffstart <- as.numeric(difftime(start,m$datetime,units="mins"))
    diffend <- as.numeric(difftime(end,m$datetime,units="mins"))
    
    startidx <- which(diffstart>=0 & diffstart<10)
    endidx <- which(diffend>=0 & diffend<10)
    
    if (length(startidx)!=0 & length(endidx)!=0) {
      # both start and end occur during HMM data
      m$GLS[startidx:endidx] <- "wet"
    } else if (length(startidx)!=0 & length(endidx)==0) {
      # end doesn't occur during HMM data, start occurs during HMM data
      endidx <- max(which(diffend>=0)) 
      m$GLS[startidx:endidx] <- "wet"
    } else if (length(startidx)==0 & length(endidx)!=0) {
      # start doesn't occur HMM data, end occurs during HMM data
      startidx <- min(which(diffstart<10))
      m$GLS[startidx:endidx] <- "wet"
    }
  }
  
  confusion_matrix_list[[i]] <- table(m$state,m$GLS)
  
    
}

# run thru confusion matrices
stats_scores <- data.frame(birdnames)
stats_scores$accuracy[i] <- NA
stats_scores$precision[i] <- NA
stats_scores$recall[i] <- NA
stats_scores$f1_score[i] <- NA
stats_scores$specificity[i] <- NA

for (i in 1:length(confusion_matrix_list)) {
  cm <- confusion_matrix_list[[i]]
  if (is.null(cm)) {
    next
  }
  
  # "Positive" refers to the bird being wet
  
  # 2 states
  # tp <- cm[1,2]
  # fp <- cm[1,1]
  # fn <- cm[2,2]
  # tn <- cm[2,1]
  
  # 3 states
  tp <- cm[1,2] + cm[2,2]
  fp <- cm[1,1] + cm[2,1]
  fn <- cm[3,2]
  tn <- cm[3,1]
  
  stats_scores$accuracy[i] <- (tp+tn) / (sum(cm))
  precision <- tp / (tp+fp)
  recall <- tp / (tp+fn)
  stats_scores$precision[i] <- precision
  stats_scores$recall[i] <- recall
  stats_scores$f1_score[i] <- (2*precision*recall) / (precision+recall)
  stats_scores$specificity[i] <- tn / (fp + tn)
}

mean(na.omit(stats_scores$accuracy))
mean(na.omit(stats_scores$precision))
mean(na.omit(stats_scores$recall))
mean(na.omit(stats_scores$f1_score))
mean(na.omit(stats_scores$specificity))


