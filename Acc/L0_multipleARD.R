################################################################################

# Check what's happening when there are multiple .ard files 
#
# I. Maywar

################################################################################

# Use this code when there are multiple .ard files for a given bird.
#
# Multiple .ard files may exist because the tag was turned on/off on accident or as a test.
# When the tag is turned off an .ard file is written. Turning the tag on will start the writing of the next .ard file.
# We must remove on/off test files and use the file written during the actual deployment.
#
# Multiple .ard files may also exist because the tag resets or glitches mid-deployment.
# This will cause the tag to stop writing its current file and start writing the next .ard file once it stops glitching.
# This is an issue because we do not know when the next file starts writing.
#
# STEPS in this code:
#
# 1. Import the first converted .ard file and decide if 
#
# 2. Append birds with multiple files (Multiple files should only exist for birds that )
#
# 2. Remove the problematic metadata column
#
# 3. Create new TagID using bird's darvic number
#
# NOTE: Unfortunately, X manager cannot be automated so the conversion process is pretty inefficient

################################################################################

# Make sure that DateTime includes milliseconds
my_options <- options(digits.secs = 3)
library(lubridate)

# Load Metadata file

# 2019_2020: 
# meta_file <- "G:/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Bird_Island/2019_2020/Metadata_Acc-On-Times/BI2019_2020_Metadata_L1_metaTimes_PROOFEDtidy.csv"

# 2020_2021:
# meta_file <- "G:/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Bird_Island/2020_2021/Deployment Meta Data/BirdIsland2021_Deployment_Data_trimmed-TimesMeta.csv"

# 2021_2022:
meta_file <- "G:/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/metadata/Bird_Island/2021_2022/Deployment Meta Data/BirdIsland2022_Deployment_Data.csv"

meta <- read.csv(meta_file)

################################################################################

# 1. Inspect the first file

S1_csv <- "C:/Users/IanMaywar/Desktop/2021-2022/AGM/Brood_Guard/BBAL96_O332_2022_AGM03_120122_S2.csv"
m1 <- read.csv(S1_csv)
first1 <- strptime(paste(m1$Date[1],m1$Time[1], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")
last1 <- strptime(paste(m1$Date[nrow(m1)],m1$Time[nrow(m1)], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")

# You're looking at 3 things here:
  # 1. The metadata of the final row in the data. Does it say "Power off command received."?
      tail(m1)
  # 2. The duration of the data. Is it on the order of days?
      as.period(interval(first1,last1))
  # 3. The datetime of the final row. Is it roughly the same (on the order of hours) as the recap datetime?
      last1

# If 1, 2, and 3 are all TRUE: 
#   This is a complete file. The tag lasted for the whole deployment and captured acc data continuously.
#   Subsequent .ard file(s) are on/off tests or user error. 
#   Just keep this first .csv file and don't convert the rest.
      
################################################################################      

# If 1 is TRUE but 2 and 3 are FALSE:
#   This file is likely an on/off test or user error. 
#   Delete this .csv file and convert the next .ard file using X manager and the axyON_datetime

# If 1 is FALSE:
#   Oh no, this tag glitched mid-deployment. 
#   Convert the next .ard file using X Manager.
#   The START TIME should be the second following the datetime of the final entry in the current .csv file.
#   For example, if the the datetime of the final row in this .csv file is "2020-01-03 22:14:33.96 UTC", 
#   the START TIME for the next .ard file conversion should be "2020-01-03 22:14:34"
      
m2 <- read.csv(paste(substr(S1_csv,1,nchar(S1_csv)-5),"2.csv",sep=""))
first2 <- strptime(paste(m2$Date[2],m2$Time[2], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")
last2 <- strptime(paste(m2$Date[nrow(m2)],m2$Time[nrow(m2)], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")

tail(m2)
as.period(interval(first2,last2))
last2
  



m3 <- read.csv(paste(substr(S1_csv,1,nchar(S1_csv)-5),"3.csv",sep=""))
first3 <- strptime(paste(m3$Date[3],m3$Time[3], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")
last3 <- strptime(paste(m3$Date[nrow(m3)],m3$Time[nrow(m3)], sep=" "), "%Y/%m/%d %H:%M:%OS", tz="UTC")

tail(m3)
as.period(interval(first3,last3))
last3


