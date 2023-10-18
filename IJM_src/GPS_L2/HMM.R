
# Sample code to apply Theo Michelot's Hiden Markove Model to estimate behavioral states from 
# animal movement data (applied here to jaegers tracked in the Pacific Ocean for a paper, Amon et al. In Review)

# Autumn-Lynn Harrison, HarrisonAL@si.edu, 2023



# Clear environment -------------------------------------------------------

rm(list = ls())

######################################################
### Estimate movement states
######################################################

library(moveHMM)
library(ggplot2)

wrapCor = function(cor) {corWrap<-ifelse(cor>180,cor-360,cor);return(corWrap)}

# Two state model will likely separate:
# Breeding/stationary/ARS as a single state
# and commuting/migrating as a single state

#############################
### Estimate movement states
#############################

# Load Data

df = read.csv(file = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L2/Bird_Island/Tag_Data/GPS/compiled/300s/BBALinterp_300s.csv")

df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
df$lon <- wrapCor(df$lon)

names(df) <- c("ID",   "date", "lon",  "lat" )

# df$ID =factor(x=df$ID)
## I might need to factor trip ID
## Is it required that all GPS outputs are put into one csv file or can i run them seperately?

data = prepData(
  df,
  type = "LL",
  coordNames = c("lon", "lat"),
  LLangle = TRUE
)


## initial parameters for gamma and von Mises distributions. See Michelot vignette
## TWO STATE MODEL
mu0 <- c(0.2,1) #(state1,state2)
sigma0 <- c(0.2,0.5) #(state1,state2)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(pi,0)
kappa0 <- c(1,4) # angle concentration
anglePar0 <- c(angleMean0,kappa0)
# anglePar0 <- kappa0

m <- fitHMM(data=data, 
            nbStates = 2, 
            stepPar0 = stepPar0, 
            anglePar0 = anglePar0)

# Plot the results #
plot(m, plotCI = TRUE)

# Get the states #
states <- viterbi(m)
state_probs <- stateProbs(m)

# Plots time series showing for each position a binary estimate of whether they are in State 1 or 2 (dot plot, based on a threshold of the probability line graphs plotted below)
# Plots line graphs showing the continuous probability of being in state 1 or 2 (a threshold is used to turn into binary)
plotStates(m)

###########################
### SAVE OUT TWO STATE MODEL
###########################

# Add states to the data set #
df$predictedState <- viterbi(m)
df$probState1 <- stateProbs(m)[,1]
df$probState2 <- stateProbs(m)[,2]

#md24 <- md
save(df, file = "data-created/2stateHMM_24hr.Rdata")

migState = df[df$predictedState ==1,]
resState = df[df$predictedState==2,]




##################################################
###
### Plot turn angle and step size with GPS track
###
##################################################

data$predictedstate = viterbi(m)

plotdomain = 500:1000

ggplot(data[plotdomain,], aes(x=x,y=y)) + 
  geom_point(size=2,aes(color=abs(angle))) +
  scale_color_gradient(low="yellow",high="black") +
  geom_point(x=-38.0658417,y=-54.0101833,color="blue")

ggplot(data[plotdomain,], aes(x=x,y=y)) + 
  geom_point(size=2,aes(color=factor(predictedstate))) +
  geom_point(x=-38.0658417,y=-54.0101833,color="blue")

ggplot(data[plotdomain,], aes(x=x,y=y)) + 
  geom_point(size=2,aes(color=abs(step))) +
  scale_color_gradient(low="yellow",high="black") +
  geom_point(x=-38.0658417,y=-54.0101833,color="blue")








######################################################################################################
### PLOT on Pacific MAP, color code State 1 vs State 2 and overlay Clarion-Clipperton Zone boundaries
## plot selected IDs for Diva's paper with the residence periods shown
#######################################################################################################

## EXAMPLE PLOTTING CODE (cl45 and cczRGDAL were previously loaded geographic layers, but left this in so you can just see some example)
# p <- "+proj=robin +lon_0=-110 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# mapPlot(cl45, projection=p, col='gray88')
# ind = migState
# mapPoints(ind$lon, ind$lat, pch=19, cex=0.15, col=alpha("lightgrey",0.75))
# ind = resState
# mapPoints(ind$lon, ind$lat, pch=19, cex=0.15, col=alpha("darkblue",0.75))

# lines(cczRGDAL, col="red", lwd = 2)

#mapLines(ind$lon, ind$lat, pch=19, cex=0.25, col=alpha("darkblue"))

######################################################
### COMPARE TO 3-STATE MODEL
# To see if model separates breeding residency periods from ARS during flights
######################################################

df = read.csv(file = "/Users/harrisonAL/Dropbox (Personal)/Manuscripts/2024_PacificJaegers/Harrison_Jaegers_CCZ.csv")

df$date <- as.POSIXct(df$date, format = "%m/%d/%y %H:%M")

names(df) <- c("ID",   "date", "lon",  "lat" )

df$ID =factor(x=df$ID)

data = prepData(
  df,
  type = c("LL"),
  coordNames = c("lon", "lat"),
  LLangle = TRUE
)

# THREE STATE MODEL
# initial parameters pulled completely out of thin air?? How to decide?
mu0 <- c(1,15,30)
sigma0 <- c(50,50,100)
zeromass0 <- c(0,1,1)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(pi,pi,0)
kappa0 <- c(1,1,1)
anglePar0 <- c(angleMean0,kappa0)

m3 <- fitHMM(data=data, 
             nbStates = 3, 
             stepPar0 = stepPar0, 
             anglePar0 = anglePar0, 
             formula = ~1)

# Plot the results #
plot(m3, plotCI = TRUE)
# Three state model seems like it correctly separates breeding from migration from staging and over-wintering. 

# compare the 2 models
AIC(m,m3)

###########################
### SAVE OUT THREE STATE MODEL
###########################

# Add states to the data set #
df$predictedState <- viterbi(m)
df$probState1 <- stateProbs(m3)[,1]
df$probState2 <- stateProbs(m3)[,2]
df$probState3 <- stateProbs(m3)[,3]

save(df, file = "data-created/3stateHMM_24hr.Rdata")



