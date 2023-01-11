################################################################################
## sequences.R
##
## Will Oestreich
################################################################################
## This script calculates lengths (in days) of sequences of consecutive 
## recording days with cachalot click presence and absence. 2 methods are used:
## (1) simply consecutive days of presence or absence; and (2) consecutive days 
## of presence or absence, ignoring single days of click absence (which could, 
## in rare cases, be false absences). This 2nd scenario is very unlikely, but 
## another useful way of looking at this data. Both approaches produce very 
## similar results (see outputs).
################################################################################

##### packages
library(tidyverse)
## clear variables
rm(list=ls())

##### load in daily presence/absence data
presence <- read.csv("outputs/files/presence.csv")

##### Distribution of days w/ consecutive presence/absence #####
##### this section calculates presence/absence sequences including 1-day absences
sequences <- data.frame(matrix(NA, nrow = 0, ncol = 4))
absences <- data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(sequences) <- c("year","month","yday","length")
colnames(absences) <- c("year","month","yday","length")
d <- data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(d) <- c("year","month","yday","length")
da <- data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(da) <- c("year","month","yday","length")
index <- 0 # the first day of switch to absence in the time series (minus 1)
indexa <- 3 # the first day of switch to presence in the time series (minus 1)

for (i in 1:(length(presence$yn)-1)) {
  
  presdiff <- presence$yn[i] - presence$yn[i+1]
  
  if (presdiff == 1) {
    
    ## meeting this condition means that we are at the end of a presence sequence
    # calculate sequence length, and store month and year info
    d$length <- i - indexa
    d$year <- format(presence$date[indexa+1], format="%Y")
    d$month <- format(presence$date[indexa+1], format="%m")
    d$yday <- yday(presence$date[indexa+1])
    sequences <- rbind(sequences,d)
    
    # finally, store the index (useful for calculating the length of the following sequence)
    index <- i 
    
  } else if (presdiff == -1) {
    ## meeting this condition means that we are at the end of a absence sequence
    # calculate absence length, and store month and year info
    da$length <- i - index
    da$year <- format(presence$date[index+1], format="%Y")
    da$month <- format(presence$date[index+1], format="%m")
    da$yday <- yday(presence$date[index+1])
    absences <- rbind(absences,da)
    
    # finally, store the index (useful for calculating the length of the following sequence)
    indexa <- i
    
  }
  else{
  }
}

##### this section calculates presence/absence sequences, excusing 1-day absences
sequences2 <- data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(sequences2) <- c("year","month","yday","length")
d2 <- data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(d2) <- c("year","month","yday","length")
indexb <- 3 # the first day of switch to presence in the time series (minus 1)

for (i in 1:(length(presence$yn)-2)) {
  
  presdiff1 <- presence$yn[i] - presence$yn[i+1]
  presdiff2 <- presence$yn[i] - presence$yn[i+2]
  
  if (presdiff1 == 1 && presdiff2 == 1) {
    ## meeting this condition means that we are at the end of a presence sequence
    # calculate sequence length, and store month and year info
    d2$length <- i - indexb
    d2$year <- format(presence$date[indexb+1], format="%Y")
    d2$month <- format(presence$date[indexb+1], format="%m")
    d2$yday <- yday(presence$date[indexb+1])
    sequences2 <- rbind(sequences2,d2)
    
    # finally, store the index (useful for calculating the length of the following sequence)
    index <- i 
    
  } 
  else{
  }
  
  ## final conditional to keep track of last index w/ 2 consecutive zeros for calculating sequence lengths
  if (presence$yn[i] == 0 && presence$yn[i+1] == 0) {
    indexb <- i + 1
  } else {
  }
}

##### save outputs
write.csv(sequences, "outputs/files/sequences.csv")
write.csv(sequences2, "outputs/files/sequences2.csv")
write.csv(absences, "outputs/files/absences.csv")
