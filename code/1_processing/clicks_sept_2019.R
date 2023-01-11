##### packages #####
library(tidyverse)
library(plyr)
library(suncalc)
library(R.matlab)
library(lubridate)

##### prepare files and parameters before looping through test #####
## clear variables
rm(list=ls())

## hydrophone lat, lon for sunrise/sunset calcs
hlat <- 36.7125
hlon <- -121.1868

## set minimum number of repetitions to classify as a cachalot
r <- 6  # this was optimized using script "daily_performance.R"

## set degree of rounding for inter-click-interval calculations
ro <- 0.25

## set maximum and minimum separation in seconds to consider sequence to be cachalot
minsep <- 0.5
maxsep <- 2.0

## list detection files from 2019
files <- list.files(path="data/BLED/2019", pattern="*.txt", full.names=TRUE, recursive=TRUE)
## restrict to Sept
files <- files[234:263]


## prepare an daily presence/absence data frame, including daily y/n ("yn") and day, night columns
presence <- data.frame(matrix(NA, nrow = length(files), ncol = 5))
colnames(presence) <- c("date","yn","day","night","dd")
presence$date <- as.Date(presence$date)

##### loop through files #####
for (f in 1:length(files)) {
  #### preparatory steps
  ## extract date from detection file name
  presence$date[f] <- as.Date(substr(files[f],21,28),"%Y%m%d")
  print(as.Date(substr(files[f],21,28),"%Y%m%d"))
  
  ## load in BLED
  bled1 <- read.csv(files[f], sep="\t", header=T)
  colnames(bled1) <- c("Selection","View","Channel","Begin","End","LowFreq","HighFreq","Occupancy")
  
  ## filter out waveform rows (no double counting)
  bled1 <- bled1 %>% filter(View == "Spectrogram 1")
  bled1$diff <- NA
  
  #### daily-resolution processing
  ## find difference (rounded) between consecutive detections in this day
  bled1$time <- as.POSIXct(presence$date[f]) + bled1$Begin
  attr(bled1$time,"tzone") <- "UTC"
  bled1$diff[- 1] <- diff(bled1$Begin)  # difference (in seconds) between detections
  bled1$diff[bled1$diff < 0.4] <- NA # assign NA for differences that are shorter than cachalot click sequences. w/o this, some differences (e.g. 0.38) that are very short could be rounded up to 0.5, and counted as a cachalot click sequence
  bled1$diff <- round_any(bled1$diff, ro) # round to nearest ro (set above)
  
  ## this method identifies not just counts of cadences, but consecutive sequences of a cadence
  t1 <- rle(bled1$diff)
  seq1 <- rep(t1$lengths >= r, times = t1$lengths)
  cadence1 <- cbind(bled1$diff,seq1)
  cadence1 <- as.data.frame(cadence1)
  rownames(cadence1) <- NULL
  colnames(cadence1) <- c("diff","seq")
  bled1$seq <- cadence1$seq
  
  ## get solar elevation for day/night processing
  sun <- getSunlightPosition(date = bled1$time, 
                             lat = hlat,
                             lon = hlon,
                             keep = "altitude")
  bled1$sol <- sun$altitude
  bled1$soldeg <- bled1$sol*180/pi
  
  #### is there a clear cachalot cadence in this 24-hour day? 
  test1 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE)
  if (length(test1$seq) >= 1) {
    presence$yn[f] <- 1
  }
  else {
    presence$yn[f] <- 0
  }
  
  #### is there a clear cachalot cadence in daylight hours? 
  test2 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg > 0)
  if (length(test2$seq) >= 1) {
    presence$day[f] <- 1
  }
  else {
    presence$day[f] <- 0
  }
  
  #### is there a clear cachalot cadence in nighttime hours? 
  test3 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg < -12)
  if (length(test3$seq) >= 1) {
    presence$night[f] <- 1
  }
  else {
    presence$night[f] <- 0
  }
  
  #### is there a clear cachalot cadence in nighttime hours? 
  test4 <- bled1 %>% filter(diff >= minsep & diff <= maxsep & seq == TRUE & soldeg >= -12 & soldeg <= 0)
  if (length(test4$seq) >= 1) {
    presence$dd[f] <- 1
  }
  else {
    presence$dd[f] <- 0
  }
  
  #### store away all clicks id'd in sequences for later analysis
  if (f == 1) {
    clicks <- data.frame(matrix(NA, nrow = length(bled1$Selection), ncol = length(bled1)))
    colnames(clicks) <- colnames(bled1)
    clicks <- bled1 %>% filter(seq ==1)
  }
  else {
    newclicks <- bled1 %>% filter(seq ==1)
    clicks <- rbind(clicks,newclicks)
  }
}

##### time series plot of clicks ###############################################
clicks$yconst <- 1
tiff("outputs/figures/Sept2019clicks.tiff",units="in", width=10,height=0.65,res=300)
ggplot(clicks, aes(x=as.POSIXct(time),y=yconst)) + geom_point(size = 1, color = "red") +
  ylim(c(0.995,1.005)) +
  ylab("") +
  xlab("Time (UTC)") +
  scale_x_datetime(date_breaks = "1 week", minor_breaks = "1 day", limits = c(as.POSIXct("2019-09-01"),as.POSIXct("2019-10-01"))) +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()

