################################################################################
## figures.R
##
## Will Oestreich
################################################################################
## This script contains sections for analysis of processed presence, absence, 
## and sequence results. Associated figures are saved to outputs/figures.
################################################################################

##### packages
library(tidyr)
library(patchwork)
library(suncalc)
## clear variables
rm(list=ls())

##### load output data files
presence <- read.csv("outputs/files/presence.csv")
clicks_soldeg <- read.csv("outputs/files/clicks_soldeg.csv")
sequences <- read.csv("outputs/files/sequences.csv")
sequences2 <- read.csv("outputs/files/sequences2.csv")
absences <- read.csv("outputs/files/absences.csv")

##### 1. annual time series plots (by month) ###################################
## select year here
yr <- 2015

monthly <- data.frame(matrix(NA, nrow = 12, ncol = 2))
colnames(monthly) <- c("month","perc")
for (m in 1:12) {
  mo <- presence %>% filter(month == m, year == yr)
  monthly$perc[m] <- (sum(mo$yn)/length(mo$yn))*100
  monthly$month[m] <- m
}

subyr <- presence %>% filter(year == yr)
perc_pres <- (sum(subyr$yn)/length(subyr$yn))*100

tiff(paste("outputs/figures/",yr,".tiff",sep=""),units="in", width=10,height=4,res=300)
ggplot(monthly, aes(month,perc)) + geom_col() + 
  scale_x_continuous(breaks=seq(1,12,1), limits=c(0.5,12.5)) +
  xlab("Month") +
  ylab("Percent of recording days\nwith cachalot present") +
  ylim(c(0,100)) +
  ggtitle(paste(yr,": clicks present ",round(perc_pres,digits = 1),"% of recording days",sep="")) +
  theme_classic()
dev.off()


##### 2. avg annual climatology boxplots by month ##############################
yy <- rep(c(2015,2016,2017,2018,2019,2020,2021,2022),each=12)
yy <- yy[8:91]
mm <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=8)
mm <- mm[8:91]

allmonths <- data.frame(matrix(NA, nrow = length(yy), ncol = 3))
colnames(allmonths) <- c("year","month","perc")
for (i in 1:length(yy)) {
  mo <- presence %>% filter(month == mm[i], year == yy[i])
  allmonths$year[i] <- yy[i]
  allmonths$month[i] <- mm[i]
  allmonths$perc[i] <- (sum(mo$yn)/length(mo$yn))*100
}

tiff("outputs/figures/climatology.tiff",units="in", width=10,height=4,res=300)
  ggplot(allmonths, aes(x = as.factor(month), y = perc)) + geom_boxplot() +
    ylim(c(0,100)) +
    xlab("Month") +
    ylab("Percent of recording days \n with cachalot present") +
    geom_vline(aes(xintercept=1.5), linetype="dotted") +
    geom_vline(aes(xintercept=2.5), linetype="dotted") +
    geom_vline(aes(xintercept=3.5), linetype="dotted") +
    geom_vline(aes(xintercept=4.5), linetype="dotted") +
    geom_vline(aes(xintercept=5.5), linetype="dotted") +
    geom_vline(aes(xintercept=6.5), linetype="dotted") +
    geom_vline(aes(xintercept=7.5), linetype="dotted") +
    geom_vline(aes(xintercept=8.5), linetype="dotted") +
    geom_vline(aes(xintercept=9.5), linetype="dotted") +
    geom_vline(aes(xintercept=10.5), linetype="dotted") +
    geom_vline(aes(xintercept=11.5), linetype="dotted") +
    theme_classic()
dev.off()


##### 3. solar elevation #######################################################
clicks_soldeg <- as.data.frame(clicks_soldeg)
colnames(clicks_soldeg) <- "soldeg"

## hydrophone lat, lon for sunrise/sunset calcs
hlat <- 36.7125
hlon <- -121.1868

## compare this to distribution of solar elevations in a year
t <- seq.POSIXt(from = as.POSIXct("2021-01-01"), to = as.POSIXct("2021-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

tiff("outputs/figures/daynight.tiff",units="in", width=10,height=8,res=300)
pa <- ggplot(clicks_soldeg, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 2) +
  geom_density(data = tsol, aes(soldeg), size = 2, linetype = "dashed") +
  annotate("label", label = "Night\n11.3 clicks/hr", x = -44.5, y = 0.0007, fill = "white", size = 5) +
  annotate("label", label = "Dusk/Dawn\n10.3 clicks/hr", x = -6, y = 0.0007, fill = "white", size = 5) +
  annotate("label", label = "Day\n14.1 clicks/hr", x = 32.5, y = 0.0007, fill = "white", size = 5) +
  xlab("solar elevation (degrees)") +
  ylab("density") +
  geom_segment(aes(x=-70, y=0.013, xend=-50, yend=0.013),color="black",size=2) +
  geom_segment(aes(x=-70, y=0.012, xend=-50, yend=0.012),color="black",linetype="dashed",size=2) +
  annotate("text", label = "Detected clicks", x = -45, y = 0.013, hjust=0, size = 5) +
  annotate("text", label = "Annual distribution\nof solar elevation", x = -45, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))
pa
dev.off()


##### 4. presence/absence sequences ############################################
## distribution of daily presence sequence lengths
u <- unique(sequences$length)
mode <- u[which.max(tabulate(match(sequences$length, u)))]
ua <- unique(absences$length)
modea <- ua[which.max(tabulate(match(absences$length, ua)))]
tiff("outputs/figures/sequence_distribution.tiff",units="in", width=8,height=8,res=300)
pa <- ggplot(sequences, aes(length)) + geom_histogram(binwidth=1) +
  xlab("number of consecutive recording days of click presence") +
  ylab("count") +
  xlim(c(0,45)) +
  theme_classic()
pb <- ggplot(absences, aes(length)) + geom_histogram(binwidth=1) +
  xlab("number of consecutive recording days of click absence") +
  ylab("count") +
  xlim(c(0,45)) +
  theme_classic()
pa/pb
dev.off()

## presence sequence lengths vs. yearday
tiff("outputs/figures/sequences_yday.tiff",units="in", width=8,height=4,res=300)
pa <- ggplot(sequences, aes(x=yday,y=length)) + geom_jitter(size=2) +
  xlab("sequence start (day of year)") +
  ylab("number of consecutive recording days \nof click presence") +
  ylim(c(0,45)) +
  theme_classic()
pa
dev.off()

## distribution of daily presence sequence lengths (excluding single day absences)
u <- unique(sequences2$length)
mode <- u[which.max(tabulate(match(sequences2$length, u)))]
# remove absences of length 1
absences2 <- absences
absences2$length[absences2$length == 1] <- NA

tiff("outputs/figures/sequence_distribution_2.tiff",units="in", width=8,height=8,res=300)
pa2 <- ggplot(sequences2, aes(length)) + geom_histogram(binwidth=1) +
  xlab("number of consecutive recording days of click presence") +
  ylab("count") +
  xlim(c(0,45)) +
  theme_classic()
pb2 <- ggplot(absences2, aes(length)) + geom_histogram(binwidth=1) +
  xlab("number of consecutive recording days of click absence") +
  ylab("count") +
  xlim(c(0,45)) +
  theme_classic()
pa2/pb2
dev.off()

## presence sequence lengths vs. yearday
tiff("outputs/figures/sequences_yday_2.tiff",units="in", width=8,height=4,res=300)
pa <- ggplot(sequences2, aes(x=yday,y=length)) + geom_jitter(size=2) +
  xlab("sequence start (day of year)") +
  ylab("number of consecutive recording days \nof click presence") +
  ylim(c(0,45)) +
  theme_classic()
pa
dev.off()
