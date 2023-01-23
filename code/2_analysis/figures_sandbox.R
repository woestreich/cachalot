################################################################################
## figures_sandbox.R
##
## Will Oestreich
################################################################################
## This script contains sections for analysis of processed presence, absence, 
## and sequence results. Exploratory figures are saved to outputs/figures.
################################################################################

##### packages
library(tidyr)
library(patchwork)
library(suncalc)
library(zoo)
## clear variables
rm(list=ls())

##### load output data files
presence <- read.csv("outputs/files/presence.csv")
clicks_soldeg <- read.csv("outputs/files/clicks_soldeg.csv")
sequences <- read.csv("outputs/files/sequences.csv")
sequences2 <- read.csv("outputs/files/sequences2.csv")
absences <- read.csv("outputs/files/absences.csv")
annual_perc <- read.csv("outputs/files/annual_perc.csv")
late_perc <- read.csv("outputs/files/late_perc.csv")
diel <- read.csv("outputs/files/day_night_dd_yearly.csv")
monthly <- read.csv("outputs/files/monthly.csv")
enso <- read.csv("data/ocean/mei.csv")
enso_year <- read.csv("data/ocean/mei_annual.csv")


##### 1. annual time series plots (by month) ###################################
## select year here
yr <- 2022

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
yy <- yy[8:96]
mm <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=8)
mm <- mm[8:96]

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

##### 5. presence sequences by year, month######################################
pa <- ggplot(sequences, aes(x=as.factor(year),y=log(length))) + geom_boxplot() +
  xlab("Year") +
  ylab("ln(sequence length)") +
  theme_classic()

pb <- ggplot(sequences, aes(x=as.factor(month),y=log(length))) + geom_boxplot() + 
  xlab("Month") +
  ylab("ln(sequence length)") +
  theme_classic()

tiff("outputs/figures/sequences_year_month.tiff",units="in", width=8,height=4,res=300)
pa/pb
dev.off()

##### 6. annual percent of recording days#######################################
enso$date <- mdy(enso$ym)
annual_perc$date <- as.Date(paste(as.character(annual_perc$year),"-07-01",sep = ""))
pa <- ggplot(annual_perc, aes(x=date,y=perc)) + geom_line() + geom_point() + 
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "*", x = as.Date("2015-07-01"), y = 47, size = 5) +
  annotate("text", label = "*", x = as.Date("2022-07-01"), y = 36, size = 5) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(30,70)) +
  xlab("") +
  ylab("% recording days\n with cachalot clicks") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pb <- ggplot(enso, aes(x=factor(year),y=mei)) + geom_bar(stat = "summary", fun = "mean")

pc <- ggplot(enso, aes(x=date,y=mei)) + geom_col(aes(fill = cut(mei, breaks = c(-5, -0.5, 0.5, 5)))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = -2.4, size = 3) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(-2.5,2.5)) +
  xlab("") +
  ylab("Multivariate ENSO Index") +
  scale_fill_manual(values=c("blue", "grey", "red")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pa/pc

##### 7. diel + interannual + oceanography######################################
enso$date <- mdy(enso$ym)
annual_perc$date <- as.Date(paste(as.character(annual_perc$year),"-07-01",sep = ""))
diel$date <- as.Date(paste(as.character(diel$year),"-07-01",sep = ""))
pa <- ggplot(annual_perc, aes(x=date,y=perc)) + geom_line() + geom_point() + 
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 31.5, size = 3) +
  annotate("text", label = "*", x = as.Date("2015-07-01"), y = 47, size = 5) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(30,70)) +
  xlab("") +
  ylab("% recording days\n with cachalot clicks") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pb <- ggplot(diel, aes(x=date,y=ratio)) + geom_point() + geom_line() +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dashed") + 
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 0.7, size = 3) +
  annotate("text", label = "*", x = as.Date("2015-07-01"), y = 1.6, size = 5) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(0.65,2)) +
  xlab("") +
  ylab("click rate ratio\n(day:night)") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pc <- ggplot(enso, aes(x=date,y=mei)) + geom_col(aes(fill = cut(mei, breaks = c(-5, -0.5, 0.5, 5)))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = -2.4, size = 3) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(-2.5,2.5)) +
  xlab("") +
  ylab("Multivariate\nENSO Index") +
  scale_fill_manual(values=c("blue", "grey", "red")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

tiff("outputs/figures/interannual_3panel.tiff",units="in", width=6,height=6,res=300)
pa/pb/pc
dev.off()


##### 8. diel + interannual + oceanography, with smoothing######################
## rolling mean window size
wsize = 6
##
enso$date <- mdy(enso$ym)
monthly$date <- as.Date(paste(as.character(monthly$year),as.character(monthly$month),"15",sep = "-"))
monthly$perc_rm <- rollapply(monthly$perc,wsize,mean,fill=NA,na.rm = TRUE)
monthly$ratio_rm <- rollapply(monthly$ratio,wsize,mean,fill=NA,na.rm = TRUE)

pa <- ggplot(monthly, aes(x=date,y=perc_rm)) + geom_line() + geom_point() + 
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 16.5, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 16.5, size = 3) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(15,80)) +
  xlab("") +
  ylab("% recording days\n with cachalot clicks\n(6-month running mean)") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pb <- ggplot(monthly, aes(x=date,y=ratio_rm)) + geom_point() + geom_line() +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dashed") + 
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = -0.5, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = -0.5, size = 3) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(-1,15)) +
  xlab("") +
  ylab("click rate ratio\n(day:night)\n(6-month running mean)") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

pc <- ggplot(enso, aes(x=date,y=mei)) + geom_col(aes(fill = cut(mei, breaks = c(-5, -0.5, 0.5, 5)))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2015-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2016-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2018-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2019-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-01-01"),linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-01-01"),linetype = "dotted") +
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = -2.4, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = -2.4, size = 3) +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(-2.5,2.5)) +
  xlab("") +
  ylab("Multivariate\nENSO Index") +
  scale_fill_manual(values=c("blue", "grey", "red")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank())

tiff("outputs/figures/interannual_3panel_rm.tiff",units="in", width=6,height=6,res=300)
pa/pb/pc
dev.off()

##### 9. Scatterplots of diel ratio vs. % presence #############################
## ANNUAL
ggplot(enso_year, aes(perc,ratio)) + geom_point(size=2) +
  ylab("click rate ratio\n(day:night)") +
  xlab("% of recording days\nwith cachalot clicks") +
  theme_classic()

## MONTHLY
# raw
ggplot(monthly, aes(perc,ratio)) + geom_point(size=2) +
  ylab("click rate ratio\n(day:night)") +
  xlab("% of recording days\nwith cachalot clicks") +
  theme_classic()
# smoothed
ggplot(monthly, aes(perc_rm,ratio_rm)) + geom_point(size=2) +
  ylab("click rate ratio\n(day:night)") +
  xlab("% of recording days\nwith cachalot clicks") +
  theme_classic()

##### 10. Scatterplots of annual metrics vs. ENSO index ########################
## LINEAR REGRESSIONS
## % presence vs. MEI
x <- enso_year$mei
y <- enso_year$perc
perc_mod <- lm(y~x)
perc_pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(perc_pts) <- c("x","y")
perc_pts$x <- c(min(x),max(x))
perc_pts$y <- c(perc_mod$coefficients[1] + perc_mod$coefficients[2]*min(x),
                   perc_mod$coefficients[1] + perc_mod$coefficients[2]*max(x))
## % presence vs. MEI (lagged)
x <- enso_year$mei_lag
y <- enso_year$perc
perclag_mod <- lm(y~x)
perclag_pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(perclag_pts) <- c("x","y")
perclag_pts$x <- c(min(x),max(x))
perclag_pts$y <- c(perclag_mod$coefficients[1] + perclag_mod$coefficients[2]*min(x),
                perclag_mod$coefficients[1] + perclag_mod$coefficients[2]*max(x))
## ratio vs. MEI 
x <- enso_year$mei
y <- enso_year$ratio
ratio_mod <- lm(y~x)
ratio_pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(ratio_pts) <- c("x","y")
ratio_pts$x <- c(min(x),max(x))
ratio_pts$y <- c(ratio_mod$coefficients[1] + ratio_mod$coefficients[2]*min(x),
                   ratio_mod$coefficients[1] + ratio_mod$coefficients[2]*max(x))
## ratio vs. MEI(lagged)
x <- enso_year$mei_lag
y <- enso_year$ratio
ratiolag_mod <- lm(y~x)
ratiolag_pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(ratiolag_pts) <- c("x","y")
ratiolag_pts$x <- c(min(x),max(x))
ratiolag_pts$y <- c(ratiolag_mod$coefficients[1] + ratiolag_mod$coefficients[2]*min(x),
                 ratiolag_mod$coefficients[1] + ratiolag_mod$coefficients[2]*max(x))


pa <- ggplot(enso_year, aes(mei,perc)) + geom_point(size=2) +
  geom_line(data=perc_pts, aes(x=x,y=y), linetype = "dashed") +
  annotate("text", x = 0.4, y = 55, 
           label = paste('R^2 ==',round(summary(perc_mod)$r.squared, 2)), size=3, parse = TRUE, hjust = 0) +
  ylab("% of recording days\nwith cachalot clicks") +
  xlab("Multivariate\nENSO Index") +
  xlim(c(-1.75,1.75)) +
  theme_classic()

pb <- ggplot(enso_year, aes(mei,ratio)) + geom_point() +
  geom_line(data=ratio_pts, aes(x=x,y=y), linetype = "dashed") +
  annotate("text", x = 0.4, y = 1.25, 
           label = paste('R^2 ==',round(summary(ratio_mod)$r.squared, 2)), size=3, parse = TRUE, hjust = 0) +
  ylab("click rate ratio\n(day:night)") +
  xlab("Multivariate\nENSO Index") +
  xlim(c(-1.75,1.75)) +
  theme_classic()

pc <- ggplot(enso_year, aes(mei_lag,perc)) + geom_point(size=2) +
  geom_line(data=perclag_pts, aes(x=x,y=y), linetype = "dashed") +
  annotate("text", x = 0.4, y = 57, 
           label = paste('R^2 ==',round(summary(perclag_mod)$r.squared, 2)), size=3, parse = TRUE, hjust = 0) +
  ylab("% of recording days\nwith cachalot clicks") +
  xlab("MultivariateENSO Index\n(6-month lag)") +
  xlim(c(-1.75,1.75)) +
  theme_classic()

pd <- ggplot(enso_year, aes(mei_lag,ratio)) + geom_point() +
  geom_line(data=ratiolag_pts, aes(x=x,y=y), linetype = "dashed") +
  annotate("text", x = 0.4, y = 1.25, 
           label = paste('R^2 ==',round(summary(ratiolag_mod)$r.squared, 2)), size=3, parse = TRUE, hjust = 0) +
  ylab("click rate ratio\n(day:night)") +
  xlab("Multivariate ENSO Index\n(6-month lag)") +
  xlim(c(-1.75,1.75)) +
  theme_classic()

tiff("outputs/figures/enso_scatter.tiff",units="in", width=8,height=6,res=300)
(pa | pb) / (pc | pd)
dev.off()

##### 11. ENSO lag cross correlation############################################
mei_lag <- data.frame(matrix(NA, nrow = 13, ncol = 5))
colnames(mei_lag) <- c("lag","perc_r2","perc_p","ratio_r2","ratio_p")
for (lag in -12:0) {
#for (lag in -12) {
  # placholder for cross-correlation data frame
  j <- lag + 13
  
  # set up data frame for linear models
  curr_lag <- data.frame(matrix(NA, nrow = 7, ncol = 3))
  colnames(curr_lag) <- c("meiavg","perc","ratio")
  
  for (yr in 2016:2022) {
    # find indices for this lag
    y <- which(enso$year == yr)
    idx <- y[1]
    idx1 <- idx + lag
    idx2 <- idx1 + 11
    
    # calculate MEI 12-month avg at this lag, store alongside % and ratio
    curr_lag$meiavg[yr-2015] <- mean(enso$mei[idx1:idx2])
    curr_lag$perc[yr-2015] <- annual_perc$perc[annual_perc$year == yr]
    curr_lag$ratio[yr-2015] <- diel$ratio[diel$year == yr]
  }
  
  # linear models
  mod_perc <- lm(curr_lag$perc~curr_lag$meiavg)
  mod_ratio <- lm(curr_lag$ratio~curr_lag$meiavg)
  
  # store all info for this lag
  mei_lag$lag[j] <- lag
  mei_lag$perc_r2[j] <- summary(mod_perc)$r.squared
  mei_lag$ratio_r2[j] <- summary(mod_ratio)$r.squared
  c1 <- summary(mod_perc)$coefficients[,4]
  c2 <- summary(mod_ratio)$coefficients[,4]
  mei_lag$perc_p[j] <- as.numeric(c1[2])
  mei_lag$ratio_p[j] <- as.numeric(c2[2])
}

ggplot(mei_lag, aes(lag,perc_r2)) + geom_point()
ggplot(mei_lag, aes(lag,perc_p)) + geom_point()
ggplot(mei_lag, aes(lag,ratio_r2)) + geom_point()
ggplot(mei_lag, aes(lag,ratio_p)) + geom_point()


##### 12. ENSO lag cross correlation, controlling for seasonality ##############
clim <-  data.frame(matrix(NA, nrow = 12, ncol = 3))
colnames(clim) <- c("month","perc","ratio")
for (m in 1:12) {
  clim$month[m] <- m
  curr_mo <- monthly %>% filter(month == m)
  clim$perc[m] <- mean(curr_mo$perc)
  clim$ratio[m] <- mean(curr_mo$ratio)
}

for (i in 1:length(monthly$perc)) {
  monthly$perc_anom[i] <- monthly$perc[i] - clim$perc[clim$month == monthly$month[i]]
  monthly$ratio_anom[i] <- monthly$ratio[i] - clim$ratio[clim$month == monthly$month[i]]
}

d <- merge(enso, monthly, by="date", all = TRUE)
colnames(d)[colnames(d) == "year.x"] <- "year"
colnames(d)[colnames(d) == "month.x"] <- "month"

cross_perc <- ccf(d$mei,d$perc_anom,na.action = na.pass)
cross_ratio <- ccf(d$mei,d$ratio_anom,na.action = na.pass)
lags_monthly <- data.frame(matrix(NA, nrow = length(cross_perc$lag), ncol = 3))
colnames(lags_monthly) <- c("lag","perc_acf","ratio_acf")
lags_monthly$lag <- cross_perc$lag
lags_monthly$perc_acf <- cross_perc$acf
lags_monthly$ratio_acf <- cross_ratio$acf

ggplot(lags_monthly,aes(lag,perc_acf)) + geom_point()
ggplot(lags_monthly,aes(lag,ratio_acf)) + geom_point()

##### 13. day-night dynamics by year#############################################
## hydrophone lat, lon for sunrise/sunset calcs
hlat <- 36.7125
hlon <- -121.1868

## compare to distribution of solar elevations in each year
#2015
yr <- 2015
t <- seq.POSIXt(from = as.POSIXct("2015-01-01"), to = as.POSIXct("2015-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p15 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("") +
  ylab("density") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2016
yr <- 2016
t <- seq.POSIXt(from = as.POSIXct("2016-01-01"), to = as.POSIXct("2016-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p16 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("") +
  ylab("") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2017
yr <- 2017
t <- seq.POSIXt(from = as.POSIXct("2017-01-01"), to = as.POSIXct("2017-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p17 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("") +
  ylab("") +
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2018
yr <- 2018
t <- seq.POSIXt(from = as.POSIXct("2018-01-01"), to = as.POSIXct("2018-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p18 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("") +
  ylab("") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2019
yr <- 2019
t <- seq.POSIXt(from = as.POSIXct("2019-01-01"), to = as.POSIXct("2019-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p19 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("solar elevation (degrees)") +
  ylab("density") +
  ylim(c(0,0.021)) + 
  geom_segment(aes(x=-70, y=0.020, xend=-50, yend=0.020),color="black",size=1) +
  geom_segment(aes(x=-70, y=0.015, xend=-50, yend=0.015),color="black",linetype="dashed",size=1) +
  annotate("text", label = "Detected clicks", x = -45, y = 0.020, hjust=0, size = 4) +
  annotate("text", label = "Annual distribution\nof solar elevation", x = -45, y = 0.015, hjust=0, size = 4) +
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2020
yr <- 2020
t <- seq.POSIXt(from = as.POSIXct("2020-01-01"), to = as.POSIXct("2020-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p20 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("solar elevation (degrees)") +
  ylab("") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2021
yr <- 2021
t <- seq.POSIXt(from = as.POSIXct("2021-01-01"), to = as.POSIXct("2021-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p21 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("solar elevation (degrees)") +
  ylab("") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

#2022
yr <- 2022
t <- seq.POSIXt(from = as.POSIXct("2022-01-01"), to = as.POSIXct("2022-12-31"), by = 60)
tsol <- getSunlightPosition(date = t, 
                            lat = hlat,
                            lon = hlon,
                            keep = "altitude")
tsol$soldeg <- tsol$altitude*180/pi

clicks_yr <- clicks_soldeg %>% filter(year == yr)
p22 <- ggplot(clicks_yr, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.9, 
           xmin = -77, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 1) +
  geom_density(data = tsol, aes(soldeg), size = 1, linetype = "dashed") +
  xlab("solar elevation (degrees)") +
  ylab("") +
  ylim(c(0,0.021)) + 
  annotate("text", label = yr, x = 55, y = 0.012, hjust=0, size = 5) +
  theme_classic() +
  theme(text = element_text(size = 18))

tiff("outputs/figures/diel_yearly.tiff",units="in", width=16,height=6,res=300)
(p15 | p16 | p17 | p18) / (p19 | p20 | p21 | p22)
dev.off()


