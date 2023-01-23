################################################################################
## figures_final.R
##
## Will Oestreich
################################################################################
## Clean set of figures
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


####### FIGURE 1. MAP + PHOTO + SPECTROGRAM METHODS ############################



####### FIGURE 2. SEASONAL & DIEL PATTERNS #####################################
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

pa <- ggplot(allmonths, aes(x = as.factor(month), y = perc)) + geom_boxplot() +
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
  annotate("text", label = "Jan", x = 1, y = 0, size = 5) +
  annotate("text", label = "Feb", x = 2, y = 0, size = 5) +
  annotate("text", label = "Mar", x = 3, y = 0, size = 5) +
  annotate("text", label = "Apr", x = 4, y = 0, size = 5) +
  annotate("text", label = "May", x = 5, y = 0, size = 5) +
  annotate("text", label = "Jun", x = 6, y = 0, size = 5) +
  annotate("text", label = "Jul", x = 7, y = 0, size = 5) +
  annotate("text", label = "Aug", x = 8, y = 0, size = 5) +
  annotate("text", label = "Sep", x = 9, y = 0, size = 5) +
  annotate("text", label = "Oct", x = 10, y = 0, size = 5) +
  annotate("text", label = "Nov", x = 11, y = 0, size = 5) +
  annotate("text", label = "Dec", x = 12, y = 0, size = 5) +
  annotate("text", label = "A", x = 0.7, y = 99, size = 5, fontface = "bold") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        text = element_text(size = 18))

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

pb <- ggplot(clicks_soldeg, aes(soldeg)) +
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = -12, xmax = 0, ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray", alpha = 0.95, 
           xmin = -80, xmax = -12, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = -12, linetype="dashed", color = "black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  geom_density(size = 2) +
  geom_density(data = tsol, aes(soldeg), size = 2, linetype = "dashed") +
  annotate("label", label = "Night\n11.3 clicks/hr", x = -44.5, y = 0.0007, fill = "white", size = 5) +
  annotate("label", label = "Dusk/Dawn\n10.3 clicks/hr", x = -6, y = 0.0007, fill = "white", size = 5) +
  annotate("label", label = "Day\n13.9 clicks/hr", x = 32.5, y = 0.0007, fill = "white", size = 5) +
  xlab("solar elevation (degrees)") +
  ylab("density") +
  ylim(c(0,0.015)) +
  scale_x_continuous(limits=c(-80, 80), breaks=c(-75,-50,-25,-0,25,50,75), expand = c(0, 0)) +
  geom_segment(aes(x=36, y=0.014, xend=48, yend=0.014),color="black",size=2) +
  geom_segment(aes(x=36, y=0.011, xend=48, yend=0.011),color="black",linetype="dashed",size=2) +
  annotate("text", label = "Detected clicks", x = 49, y = 0.014, hjust=0, size = 5) +
  annotate("text", label = "Annual distribution\nof solar elevation", x = 49, y = 0.011, hjust=0, size = 5) +
  annotate("text", label = "B", x = -76, y = 0.015, size = 5, fontface = "bold") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 18))

tiff("outputs/figures/final/Fig2.tiff",units="in", width=10,height=8,res=300)
pa/pb
dev.off()


####### FIGURE 3. 3-PANEL INTERANNUAL ##########################################
enso$date <- mdy(enso$ym)
annual_perc$date <- as.Date(paste(as.character(annual_perc$year),"-07-01",sep = ""))
diel$date <- as.Date(paste(as.character(diel$year),"-07-01",sep = ""))
## running mean
wsize = 3
##
enso$date <- mdy(enso$ym)
monthly$date <- as.Date(paste(as.character(monthly$year),as.character(monthly$month),"15",sep = "-"))
monthly$perc_rm <- rollapply(monthly$perc,wsize,mean,fill=NA,na.rm = TRUE)
monthly$ratio_rm <- rollapply(monthly$ratio,wsize,mean,fill=NA,na.rm = TRUE)

pa <- ggplot(monthly, aes(x=date,y=perc_rm)) + geom_line(color="gray") +
  geom_line(data=annual_perc,aes(x=date,y=perc)) + 
  geom_point(data=annual_perc,aes(x=date,y=perc),color="black", fill="black", shape=24, size = 3) +
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
  annotate("text", label = "2015", x = as.Date("2015-07-01"), y = 20, size = 3) +
  annotate("text", label = "2016", x = as.Date("2016-07-01"), y = 20, size = 3) +
  annotate("text", label = "2017", x = as.Date("2017-07-01"), y = 20, size = 3) +
  annotate("text", label = "2018", x = as.Date("2018-07-01"), y = 20, size = 3) +
  annotate("text", label = "2019", x = as.Date("2019-07-01"), y = 20, size = 3) +
  annotate("text", label = "2020", x = as.Date("2020-07-01"), y = 20, size = 3) +
  annotate("text", label = "2021", x = as.Date("2021-07-01"), y = 20, size = 3) +
  annotate("text", label = "2022", x = as.Date("2022-07-01"), y = 20, size = 3) +
  annotate("text", label = "*", x = as.Date("2015-07-01"), y = 50, size = 5) +
  annotate("text", label = "A", x = as.Date("2015-03-01"), y = 78, size = 4, fontface = "bold") +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(20,80)) +
  xlab("") +
  ylab("% recording days\n with cachalot clicks") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank())

pb <- ggplot(diel, aes(x=date,y=ratio)) + geom_point(color="black", fill="black", shape=24, size = 3) + geom_line() +
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
  annotate("text", label = "*", x = as.Date("2015-07-01"), y = 1.7, size = 5) +
  annotate("text", label = "B", x = as.Date("2015-03-01"), y = 1.95, size = 4, fontface = "bold") +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(0.65,2)) +
  xlab("") +
  ylab("click rate ratio\n(day:night)") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank())

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
  annotate("text", label = "C", x = as.Date("2015-03-01"), y = 2.4, size = 4, fontface = "bold") +
  xlim(c(as.Date("2015-01-01"),as.Date("2022-12-31"))) +
  ylim(c(-2.5,2.5)) +
  xlab("") +
  ylab("Multivariate\nENSO Index") +
  scale_fill_manual(values=c("blue", "grey", "red")) +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank())

tiff("outputs/figures/final/Fig3.tiff",units="in", width=6,height=6,res=300)
pa/pb/pc
dev.off()

####### FIGURE 4. % presence vs. ENSO (3 panels) ###############################
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

pa <- ggplot(lags_monthly,aes(lag,perc_acf)) + geom_point(color="black", fill="black", shape=21, size = 2) + geom_line() +
  geom_hline(yintercept = 0.2,linetype = "dashed") + ##this value of 0.2 for the 95% CI comes from ccf() above
  xlim(-15,0) +
  xlab("Lag (months)") +
  ylab("Cross correlation") +
  annotate("text", label = "% presence (anomaly) vs. Multivariate ENSO Index", x = -6, y = 0.08, size = 4) +
  annotate("text", label = "A", x = -15, y = 0.38, size = 4, fontface = "bold") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# panel B: scatter plot with 8-month lag (peak of cross correlation function)
d$mei_lag8 <- lag(d$mei,8)
lag_mod <- lm(d$perc_anom~d$mei_lag8)
lm_pts <- data.frame(matrix(ncol = 2, nrow = 2))
colnames(lm_pts) <- c("x","y")
lm_pts$x <- c(min(d$mei_lag8,na.rm = TRUE),max(d$mei_lag8,na.rm = TRUE))
lm_pts$y <- c(lag_mod$coefficients[1] + lag_mod$coefficients[2]*min(d$mei_lag8,na.rm = TRUE),
                 lag_mod$coefficients[1] + lag_mod$coefficients[2]*max(d$mei_lag8,na.rm = TRUE))

pb <- ggplot(d,aes(x=mei_lag8,y=perc_anom)) + geom_point(size=2) +
  geom_line(data=lm_pts, aes(x=x,y=y), linetype = "dashed") +
  xlab("Multivariate ENSO Index\n(8-month lag)") +
  ylab("Monthly % of recording days\nwith clicks present (anomaly)") +
  annotate("text", x = 1.6, y = -16, label = paste('R^2 ==',round(summary(lag_mod)$r.squared, 2)), 
           size=4, parse = TRUE, hjust = 0) +
  annotate("text", x = 1.6, y = -25, label = "p = 9e-06", 
           size=4, hjust = 0) +
  annotate("text", label = "B", x = -1.8, y = 48, size = 4, fontface = "bold") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())

tiff("outputs/figures/final/Fig4.tiff",units="in", width=6,height=4.5,res=300)
pa/pb
dev.off()


####### FIGURE 5. % presence vs. ENSO (3 panels) ###############################



