#David Lehmann
#Activity 5
#Intro to date visualization

#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("stream_flow_data.csv",
                 na.strings = c("Eqp"))

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("2049867.csv")  

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#time for streamflow
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#time for precipitation
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)




#convert time from string with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#decimal year accounting for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

#times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#decimal year accounting for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))    

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

####Basic Plot Formatting####

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#plot with mean over all years and 2017
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim = c(0,90),
     xaxs = "i", yaxs = "i",  #remove axis gaps
     axes = FALSE) #no axes



#sd around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA)#no border
    
         #plot 2017
datD2017 <- datD[datD$year == 2017,]
ave2017 <- aggregate(datD2017$discharge, by=list(datD2017$doy), FUN="mean")
colnames(ave2017) <- c("doy","dailyAve")
lines(ave2017$doy,ave2017$dailyAve, 
              col = "blue")        
sd2017 <- aggregate(datD2017$discharge, by=list(datD2017$doy), FUN="sd")
colnames(sd2017) <- c("doy","dailySD")


polygon(c(ave2017$doy, rev(ave2017$doy)),#x coordinates
        c(ave2017$dailyAve-sd2017$dailySD,rev(ave2017$dailyAve+sd2017$dailySD)),#ycoord
        col=rgb(0.9, 0.2, 0.929,.2), #color that is semi-transparent
        border=NA)#no border

#adjusting the axis display
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
#legend
legend("topright", c("mean","1 standard deviation", "2017 discharge", "2017 standard deviation"), #legend items
       lwd=c(4,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"blue", rgb(0.9, 0.2, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#plot 2017
datD2017 <- datD[datD$year == 2017,]


#hydrograph 
datP2008<- datP[datP$year == 2008,]
Summer2017 <- datP2008[150<datP2008$doy & datP2008$doy < 250,]  
fullDays2017 <- aggregate(datP2008$hour, by=list(datP2008$doy), FUN="sum")
colnames(fullDays2017) <- c("doy","TotalHours")
truFullDays <- fullDays2017[fullDays2017$TotalHours == 276, ]


#plot with mean over all years and 2017
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))

plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim = c(0,90),
     xaxs = "i", yaxs = "i",  #remove axis gaps
     axes = FALSE) #no axes

#adjusting the axis display
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0, 360, by=30)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
ypoints <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
points(truFullDays$doy,ypoints , col = "red", lwd = 10)
legend("topright", c("mean","days of high precipitation"), #legend items
       lwd=c(4,NA),#lines
       col=c("black", "red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


#subsest discharge and precipitation within range of interest
hydroD1 <- datD[datD$doy >= 45 & datD$doy < 47 & datD$year == 2011,]
hydroP1 <- datP[datP$doy >= 45 & datP$doy < 47 & datP$year == 2011,]



min(hydroD1$discharge)
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD1$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD1$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP1$HPCP))+.5
#scale precipitation to fit on the 
hydroP1$pscale <- (((yh-yl)/(pm-pl)) * hydroP1$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD1$decDay,
     hydroD1$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP1)){
  polygon(c(hydroP1$decDay[i]-0.017,hydroP1$decDay[i]-0.017,
            hydroP1$decDay[i]+0.017,hydroP1$decDay[i]+0.017),
          c(yl,hydroP1$pscale[i],hydroP1$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#boxplots and violin plots
install.packages("ggplot2")
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#2016 and 2017 violin plots
seasons <- rep(c(0,1,2,3,4), times = c(73, 93, 93, 90, 16 ))
ave2017$seasons.f <- factor(seasons, labels = c("winter", "spring", "summer", "fall",
                                      "winter"))

ggplot(data= ave2017, aes(seasons.f,dailyAve), ) + 
  geom_violin() 

datD2016 <- datD[datD$year == 2016,]
ave2016 <- aggregate(datD2016$discharge, by=list(datD2016$doy), FUN="mean")
colnames(ave2016) <- c("doy","dailyAve")

seasons2 <- rep(c(0,1,2,3,4), times = c(73, 93, 93, 90, 17 ))
ave2016$seasons.f <- factor(seasons2, labels = c("winter", "spring", "summer", "fall",
                                                "winter"))

ggplot(data= ave2016, aes(seasons.f,dailyAve), ) + 
  geom_violin() 


