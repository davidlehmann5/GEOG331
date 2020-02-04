#Activity 2
#David Lehmann

#Vectors

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#[] square brackets indicate subsetting
#look at the first tree height
heights[1]
#look at the 2nd and 3rd tree heights
heights[2:3]

#matrices

#help function that will work on any function to provide an explanation of the function
help(matrices)

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#matrix formation is always [row, column]

#subsetting a matrix
Mat.bycol[1,2]

#look at all the values in row 1
Mat.bycol[1,]

#look at all the values in column 2
Mat.bycol[,2]

#read in weather station file from the data folder
datW <- read.csv("y:\\Students\\hkropp\\a02\\2011124.csv")

#view the data by clicking on it in the global enviornment
#get more information about the data frame
str(datW)

#create the correct data format
#many more formatting dates online
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#creating a column for dates with only years
#also indicates it should be numeric data
datW$dateF <- as.numeric(format(datW$dateF, "%Y"))

#find all unique site names
levels(datW$NAME)

#mean max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#need to include "na.rm = true" to ignore missing data
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#average daily temp
#halfway between min and max temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)


#aggregate function calculate means acrossing and indexing the value
#get mean across all sites
#by function is the variable to index over
#FUN represents the function used
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change column output names to make more sense
#MAAT stands for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#need to reference the level output or look at the row of data
datW$siteN <- as.numeric(datW$NAME)

#Question 2
n <- c(3.2,7.6,10.0, 11, 5.5 )#numeric vector
i <- as.integer(n) #integer vector
c <- c("siteA", "siteB", "siteC", "siteD", "siteD")#character data
f <- factor(c)


#histogram shows the frequence of temp observations
#histogram for the first site in our levels
#main= is the title for the main arg
#use the name of the factor for this not the numeric index
par(mfrow=c(2,2)) #add all histograms to the same plot
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add the mean and standard deviation to better understand the histogram
#mean, col is the color, lwd is the thickness of the line

abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
          col = "tomato3",
          lwd = 3)

#add the standard deviation line below the mean

abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE)-sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)

#standard deviation line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#MANDAN EXPERIMENT STATION, ND US histogram

hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue",
     border="white")

abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm = TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm = TRUE)-sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#MORRISVILLE 6 SW, NY US histogram

hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green",
     border="white")

abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm = TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm = TRUE)-sd(datW$TAVE[datW$siteN == 5], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#LIVERMORE, CA US histogram
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="orange",
     border="white")

abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE)-sd(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#probability distribution for Aberdeen

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#0.01682526
#temperatures below freezing are very unlikely, only about 1% of the time

#pnorm with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#0.1343358,

#pnorm with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#probability of temperatures from 0-5

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#temperature above 20 degrees C

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
## [1] 18.51026
#qnorm will return the value associated with a probability
abTemp <- mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE)
#mean increase by 4 degrees
qnorm(0.95,
      abTemp + 4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Aberdeen precipation histogram



sumYear <- aggregate(datW$PRCP, by=list(datW$NAME, datW$dateF), FUN="sum",na.rm=TRUE)

hist(sumYear$x[sumYear$Group.1 == "ABERDEEN, WA US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

MeanYear <- aggregate(sumYear$x, by=list(sumYear$Group.1), FUN="mean",na.rm=TRUE)



