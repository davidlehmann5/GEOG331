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
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

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

#histogram shows the frequence of temp observations
#histogram for the first site in our levels
#main= is the title for the main arg
#use the name of the factor for this not the numeric index

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
