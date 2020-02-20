#David Lehmann
#Activity 3

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data

datW <- read.csv("C:/Users/dlehmann/Desktop/activity3/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)

#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("C:/Users/dlehmann/Desktop/activity3/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

install.packages(c("lubridate"))

#load a package into r environment
library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#QAQC
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  


#normalize lighting strikes and precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy


#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#assert the vectors are the same length
assert(length(datW$DD) == length(lightscale), "error: unequal values")


datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


lightscale_Wind <- (max(datW$wind.speed)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy


#make it empty to start and add in features
plot(datW$DD , datW$wind.speed, xlab = "Day of Year", ylab = "Wind Speed & lightning",
     type="l")
#plot wind only when there is wind 
#make the points semi-transparent
points(datW$DD[datW$wind.speed > 0], datW$wind.speed[datW$wind.speed > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

datW$air.tempQ3 <- ifelse(datW$wind.speed  >= 0 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$wind.speed > 1, NA, datW$air.tempQ1))

assert(datW$wind.speed > 0, "error: unequal values")


plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temp",
     type="n")
points(datW$DD, datW$soil.temp,
       col= rgb(95/255,158/255,160/255,.5), pch=15)
points(datW$DD, datW$air.temperature,
       col= "red", pch=10)


plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="n")
points(datW$DD, datW$soil.moisture,
       col= rgb(95/255,158/255,160/255,.5), pch=15)
points(datW$DD, datW$precipitation,
       col= "red", pch=15)

#data table

meanTemp <- mean(datW$air.temperature)
meanWindSpeed <- mean(datW$wind.speed)
meanSoilMoisture <- mean(datW$soil.moisture, na.rm = TRUE)
meanSoilTemp <- mean(datW$soil.temp, na.rm = TRUE)

#plots
#soil moisture
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="n")
points(datW$DD, datW$soil.moisture,
       col= rgb(95/255,158/255,160/255,.5), pch=15)

#soil temperature
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temp",
     type="n")
points(datW$DD, datW$soil.temp,
       col= rgb(95/255,158/255,160/255,.5), pch=15)

#air temperature
limitedAirTemp <- datW$air.temperature[datW$doy < 193]
limitedDays <- datW$DD[datW$doy < 193]

plot(limitedDays , limitedAirTemp, xlab = "Day of Year", ylab = "Air Temp",
     type="n")
points(limitedDays, limitedAirTemp,
       col= rgb(95/255,158/255,160/255,.5), pch=15)
#wind speed

limitedWindSpeed<- datW$wind.speed[datW$doy < 193]

plot(limitedDays ,limitedWindSpeed, xlab = "Day of Year", ylab = "Wind Speed",
     type="n")
points(limitedDays, limitedWindSpeed,
       col= rgb(95/255,158/255,160/255,.5), pch=15)



