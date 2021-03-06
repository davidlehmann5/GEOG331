#David Lehmann
#Activity 6
#Envioronmental Data Science
#4/1/2020

setwd("/Users/davidlehmann/Desktop/GEOG331/activity6/a06")

#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("GNPglaciers/GNPglaciers_1966.shp")
g1998 <- readOGR("GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("GNPglaciers/GNPglaciers_2005.shp")

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


#read in rgb imagery from landsat
redL <- raster("glacier_09_05_14/l08_red.tif")
greenL <- raster("glacier_09_05_14/l08_green.tif")
blueL <- raster("glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#subsetting the plot to zoom in on a few glaciers
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)
#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("NDVI/NDVI_",ndviYear[i],".tif"))
  
}

#look at single raster function
str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

#plot raster data 
plot(NDVIraster[[1]])

#attempting to put them on the same plot
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)




#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#question 4
par(mai=c(1,1,1,1))
plot(NDVIraster[[13]], axes = FALSE)
plot(g2015p, col= NA, border= "black", add = TRUE, fill = FALSE, axes = FALSE )

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#joining all glacier data
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#plot of the area for each glacier
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}

#question 5
area1966 <- sum(gAll$a1966m.sq)
area2015 <- sum(gAll$a2015m.sq)
percentChange <- (area2015 - area1966)/area2015 * 100
GLACDIFF <- c((gAll$a2015m.sq - gAll$a1966m.sq)/gAll$a2015m.sq * 100)
g2015p@data$GLACDIFF <- GLACDIFF

spplot(g2015p, "GLACDIFF")


#question6
gAll$GLACDIFF <- GLACDIFF
Boulder66 <- subset(g1966, GLACNAME == "Boulder Glacier")
Boulder98 <- subset(g1998, GLACNAME == "Boulder Glacier")
Boulder05 <- subset(g2005, GLACNAME == "Boulder Glacier")
Boulder15 <- subset(g2015, GLACNAME == "Boulder Glacier") 

#plot Boulder glacier melting
par(mai=c(1,1,1,1))
plotRGB(rgbL, ext=c(260000,280000,5400000,5440000), stretch="lin",  axes = TRUE,main = "Boulder Glacier -404.37% Glacier Loss")
#add polygons to plot
plot(Boulder66, col="tan3", border=NA, add=TRUE)
plot(Boulder98, col="royalblue3", add=TRUE, border=NA)
plot(Boulder05, col="darkgoldenrod4", add=TRUE, border=NA)
plot(Boulder15, col="tomato3", add=TRUE, border=NA)

#vegetation data analysis
#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]], diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

#plot index change
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data

plot(buffRaster)


#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply

head(meanChange)

#question 9
meanChange.df <- as.data.frame(meanChange)
meanChange1 <- subset(meanChange.df, zone > 0)

g2015p$meanChange <- meanChange1$mean
spplot(g2015p, "meanChange")


#question 11

NDVI.data <- rasterToPoints(NDVIraster[[1]])

NDVI.data1 <- as.data.frame(NDVI.data)
NDVI.mean <- mean(NDVI.data1$NDVI_2003)




