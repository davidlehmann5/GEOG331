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






