################################################
#
# FBQ S2023
# 
# Title: Bryophyte Maximum and Minimum Humidity 
#
# Author: Nyla Jafri (nylajafri@ucla.edu)
#
# Script version: 0.2
#
# R Version: 4.3.0
#
################################################

### Useful References
#https://rfunctions.blogspot.com/2017/08/extracting-data-from-rasters-using.html

### Dependencies
install.packages('raster')
install.packages("sp")
install.packages("rgdal")

library(sp)
library(raster)
library(rgdal)

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/Humidity Data") 

### Assign Variables to Data Set 
maxhumid <- raster("GRIDMET_max_RH.tif")
minhumid <- raster("GRIDMET_min_RH.tif")

### Plot Maps 
plot(maxhumid, main="Average Maximum Relative Humidity", xlab="Longitude", ylab="Latitude")
plot(minhumid, main="Average Minimum Relative Humidity", xlab="Longitude", ylab="Latitude")

### Obtain Coordinates from Data Set CVS
bryophyte <- read.csv("FBQ_Data_Sheet_HUM.csv", sep = ",")
head(bryophyte)
location <- data.frame(lat = bryophyte[ ,11], lon = bryophyte [ ,10])
head(location) 
coordinates(location)<-c("lat","lon")
head(location)

### Obtain Corresponding Max Humidity 
valmax <- extract(x=maxhumid, y=location, small=TRUE, na.rm=TRUE)
maxdf <- data.frame(valmax)
maxdf
write.csv(maxdf, "FBQ_MAX_Humidity.csv", row.names=FALSE)

### Obtain Corresponding Min Humidity 
valmin <- extract(x=minhumid, y=location, small=TRUE, na.rm=TRUE)
mindf <- data.frame(valmin)
mindf
write.csv(mindf, "FBQ_MIN_Humidity.csv", row.names=FALSE)





