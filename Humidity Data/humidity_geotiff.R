################################################
#
# FBQ S2023
# 
# Title: Bryophyte Maximum and Minimum Humidity 
#
# Author: Nyla Jafri (nylajafri@ucla.edu)
#
# Script version: 0.1
#
# R Version: 4.3.0
#
################################################

### Useful References
#https://www.youtube.com/watch?v=LlA9ufirDwc 
#https://gsp.humboldt.edu/olm/R/03_03_RasterFiles.html 
#https://rfunctions.blogspot.com/2017/08/extracting-data-from-rasters-using.html

### Dependencies
install.packages('raster')
install.packages("sp")
install.packages("rgdal")
install.packages("readxl")

library(sp)
library(raster)
library(rgdal)
library(readxl)

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/Humidity Data") 

### Assign Variables to Data Set 
maxhumid <- raster("GRIDMET_max_RH.tif")
maxhumid
minhumid <- raster("GRIDMET_min_RH.tif")

### Plot Maps 
plot(maxhumid, main="Average Maximum Relative Humidity", xlab="Longitude",ylab="Latitude")
plot(minhumid, main="Average Minimum Relative Humidity", xlab="Longitude",ylab="Latitude")

### Obtain Coordinates from Data Set Excel 
dfc <- read_excel("FBQ_Data_Sheet.xlsx")
xc<- as.numeric (unlist(dfc[ ,8]))
yc<- as.numeric (unlist(dfc[ ,9]))
xy <- data.frame(x=xc, y=yc)
print(xy)


### Obtain Corresponding Max Humidity 
?extract
maxdf.xy <- cbind(extract(maxhumid, xy, df=T), xy)
maxdf.xy

### Obtain Corresponding Min Humidity 
mindf.xy <- cbind(extract(minhumid, xy, df=T), xy) 
mindf.xy











### No NAs 

library(sp)
library(raster)
library(rgdal)
library(readxl)

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/Humidity Data") 

### Assign Variables to Data Set 
maxhumid <- raster("GRIDMET_max_RH.tif")
minhumid <- raster("GRIDMET_min_RH.tif")

### Plot Maps 
plot(maxhumid, main="Average Maximum Relative Humidity", xlab="Longitude",ylab="Latitude")
plot(minhumid, main="Average Minimum Relative Humidity", xlab="Longitude",ylab="Latitude")

### Obtain Coordinates from Data Set Excel 
dfc <- read_excel("FBQ_Data_Sheet_Excel_No_NA.xlsx")
xc<- as.numeric (unlist(dfc[ ,8]))
yc<- as.numeric (unlist(dfc[ ,9]))
xy <- data.frame(x=xc, y=yc)
print(xy)

coordinates(xy)<-c("x","y")
xy
val<-extract(x=maxhumid, y=xy)
val








#### CSV 

# Load a CSV that contains colmns of X and Y coordinate values

install.packages("raster")
library(raster)
clim<-raster(choose.files())
plot(clim, main="Annual Mean Temperature", xlab="Longitude",ylab="Latitude",cex.axis=1.3, cex.lab=1.4, cex.main=1.5,col=rev(heat.colors(10)))
places<-read.csv(choose.files(), head=T, sep=";")
coords<-data.frame(lon=places[,2], lat=places[,3])
coordinates(coords)<-c("lon","lat")
val<-extract(x=clim, y=coords)

##############
library(sp)
library(raster)
library(rgdal)
library(readxl)

setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/Humidity Data") 

maxhumid <- raster("GRIDMET_max_RH.tif")

plot(maxhumid, main="Average Maximum Relative Humidity", xlab="Longitude",ylab="Latitude")

bryophyte <- read.csv("FBQ_Data_Sheet_Csv_No_NA.csv", sep = ",")
head(bryophyte)

location <- data.frame(lat = bryophyte[ ,9], lon = bryophyte [ ,8])
head(location) 
coordinates(location)<-c("lat","lon")
head(location)
val <- extract(x=maxhumid, y=location, small=TRUE, na.rm=FALSE)
val

######################


coordinates(pointCoordinates)= ~ pointCoordinates[ ,8] + pointCoordinates[ ,9]   # Convert to a coordinates object

# Extract the values using the coordinates
rasValue=raster::extract(maxhumid, pointCoordinates)

# Add the pixel values back into the data frame
pointCoordinates=data.frame(pointCoordinates,rasValue)

