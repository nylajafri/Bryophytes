#notes from joey's presentation

rm(list = ls())
setwd("~/Bryophytes/")

#data dependencies 

install.packages("ggplot2")
library(ggplot2)

bry.data <- read.csv( "~/Bryophytes/", header=T, sep = ",", stringsAsFactors = F)
head(bry.data)
tail(bry.data)
View(bry.data)

#if want to only observe certain rows 

bry.data2 <- bry.data[1:__, ] 
view(bry.data2)