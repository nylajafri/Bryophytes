#notes from joey's presentation

rm(list = ls())
setwd("~/Moss/")

#data dependencies 

install.packages("ggplot2")
library(ggplot2)

moss.data <- read.csv( "~/Moss/", header=T, sep = ",", stringsAsFactors = F)
head(moss.data)
tail(moss.data)
View(moss.data)

#if want to only observe certain rows 

moss.data2 <- moss.data[1:__, ] 
view (moss.data2)