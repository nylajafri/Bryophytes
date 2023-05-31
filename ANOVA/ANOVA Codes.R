################################################
#
# FBQ S2023
# 
# Title: Bryophyte 2 Way ANOVA 
#
# Author: Nyla Jafri (nylajafri@ucla.edu)
#
# Script version: 0.1
#
# R Version: 4.3.0
#
################################################

### Useful References
#https://www.scribbr.com/statistics/anova-in-r/ 

### Dependencies
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/ANOVA") 

#Read in Data 
crop.data <- read.csv("FBQ_Data_Sheet_FINAL.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))

#One Way ANOVA 
two.way <- aov(yield ~ fertilizer + density, data = crop.data)

summary(two.way)

#Two Way ANOVA 
two.way <- aov(yield ~ fertilizer + density, data = crop.data)

summary(two.way)

#Blocking 
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)

summary(blocking)

#AIC Modeling 
library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)

#Check for Homoscedasticity 
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#Post-Hoc Test 
tukey.two.way<-TukeyHSD(two.way)

tukey.two.way

#Plot
tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


### MANOVA
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/ANOVA") 
bryophyte <- read.csv("FBQ_Data_Sheet.csv")
dependent_vars <- cbind(bryophyte$uhii_year, bryophyte$micro_cat, bryophyte$can_cov, bryophyte$dist_walk_cat, bryophyte$dist_road_cat, bryophyte$water_pres, bryophyte$max_humidity, bryophyte$min_humidity, bryophyte$growth_cat)
independent_var <- bryophyte$moss_area_m2

manova_model <- manova(dependent_vars ~ independent_var, data = bryophyte)
summary(manova_model)

library(MASS)

iris_lda <- lda(independent_var ~ dependent_vars, CV = F)
iris_lda

lda_df <- data.frame(
  species = bryophyte[, "Species"],
  lda = predict(iris_lda)$x
)
lda_df