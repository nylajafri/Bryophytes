################################################
#
# FBQ S2023
# 
# Title: Bryophyte Linear Regression
#
# Author: Nyla Jafri (nylajafri@ucla.edu)
#
# Script version: 0.1
#
# R Version: 4.3.0
#
################################################

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/ANOVA") 


### Dependencies 
install.packages("sjPlot")
install.packages("lme4")
install.packages("ggplot2")

library(sjPlot)
library(lme4)
library(ggplot2)
library(car)


### Load Data
bryophyte <- read.csv("FBQ_Data_Sheet.csv")
View(bryophyte)


### Compare Data 
boxplot(bryophyte$uhii_year~bryophyte$site.name, xlab='Urban Heat Island Index',
        ylab='Site Name')
shapiro.test(bryophyte$moss_area_m2) # To test normality
kruskal.test(bryophyte$uhii_year~bryophyte$site.name) #low p value means that there is variation across sites 


### Linear Regression Test 
## Moss Area 
modelarea <- lm(moss_area_m2 ~ uhii_year + can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelarea)

## Number of Colonies 
modelcolony <- lm(num_col ~ uhii_year + can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelcolony)

## Number of Species
modelspecies <- lm(num_species ~ uhii_year + can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelspecies)


### Print Table
tab_model(modelarea, modelcolony, modelspecies, 
          pred.labels = c("Intercept", "Urban Heat Island Index (over 1 year)",
                          "Canopy Cover", "Maximum Humidity (over 1 year)", 
                          "Minimum Humidity (over 1 year)"),  
          dv.labels = c("Moss Area (m^2)", "Number of Colonies", "Number of Species"))

### Plot 
avPlots(modelarea)
avPlots(modelcolony)
avPlots(modelspecies)


### Check Models 

#AIC(modelarea, modelcolony, modelspecies)

AIC(modelarea)
AIC(modelcolony)
AIC(modelspecies)





### Ignore below 



vcov(modela)
#modela <- lm(moss_area_m2 ~ uhii_year + micro_cat + can_cov + dist_walk_cat + dist_road_cat + dist_water_cat 
#             + max_humidity + min_humidity + growth_cat, data = bryophyte)

