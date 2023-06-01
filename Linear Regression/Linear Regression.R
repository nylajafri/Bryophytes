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
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/Linear Regression") 


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


##Moss Area and Micro Cat have high p value
### Compare Data 
boxplot(bryophyte$moss_area_m2~bryophyte$water_pres, xlab='Urban Heat Island Index',
        ylab='Site Name')

boxplot(bryophyte$num_species~bryophyte$heat_island, xlab='Urban Heat Island Index',
        ylab='Site Name')

boxplot(bryophyte$num_col~bryophyte$heat_island, xlab='Urban Heat Island Index',
        ylab='Site Name')

shapiro.test(bryophyte$moss_area_m2) # To test normality
kruskal.test(bryophyte$moss_area_m2~bryophyte$dist_water_cat) #low p value means that there is variation across sites 
kruskal.test(bryophyte$num_species~bryophyte$heat_island)
kruskal.test(bryophyte$num_col~bryophyte$heat_island)


### Linear Regression Test 
## Moss Area 
modelarea <- lm(moss_area_m2 ~ can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelarea)

## Number of Colonies 
modelcolony <- lm(num_col ~ can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelcolony)

## Number of Species
modelspecies <- lm(num_species ~ can_cov + max_humidity + min_humidity, data = bryophyte)
summary(modelspecies)


### Print Table
tab_model(modelarea, modelcolony, modelspecies, 
          pred.labels = c("Intercept", "Canopy Cover (Percentage)", 
                          "Maximum Relative Humidity (over 1 year)", 
                          "Minimum Relative Humidity (over 1 year)"),  
          dv.labels = c("Moss Area (m^2)", "Number of Colonies", "Number of Species"), 
          title= "Linear Regression Analysis of Bryophytes")

### Plot Added Variable Linear Regressions 
avPlots(modelarea)
avPlots(modelcolony)
avPlots(modelspecies) 


##Plot Significant Figures
plot(bryophyte$can_cov, bryophyte$num_col, xlab = "Canopy Cover (Percentage)", ylab = "Number of Colonies")
abline(lm(bryophyte$num_col ~ bryophyte$can_cov), col="red") 

plot(bryophyte$can_cov, bryophyte$num_species, xlab = "Canopy Cover (Percentage)", ylab = "Number of Species")
abline(lm(bryophyte$num_species ~ bryophyte$can_cov), col="red") 


### Check Models 

AIC(modelarea)
AIC(modelcolony)
AIC(modelspecies)

