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
### Test Normality
shapiro.test(bryophyte$moss_area_m2)  #W = 0.57078, p-value < 2.2e-16
shapiro.test(bryophyte$num_species)   #W = 0.63541, p-value < 2.2e-16
shapiro.test(bryophyte$num_col)       #W = 0.62936, p-value < 2.2e-16

### Kruskal Wallis Test 
#low p value means that there is variation across indep var 
kruskal.test(bryophyte$moss_area_m2~bryophyte$can_cov)        #p-value = 0.2976
kruskal.test(bryophyte$moss_area_m2~bryophyte$max_humidity)   #p-value = 0.03173
kruskal.test(bryophyte$moss_area_m2~bryophyte$min_humidity)   #p-value = 0.03173

kruskal.test(bryophyte$num_species~bryophyte$can_cov)         #p-value = 0.3135
kruskal.test(bryophyte$num_species~bryophyte$max_humidity)    #p-value = 0.02855
kruskal.test(bryophyte$num_species~bryophyte$min_humidity)    #p-value = 0.02855

kruskal.test(bryophyte$num_col~bryophyte$can_cov)             #p-value = 0.3404
kruskal.test(bryophyte$num_col~bryophyte$max_humidity)        #p-value = 0.008885
kruskal.test(bryophyte$num_col~bryophyte$min_humidity)        #p-value = 0.008885


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

