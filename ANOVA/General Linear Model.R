################################################
#
# FBQ S2023
# 
# Title: Bryophyte Generalized  Linear Model
#
# Author: Nyla Jafri (nylajafri@ucla.edu), Prada 
#
# Script version: 0.1
#
# R Version: 4.3.0
#
################################################

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/ANOVA") 

#read bryophyte data file as csv, reupload once data input is complete
bryophyte <- read.csv("FBQ_Data_Sheet.csv")
View(bryophyte)

#load
library(MASS)
library(rstatix)
library(readr)
library(sjPlot)
library(lme4)
library(ggplot2)
library(car)

#Kruskal 
#low p value means that there is variation across sites 
kruskal.test(bryophyte$num_species~bryophyte$heat_island) ##p-value = 0.02435   <-
kruskal.test(bryophyte$num_col~bryophyte$heat_island)     ##p-value = 0.01486   <-

kruskal.test(bryophyte$num_species~bryophyte$micro_cat)   ##p-value = 0.1185
kruskal.test(bryophyte$num_col~bryophyte$micro_cat)       ##p-value = 0.6353

kruskal.test(bryophyte$num_species~bryophyte$water_pres)  ##p-value = 0.06603
kruskal.test(bryophyte$num_col~bryophyte$water_pres)      ##p-value = 0.007997  <-

kruskal.test(bryophyte$num_species~bryophyte$growth_cat)  ##p-value = 0.000932  <-
kruskal.test(bryophyte$num_col~bryophyte$growth_cat)      ##p-value = 0.003115  <-

kruskal.test(bryophyte$num_species~bryophyte$dist_walk_cat)##p-value = 0.007594 <-
kruskal.test(bryophyte$num_col~bryophyte$dist_walk_cat)   ##p-value = 0.05615   

kruskal.test(bryophyte$num_species~bryophyte$dist_road_cat)##p-value = 0.006314 <-
kruskal.test(bryophyte$num_col~bryophyte$dist_road_cat)   ##p-value = 0.2409    <-

kruskal.test(bryophyte$num_species~bryophyte$dist_water_cat)##p-value = 0.2508
kruskal.test(bryophyte$num_col~bryophyte$dist_water_cat)  ##p-value = 0.495



#runglms 
speciesglm <- glm(num_species ~ heat_island + micro_cat + water_pres + growth_cat
                  + dist_walk_cat + dist_road_cat, family = poisson, data = bryophyte)
avPlots(speciesglm)


colonyglm <- glm(num_col ~ heat_island + micro_cat + water_pres + growth_cat
                 + dist_walk_cat + dist_road_cat, family = poisson, data = bryophyte)
avPlots(colonyglm)


#Table 
tab_model(colonyglm, speciesglm, 
          pred.labels = c("Intercept", "Urban Heat Island Index (over 1 year)", "Microhabitat",
                          "Water Present", "Growth Substrate", "Distance to Walkway", "Distance to Road"),  
          dv.labels = c("Number of Colonies", "Number of Species"))

#Significant Figures
plot(bryophyte$can_cov, bryophyte$num_col, xlab = "Canopy Cover (Percentage)", ylab = "Number of Colonies")
abline(lm(bryophyte$can_cov~bryophyte$num_col), col="red") # regression line (y~x)

plot(bryophyte$can_cov, bryophyte$num_species, xlab = "Canopy Cover (Percentage)", ylab = "Number of Species")
abline(lm(bryophyte$can_cov~bryophyte$num_species), col="red") # regression line (y~x)


AIC(mossareaglm)


