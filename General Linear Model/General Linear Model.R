################################################
#
# FBQ S2023
# 
# Title: Bryophyte Generalized Linear Model
#
# Author: Nyla Jafri (nylajafri@ucla.edu), Prada Pothong
#
# Script version: 0.1
#
# R Version: 4.3.0
#
################################################

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes/General Linear Model") 

### Read CSV Data 
bryophyte <- read.csv("FBQ_Data_Sheet.csv")
View(bryophyte)

### Dependencies 
library(MASS)
library(rstatix)
library(readr)
library(sjPlot)
library(lme4)
library(ggplot2)
library(car)

### Kruskal Wallis Test for significance 
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


### GLM for Each Applicable Dependent Variable 
speciesglm <- glm(num_species ~ heat_island + micro_cat + water_pres + growth_cat
                  + dist_walk_cat + dist_road_cat, family = poisson, data = bryophyte)
avPlots(speciesglm)


colonyglm <- glm(num_col ~ heat_island + micro_cat + water_pres + growth_cat
                 + dist_walk_cat + dist_road_cat, family = poisson, data = bryophyte)
avPlots(colonyglm)


### Table 
tab_model(colonyglm, speciesglm, 
          pred.labels = c("Intercept", "Urban Heat Island Index (over 1 year)", "Microhabitat",
                          "Water Present", "Growth Substrate", "Distance to Walkway", "Distance to Road"),  
          dv.labels = c("Number of Colonies", "Number of Species"))

### Significant Figures
plot(bryophyte$heat_island, bryophyte$num_species, xlab = "Heat Island Index (over 1 year)", ylab = "Number of Species")
abline(lm(bryophyte$heat_island ~ bryophyte$num_species), col="red") 

plot(bryophyte$heat_island, bryophyte$num_col, xlab = "Heat Island Index (over 1 year)", ylab = "Number of Colonies")
abline(lm(bryophyte$heat_island ~ bryophyte$num_col), col="red") 

plot(bryophyte$micro_cat, bryophyte$num_col, xlab = "Microhabitat Type", ylab = "Number of Colonies")
abline(lm(bryophyte$micro_cat ~ bryophyte$num_col), col="red") 

plot(bryophyte$water_pres, bryophyte$num_species, xlab = "Water Present", ylab = "Number of Species")
abline(lm(bryophyte$water_pres ~ bryophyte$num_species), col="red") 

plot(bryophyte$water_pres, bryophyte$num_col, xlab = "Water Present", ylab = "Number of Colonies")
abline(lm(bryophyte$water_pres ~ bryophyte$num_col), col="red") 

plot(bryophyte$growth_cat, bryophyte$num_col, xlab = "Growth Substrate", ylab = "Number of Colonies")
abline(lm(bryophyte$growth_cat ~ bryophyte$num_col), col="red")

plot(bryophyte$dist_walk_cat, bryophyte$num_col, xlab = "Distance to Walkway", ylab = "Number of Colonies")
abline(lm(bryophyte$dist_walk_cat ~ bryophyte$num_col), col="red") 

plot(bryophyte$dist_road_cat, bryophyte$num_col, xlab = "Distance to Road", ylab = "Number of Colonies")
abline(lm(bryophyte$dist_road_cat ~ bryophyte$num_col), col="red")

### Check Models (this got rid of Moss Area using GLM since its results are not integers)
AIC(mossareaglm)


