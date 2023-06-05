################################################
#
# FBQ S2023
# 
# Title: Final Code: Bryophyte Generalized Linear Model
#
# Author: Nyla Jafri (nylajafri@ucla.edu),   Prada Pothong
#
# Script version: 0.5 (idk how many its been at this point)
#
# R Version: 4.3.0
#
################################################

### Useful References 
#https://stackoverflow.com/questions/47839988/indicating-significance-with-ggplot2-in-a-boxplot-with-multiple-groups

### Set Working Directory
setwd("C:/Users/nylaj/Desktop/Code/Bryophytes") 


### Read CSV Data 
bryophyte <- read.csv("FBQ_Data_Sheet.csv")
View(bryophyte)


### Dependencies 
install.packages("sjPlot")
install.packages("lme4")
install.packages("ggplot2")
install.packages("tidyverse")

library(sjPlot) 
library(car) 
library(tidyverse)
library(lme4)
library(ggplot2)
library(ggsignif)
library(ggpubr)


###Test Normality 
shapiro.test(bryophyte$moss_area_m2)  #W = 0.57078, p-value < 2.2e-16
shapiro.test(bryophyte$num_species)   #W = 0.63541, p-value < 2.2e-16
shapiro.test(bryophyte$num_col)       #W = 0.62936, p-value < 2.2e-16


### Kruskal Wallis Test for significance (only categorical independent variables)
kruskal.test(bryophyte$num_species~bryophyte$heat_island) ##p-value = 0.02435   <-
kruskal.test(bryophyte$num_col~bryophyte$heat_island)     ##p-value = 0.01486   <-
kruskal.test(bryophyte$moss_area_m2~bryophyte$heat_island)##p-value = 0.02279   <-

kruskal.test(bryophyte$num_species~bryophyte$micro_cat)   ##p-value = 0.1185
kruskal.test(bryophyte$num_col~bryophyte$micro_cat)       ##p-value = 0.6353
kruskal.test(bryophyte$moss_area_m2~bryophyte$micro_cat)  ##p-value = 0.3262

kruskal.test(bryophyte$num_species~bryophyte$water_pres)  ##p-value = 0.06603
kruskal.test(bryophyte$num_col~bryophyte$water_pres)      ##p-value = 0.007997  <-
kruskal.test(bryophyte$moss_area_m2~bryophyte$water_pres) ##p-value = 0.006017  <-

kruskal.test(bryophyte$num_species~bryophyte$growth_cat)  ##p-value = 0.0003979  <-
kruskal.test(bryophyte$num_col~bryophyte$growth_cat)      ##p-value = 0.003115  <-
kruskal.test(bryophyte$moss_area_m2~bryophyte$growth_cat) ##p-value = 0.0006423  <-

kruskal.test(bryophyte$num_species~bryophyte$dist_walk_cat)   ##p-value = 0.007594 <-
kruskal.test(bryophyte$num_col~bryophyte$dist_walk_cat)       ##p-value = 0.05615
kruskal.test(bryophyte$moss_area_m2~bryophyte$dist_walk_cat)  ##p-value = 0.06336

kruskal.test(bryophyte$num_species~bryophyte$dist_road_cat)    ##p-value = 0.006314 <-
kruskal.test(bryophyte$num_col~bryophyte$dist_road_cat)        ##p-value = 0.2409    
kruskal.test(bryophyte$moss_area_m2~bryophyte$dist_road_cat)   ##p-value = 0.2461    

kruskal.test(bryophyte$num_species~bryophyte$dist_water_cat)  ##p-value = 0.2508
kruskal.test(bryophyte$num_col~bryophyte$dist_water_cat)      ##p-value = 0.495
kruskal.test(bryophyte$moss_area_m2~bryophyte$dist_water_cat) ##p-value = 0.1523


### Test Correlation Between Independent Variables (>0.5 marked with ->)
cor(bryophyte$heat_island, bryophyte$micro_cat) #[1] -0.01264691    
cor(bryophyte$heat_island, bryophyte$water_pres) #[1] -0.1369173    
cor(bryophyte$heat_island, bryophyte$growth_cat) #[1] -0.04346262
cor(bryophyte$heat_island, bryophyte$dist_walk_cat) # [1] -0.1296553 
->cor(bryophyte$heat_island, bryophyte$dist_road_cat) #NA
->cor(bryophyte$heat_island, bryophyte$dist_water_cat) #NA
cor(bryophyte$heat_island, bryophyte$can_cov)    #[1] 0.3783083  
->cor(bryophyte$heat_island, bryophyte$max_humidity) #[1] -0.7229078
->cor(bryophyte$heat_island, bryophyte$min_humidity) #[1] -0.7311055

cor(bryophyte$micro_cat, bryophyte$water_pres) #[1] -0.05157269
cor(bryophyte$micro_cat, bryophyte$growth_cat) #[1] 0.0613682
cor(bryophyte$micro_cat, bryophyte$dist_walk_cat) # [1] -0.2536591
->cor(bryophyte$micro_cat, bryophyte$dist_road_cat) #NA
->cor(bryophyte$micro_cat, bryophyte$dist_water_cat) #NA
cor(bryophyte$micro_cat, bryophyte$can_cov)    #[1] -0.07484592
cor(bryophyte$micro_cat, bryophyte$max_humidity) #[1] 0.0005335086
cor(bryophyte$micro_cat, bryophyte$min_humidity) #[1] -0.1555733

cor(bryophyte$water_pres, bryophyte$growth_cat) #[1] 0.1227016
cor(bryophyte$water_pres, bryophyte$dist_walk_cat) #[1] 0.1998676
->cor(bryophyte$water_pres, bryophyte$dist_road_cat) #NA
->cor(bryophyte$water_pres, bryophyte$dist_water_cat) #NA 
cor(bryophyte$water_pres, bryophyte$can_cov)    #[1] -0.07684849
cor(bryophyte$water_pres, bryophyte$max_humidity) #[1] -0.01557516
cor(bryophyte$water_pres, bryophyte$min_humidity) #[1] 0.1144755

cor(bryophyte$growth_cat, bryophyte$dist_walk_cat) #[1] 0.1832686
->cor(bryophyte$growth_cat, bryophyte$dist_road_cat) #NA
->cor(bryophyte$growth_cat, bryophyte$dist_water_cat) #NA
cor(bryophyte$growth_cat, bryophyte$can_cov)    #[1] -0.2090483
cor(bryophyte$growth_cat, bryophyte$max_humidity) #[1] 0.01780509
cor(bryophyte$growth_cat, bryophyte$min_humidity) #[1] 0.1156399

->cor(bryophyte$dist_walk_cat, bryophyte$dist_road_cat) #NA
->cor(bryophyte$dist_walk_cat, bryophyte$dist_water_cat) #NA
cor(bryophyte$dist_walk_cat, bryophyte$can_cov)    #[1] -0.2273508
cor(bryophyte$dist_walk_cat, bryophyte$max_humidity) #[1] 0.2015627
cor(bryophyte$dist_walk_cat, bryophyte$min_humidity) #[1] 0.2704722

->cor(bryophyte$dist_road_cat, bryophyte$dist_water_cat) #NA
->cor(bryophyte$dist_road_cat, bryophyte$can_cov)    #NA
->cor(bryophyte$dist_road_cat, bryophyte$max_humidity) #NA
->cor(bryophyte$dist_road_cat, bryophyte$min_humidity) #NA

->cor(bryophyte$dist_water_cat, bryophyte$can_cov)    #NA
->cor(bryophyte$dist_water_cat, bryophyte$max_humidity) #NA
->cor(bryophyte$dist_water_cat, bryophyte$min_humidity) #NA

cor(bryophyte$can_cov, bryophyte$max_humidity) #[1] -0.477757
cor(bryophyte$can_cov, bryophyte$min_humidity) #[1] -0.4155034

->cor(bryophyte$max_humidity, bryophyte$min_humidity) #[1] 0.7306313


### Test Correlation Between Dependent and Independent Variables 
cor(bryophyte$moss_area_m2, bryophyte$heat_island) #   [1] 0.1797815 
cor(bryophyte$moss_area_m2, bryophyte$micro_cat) #    [1] 0.125601
cor(bryophyte$moss_area_m2, bryophyte$water_pres) #   [1] 0.3158686
cor(bryophyte$moss_area_m2, bryophyte$growth_cat) #[1] -0.12505
cor(bryophyte$moss_area_m2, bryophyte$dist_walk_cat) # [1] -0.04312243
cor(bryophyte$moss_area_m2, bryophyte$dist_road_cat) # NA
cor(bryophyte$moss_area_m2, bryophyte$dist_water_cat) # NA
cor(bryophyte$moss_area_m2, bryophyte$can_cov)    # [1] 0.2140123
cor(bryophyte$moss_area_m2, bryophyte$max_humidity) #[1] -0.2384258
cor(bryophyte$moss_area_m2, bryophyte$min_humidity) #[1] -0.2638464

cor(bryophyte$num_col, bryophyte$heat_island) #    NA
cor(bryophyte$num_col, bryophyte$micro_cat) #    NA
cor(bryophyte$num_col, bryophyte$water_pres) #   NA
cor(bryophyte$num_col, bryophyte$growth_cat) # NA
cor(bryophyte$num_col, bryophyte$dist_walk_cat) # NA 
cor(bryophyte$num_col, bryophyte$dist_road_cat) # NA
cor(bryophyte$num_col, bryophyte$dist_water_cat) # NA
cor(bryophyte$num_col, bryophyte$can_cov)    # NA
cor(bryophyte$num_col, bryophyte$max_humidity) # NA
cor(bryophyte$num_col, bryophyte$min_humidity) # NA

cor(bryophyte$num_species, bryophyte$heat_island) #   NA 
cor(bryophyte$num_species, bryophyte$micro_cat) #    NA
cor(bryophyte$num_species, bryophyte$water_pres) #   NA
cor(bryophyte$num_species, bryophyte$growth_cat) #NA
cor(bryophyte$num_species, bryophyte$dist_walk_cat) # NA
cor(bryophyte$num_species, bryophyte$dist_road_cat) #NA
cor(bryophyte$num_species, bryophyte$dist_water_cat) #NA
cor(bryophyte$num_species, bryophyte$can_cov)    # NA
cor(bryophyte$num_species, bryophyte$max_humidity) # NA
cor(bryophyte$num_species, bryophyte$min_humidity) # NA


### Test Correlation Between Dependent Variables 
cor(bryophyte$moss_area_m2, bryophyte$num_col) #   NA
cor(bryophyte$moss_area_m2, bryophyte$num_species) #   NA
cor(bryophyte$num_col, bryophyte$num_species) # NA


### Check Colinearity 
vif(glm(num_species ~ heat_island + micro_cat + water_pres + growth_cat + dist_walk_cat + dist_road_cat + can_cov + max_humidity + min_humidity, family = poisson, data = bryophyte))
vif(glm(num_col ~ heat_island + micro_cat + water_pres + growth_cat + dist_walk_cat + dist_road_cat + can_cov + max_humidity + min_humidity, family = poisson, data = bryophyte))
vif(glm((moss_area_m2 + 1) ~ heat_island + micro_cat + water_pres + growth_cat + dist_walk_cat + dist_road_cat + can_cov + max_humidity + min_humidity, family = Gamma, data = bryophyte))


### GLM for Each Applicable Dependent Variable (Find lowest AIC) 
areaglm <- glm((moss_area_m2 +1) ~ heat_island + micro_cat + water_pres + growth_cat + can_cov + min_humidity, family = Gamma, data = bryophyte) #Added 1 since Gamma cannot work with 0 
vif(glm((moss_area_m2 +1) ~ heat_island + micro_cat + water_pres + growth_cat + can_cov + min_humidity, family = poisson, data = bryophyte))
avPlots(areaglm)
tab_model(areaglm)
AIC(areaglm) #[1] -826.1631
summary(areaglm)

speciesglm <- glm(num_species ~ heat_island + micro_cat + water_pres + can_cov + growth_cat + dist_road_cat, family = poisson, data = bryophyte)
vif(glm(num_species ~ heat_island + micro_cat + water_pres + dist_walk_cat + dist_road_cat + can_cov, family = poisson, data = bryophyte))
avPlots(speciesglm)
tab_model(speciesglm)
AIC(speciesglm) #[1] 187.3201
summary(speciesglm)


colonyglm <- glm(num_col ~ heat_island + micro_cat + water_pres + growth_cat 
                 + can_cov + max_humidity + min_humidity + dist_road_cat, family = poisson, data = bryophyte)
vif(glm(num_col ~ heat_island + micro_cat + water_pres + growth_cat 
        + dist_road_cat + can_cov + max_humidity + min_humidity, family = poisson, data = bryophyte))
avPlots(colonyglm)
tab_model(colonyglm)
AIC(colonyglm) #[1] 2213.366
summary(colonyglm)


### Final Tables

tab_model(areaglm, colonyglm, speciesglm)
tab_model(areaglm, colonyglm, speciesglm,
          pred.labels = c("Intercept", "Urban Heat Island Index (degree (Celsius) hours per day, over 1 year)", "Microhabitat",
                          "Water Present", "Growth Substrate", "Canopy Cover (Percentage)", "Minimum Relative Humidity (Percentage, over 1 year)", "Maximum Relative Humidity (Percentage, over 1 year)",
                          "Distance to Road"),  
          dv.labels = c("Bryophyte Area (m^2)", "Number of Colonies", "Number of Species"), 
          title = "General Linear Model Analysis of Bryophytes")


### Significant Figures (Box Plots for Categorical and Scatter Plots with Line of Best Fit for Continuous)
boxplot(bryophyte$moss_area_m2 ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Bryophyte Area (m^2)")

boxplot(bryophyte$num_species ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Number of Species")

plot(bryophyte$can_cov, bryophyte$num_species, xlab = "Canopy Cover (Percentage)", ylab = "Number of Species")
abline(lm(bryophyte$num_species ~ bryophyte$can_cov), col="red")

boxplot(bryophyte$num_species ~ bryophyte$dist_road_cat, xlab = "Distance to Road", ylab = "Number of Species")

boxplot(bryophyte$num_col ~ bryophyte$heat_island, xlab = "Heat Island Index (degree (Celsius) hours per day, over 1 year)", ylab = "Number of Colonies")

boxplot(bryophyte$num_col ~ bryophyte$micro_cat, xlab = "Microhabitat", ylab = "Number of Colonies")

boxplot(bryophyte$num_col ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Number of Colonies")

boxplot(bryophyte$num_col ~ bryophyte$growth_cat, xlab = "Growth Substrate", ylab = "Number of Colonies")

boxplot(bryophyte$num_col ~ bryophyte$dist_road_cat, xlab = "Distance to Road", ylab = "Number of Colonies")

plot(bryophyte$can_cov, bryophyte$num_col, xlab = "Canopy Cover (Percentage)", ylab = "Number of Colonies")     #did not use ggplot for consistent aesthetics with box plots
abline(lm(bryophyte$num_col ~ bryophyte$can_cov), col="red") 

plot(bryophyte$min_humidity, bryophyte$num_col, xlab = "Minimum Relative Humidity (Percentage, over 1 year)", ylab = "Number of Colonies")
abline(lm(bryophyte$num_col ~ bryophyte$min_humidity), col="red") 

plot(bryophyte$max_humidity, bryophyte$num_col, xlab = "Maximum Relative Humidity (Percentage, over 1 year)", ylab = "Number of Colonies")
abline(lm(bryophyte$num_col ~ bryophyte$max_humidity), col="red") 


### Other Plots 
##Panel for UHHI
bryophyte$heat_island = as.character(bryophyte$heat_island)
cpr = list( c("0", "1"), c("0", "2"), c("0", "3"), 
                       c("1", "2"), c("1", "3"), c("2", "3") )

heatarea=
ggplot(data=bryophyte, mapping=aes(x=heat_island, y=moss_area_m2)) +
  geom_boxplot(fill="gray") + 
  xlab("Heat Island Index (degree (Celsius) hours per day, over 1 year)") + 
  ylab("Bryophyte Area (m^2)") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01,
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
heatcolony=
ggplot(data=bryophyte, mapping=aes(x=heat_island, y=num_col)) +
  geom_boxplot(fill="gray") + 
  xlab("Heat Island Index (degree (Celsius) hours per day, over 1 year)") + 
  ylab("Number of Colonies") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01,
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
heatspecies=
ggplot(data=bryophyte, mapping=aes(x=heat_island, y=num_species)) +
  geom_boxplot(fill="gray") + 
  xlab("Heat Island Index (degree (Celsius) hours per day, over 1 year)") + 
  ylab("Number of Species") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01, 
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
ggarrange(heatarea, heatcolony, heatspecies + rremove("x.text"),  labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


##Panel for Microhabitat
bryophyte$micro_cat = as.character(bryophyte$micro_cat)
my_comparisons = list( c("0", "1"), c("0", "2"), c("0", "3"), 
                       c("1", "2"), c("1", "3"), c("2", "3") )
marea=
  ggplot(data=bryophyte, mapping=aes(x=micro_cat, y=moss_area_m2)) +
  geom_boxplot(fill="gray") + 
  xlab("Microhabitat") + 
  ylab("Bryophyte Area (m^2)") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01,
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
mcolony=
  ggplot(data=bryophyte, mapping=aes(x=micro_cat, y=num_col)) +
  geom_boxplot(fill="gray") + 
  xlab("Microhabitat") + 
  ylab("Number of Colonies") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01,
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
mspecies=
  ggplot(data=bryophyte, mapping=aes(x=micro_cat, y=num_species)) +
  geom_boxplot(fill="gray") + 
  xlab("Microhabitat") + 
  ylab("Number of Species") + 
  theme_bw() + 
  stat_compare_means(comparisons = cpr, tip.length=0.01,
                     label = "p.signif", test = "kruskal.test" , 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
ggarrange(marea, mcolony, mspecies + rremove("x.text"),  labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


### No Significance Star Included Plots 
attach(mtcars)
par(mfrow=c(1,3))
boxplot(bryophyte$moss_area_m2 ~ bryophyte$heat_island, xlab = "Urban Heat Island Index (degree (Celsius) hours per day, over 1 year)", ylab = "Bryophyte Area (m^2)")
boxplot(bryophyte$num_col ~ bryophyte$heat_island, xlab = "Urban Heat Island Index (degree (Celsius) hours per day, over 1 year)", ylab = "Number of Colonies")
boxplot(bryophyte$num_species ~ bryophyte$heat_island, xlab = "Urban Heat Island Index (degree (Celsius) hours per day, over 1 year)", ylab = "Number of Species")

boxplot(bryophyte$moss_area_m2 ~ bryophyte$micro_cat, xlab = "Microhabitat", ylab = "Bryophyte Area (m^2)")
boxplot(bryophyte$num_col ~ bryophyte$micro_cat, xlab = "Microhabitat", ylab = "Number of Colonies")
boxplot(bryophyte$num_species ~ bryophyte$micro_cat, xlab = "Microhabitat", ylab = "Number of Species")

boxplot(bryophyte$moss_area_m2 ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Bryophyte Area (m^2)")
boxplot(bryophyte$num_col ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Number of Colonies")
boxplot(bryophyte$num_species ~ bryophyte$water_pres, xlab = "Water Present", ylab = "Number of Species")

plot(bryophyte$moss_area_m2 ~ bryophyte$can_cov, xlab = "Canopy Cover (Percentage)", ylab = "Bryophyte Area (m^2)")
abline(lm(bryophyte$moss_area_m2 ~ bryophyte$can_cov), col="red")
plot(bryophyte$num_col ~ bryophyte$can_cov, xlab = "Canopy Cover (Percentage)", ylab = "Number of Colonies")
abline(lm(bryophyte$num_col ~ bryophyte$can_cov), col="red") 
plot(bryophyte$num_species ~ bryophyte$can_cov, xlab = "Canopy Cover (Percentage)", ylab = "Number of Species")
abline(lm(bryophyte$num_species ~ bryophyte$can_cov), col="red")


### Check Model Final 
AIC()