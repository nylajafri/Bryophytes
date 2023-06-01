#set working directory 
setwd("/Users/lindsayreedy/Desktop/Bryophytes-main")

library(tidyr)

#read bryophyte data file as csv
bryophyte = read.csv("/Users/lindsayreedy/Desktop/FBQ Data Sheet - Final DATA SHEETS.csv")
head(bryophyte)

#subset data to one species - bryum argenteum
bryum_argenteum_data = bryophyte[bryophyte$species_present == 'bryum_argenteum',]
bryum_argenteum_data

#drop empty rows to get smaller dataframe
bryum_argenteum_data = drop_na(bryum_argenteum_data, site.name)
bryum_argenteum_data

#checking number of rows to make sure we have the right number 
nrow(bryum_argenteum_data) 

#repeat for every species (12)

#funaria_hygrometrica
funaria_hygrometrica_data = bryophyte[bryophyte$species_present == 'funaria_hygrometrica',]
funaria_hygrometrica_data = drop_na(funaria_hygrometrica_data, site.name)
nrow(funaria_hygrometrica_data)

#ceratodon_sp
ceratodon_sp_data = subset(bryophyte, species_present == 'ceratodon_sp' | species_present == 'ceratodon_purpureus' ,)
ceratodon_sp_data 

nrow(ceratodon_sp_data)

#weissia_controversa
weissia_controversa_data = subset(bryophyte, species_present == 'weissia_controversa' ,)
nrow(weissia_controversa_data)

#bryum_sp
bryum_sp_data = subset(bryophyte, species_present == 'bryum_sp' ,)
nrow(bryum_sp_data)

#scleropodium_sp
scleropodium_sp_data = subset(bryophyte, species_present == 'scleropodium_sp' | species_present == 'scleropodium_julaceum' ,)
nrow(scleropodium_sp_data)

#tortula_muralis
tortula_muralis_data = subset(bryophyte, species_present == 'tortula_muralis' ,)
nrow(tortula_muralis_data)

#liverwort_leafy
liverwort_leafy_data = subset(bryophyte, species_present == 'liverwort_leafy' ,)
nrow(liverwort_leafy_data)

#liverwort_thalloid
liverwort_thalloid_data = subset(bryophyte, species_present == 'liverwort_thalloid' ,)
nrow(liverwort_thalloid_data)

#orthotrichum_sp
orthotrichum_sp_data = subset(bryophyte, species_present == 'orthotrichum_sp' ,)
nrow(orthotrichum_sp_data)


#create a beautiful data frame with all the information we need - averaged across species
Species <- c('bryum argenteum', 'funaria hygrometrica', 'ceratodon sp', 'weissia controversa', 'bryum sp', 'scleropodium sp', 'tortula muralis', 'liverwort sp (leafy)', 'liverwort sp (thalloid)', 'orthotrichum sp')
Count <- c(nrow(bryum_argenteum_data), nrow(funaria_hygrometrica_data), nrow(ceratodon_sp_data), nrow(weissia_controversa_data), nrow(bryum_sp_data), nrow(scleropodium_sp_data), nrow(tortula_muralis_data), nrow(liverwort_leafy_data), nrow(liverwort_thalloid_data), nrow(orthotrichum_sp_data))
avg_df <- data.frame(Species, Count)
avg_df

#adding number found in each microhabitat

#greenspace
Greenspace <- c(nrow(subset(bryum_argenteum_data, micro_hab == "greenspace")), nrow(subset(funaria_hygrometrica_data, micro_hab == "greenspace")), nrow(subset(ceratodon_sp_data, micro_hab == "greenspace")), nrow(subset(weissia_controversa_data, micro_hab == "greenspace")), nrow(subset(bryum_sp_data, micro_hab == "greenspace")), nrow(subset(scleropodium_sp_data, micro_hab == "greenspace")), nrow(subset(tortula_muralis_data, micro_hab == "greenspace")), nrow(subset(liverwort_leafy_data, micro_hab == "greenspace")), nrow(subset(liverwort_thalloid_data, micro_hab == "greenspace")), nrow(subset(orthotrichum_sp_data, micro_hab == "greenspace")))
avg_df$Greenspace <- Greenspace
avg_df

#waterway
Waterway <- c(nrow(subset(bryum_argenteum_data, micro_hab == "waterway")), nrow(subset(funaria_hygrometrica_data, micro_hab == "waterway")), nrow(subset(ceratodon_sp_data, micro_hab == "waterway")), nrow(subset(weissia_controversa_data, micro_hab == "waterway")), nrow(subset(bryum_sp_data, micro_hab == "waterway")), nrow(subset(scleropodium_sp_data, micro_hab == "waterway")), nrow(subset(tortula_muralis_data, micro_hab == "waterway")), nrow(subset(liverwort_leafy_data, micro_hab == "waterway")), nrow(subset(liverwort_thalloid_data, micro_hab == "waterway")), nrow(subset(orthotrichum_sp_data, micro_hab == "waterway")))
avg_df$Waterway <- Waterway
avg_df

#urban
Urban <- c(nrow(subset(bryum_argenteum_data, micro_hab == "urban")), nrow(subset(funaria_hygrometrica_data, micro_hab == "urban")), nrow(subset(ceratodon_sp_data, micro_hab == "urban")), nrow(subset(weissia_controversa_data, micro_hab == "urban")), nrow(subset(bryum_sp_data, micro_hab == "urban")), nrow(subset(scleropodium_sp_data, micro_hab == "urban")), nrow(subset(tortula_muralis_data, micro_hab == "urban")), nrow(subset(liverwort_leafy_data, micro_hab == "urban")), nrow(subset(liverwort_thalloid_data, micro_hab == "urban")), nrow(subset(orthotrichum_sp_data, micro_hab == "urban")))
avg_df$Urban <- Urban
avg_df

#residential
Residential <- c(nrow(subset(bryum_argenteum_data, micro_hab == "residential")), nrow(subset(funaria_hygrometrica_data, micro_hab == "residential")), nrow(subset(ceratodon_sp_data, micro_hab == "residential")), nrow(subset(weissia_controversa_data, micro_hab == "residential")), nrow(subset(bryum_sp_data, micro_hab == "residential")), nrow(subset(scleropodium_sp_data, micro_hab == "residential")), nrow(subset(tortula_muralis_data, micro_hab == "residential")), nrow(subset(liverwort_leafy_data, micro_hab == "residential")), nrow(subset(liverwort_thalloid_data, micro_hab == "residential")), nrow(subset(orthotrichum_sp_data, micro_hab == "residential")))
avg_df$Residential <- Residential
avg_df

#number in each UHII region
#just doing avg UHII
Avg_UHII <- c(mean(bryum_argenteum_data$uhii_year), mean(funaria_hygrometrica_data$uhii_year), mean(ceratodon_sp_data$uhii_year), mean(weissia_controversa_data$uhii_year), mean(bryum_sp_data$uhii_year), mean(scleropodium_sp_data$uhii_year), mean(tortula_muralis_data$uhii_year), mean(liverwort_leafy_data$uhii_year), mean(liverwort_thalloid_data$uhii_year), mean(orthotrichum_sp_data$uhii_year))
avg_df$UHII <- Avg_UHII
avg_df

#variables
Avg_maxhum <- c(mean(bryum_argenteum_data$max_humidity), mean(funaria_hygrometrica_data$max_humidity), mean(ceratodon_sp_data$max_humidity), mean(weissia_controversa_data$max_humidity), mean(bryum_sp_data$max_humidity), mean(scleropodium_sp_data$max_humidity), mean(tortula_muralis_data$max_humidity), mean(liverwort_leafy_data$max_humidity), mean(liverwort_thalloid_data$max_humidity), mean(orthotrichum_sp_data$max_humidity))
avg_df$MaxHumidity <- Avg_maxhum
avg_df

Avg_minhum <- c(mean(bryum_argenteum_data$min_humidity), mean(funaria_hygrometrica_data$min_humidity), mean(ceratodon_sp_data$min_humidity), mean(weissia_controversa_data$min_humidity), mean(bryum_sp_data$min_humidity), mean(scleropodium_sp_data$min_humidity), mean(tortula_muralis_data$min_humidity), mean(liverwort_leafy_data$min_humidity), mean(liverwort_thalloid_data$min_humidity), mean(orthotrichum_sp_data$min_humidity))
avg_df$MinHumidity <- Avg_minhum
avg_df

Avg_canopy <- c(mean(bryum_argenteum_data$can_cov), mean(funaria_hygrometrica_data$can_cov), mean(ceratodon_sp_data$can_cov), mean(weissia_controversa_data$can_cov), mean(bryum_sp_data$can_cov), mean(scleropodium_sp_data$can_cov), mean(tortula_muralis_data$can_cov), mean(liverwort_leafy_data$can_cov), mean(liverwort_thalloid_data$can_cov), mean(orthotrichum_sp_data$can_cov))
avg_df$CanopyCover <- Avg_canopy
avg_df


###maybe drop these two
Avg_dist_walk_cat <- c(mean(bryum_argenteum_data$dist_walk_cat), mean(funaria_hygrometrica_data$dist_walk_cat), mean(ceratodon_sp_data$dist_walk_cat), mean(weissia_controversa_data$dist_walk_cat), mean(bryum_sp_data$dist_walk_cat), mean(scleropodium_sp_data$dist_walk_cat), mean(tortula_muralis_data$dist_walk_cat), mean(liverwort_leafy_data$dist_walk_cat), mean(liverwort_thalloid_data$dist_walk_cat), mean(orthotrichum_sp_data$dist_walk_cat))
avg_df$DistanceWalkCat <- Avg_dist_walk_cat
avg_df

Avg_dist_road_cat <- c(mean(bryum_argenteum_data$dist_road_cat), mean(funaria_hygrometrica_data$dist_road_cat), mean(ceratodon_sp_data$dist_road_cat), mean(weissia_controversa_data$dist_road_cat), mean(bryum_sp_data$dist_road_cat), mean(scleropodium_sp_data$dist_road_cat), mean(tortula_muralis_data$dist_road_cat), mean(liverwort_leafy_data$dist_road_cat), mean(liverwort_thalloid_data$dist_road_cat), mean(orthotrichum_sp_data$dist_road_cat))
avg_df$DistanceRoadCat <- Avg_dist_road_cat
avg_df
###
fix_bryum_argenteum <- drop_na(bryum_argenteum_data, num_col)

Avg_col <- c(mean(fix_bryum_argenteum$num_col), mean(funaria_hygrometrica_data$num_col), mean(ceratodon_sp_data$num_col), mean(weissia_controversa_data$num_col), mean(bryum_sp_data$num_col), mean(scleropodium_sp_data$num_col), mean(tortula_muralis_data$num_col), mean(liverwort_leafy_data$num_col), mean(liverwort_thalloid_data$num_col), mean(orthotrichum_sp_data$num_col))
avg_df$Colonies <- Avg_col
avg_df

Avg_area <- c(mean(bryum_argenteum_data$moss_area_m2), mean(funaria_hygrometrica_data$moss_area_m2), mean(ceratodon_sp_data$moss_area_m2), mean(weissia_controversa_data$moss_area_m2), mean(bryum_sp_data$moss_area_m2), mean(scleropodium_sp_data$moss_area_m2), mean(tortula_muralis_data$moss_area_m2), mean(liverwort_leafy_data$moss_area_m2), mean(liverwort_thalloid_data$moss_area_m2), mean(orthotrichum_sp_data$moss_area_m2))
avg_df$MossArea <- Avg_area
avg_df
