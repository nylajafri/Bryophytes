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

