# prepare data for individual level analysis of climate and competition
# effects on cheatgrass fitness in satellite experiments

# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

# load packages
library(dplyr)
#library(tidyr)
library(maps)

# load and clean 2021, 2022, 2023, and 2024 demography data
source("load_and_clean_demography_data.R")

# pull Daymet data for each site, multiple years
# this takes a few minutes, no need to run more than once
# source("pull_climate_data.R")
# 2024 data won't be available til early 2025

# load site info and site climate data
source("load_site_climate_data.R")

# load and clean composition data, add functional group info
source("composition2functional_groups.R")

# merge demography site data
D <- merge(D,siteD,all.x=T)

# merge everything with composition data, check and clean

# fix a couple data entry errors
D$Distance[D$SiteCode=="Symstad2" & D$Distance==5.04] <- 5
D$Distance[D$SiteCode=="Symstad2" & D$Distance==5.17] <- 5.2


### This next block of code is to troubleshoot the demography-composition merge
test <- merge(D,comp_ftypes1,all=T)
colSums(is.na(test))
table(test$SiteCode[is.na(test$annual)])
# figure out why demography and composition data don't match
missingC <- test[is.na(test$annual),]
missingC <- missingC[,c("SiteCode", "Year","Treatment","Transect","Distance")]
missingC <- subset(missingC, Year==2024)

# fixable problems solved, now do the merge
allD_ft1 <- merge(D,comp_ftypes1,all.x=T)
write.csv(allD_ft1,"../deriveddata/all_plants_ftypes1.csv",row.names=F)
allD_ft2 <- merge(D,comp_ftypes2,all.x=T)
write.csv(allD_ft2,"../deriveddata/all_plants_ftypes2.csv",row.names=F)
allD_ft3 <- merge(D,comp_ftypes3,all.x=T)
write.csv(allD_ft3,"../deriveddata/all_plants_ftypes3.csv",row.names=F)

#cleanup
rm(D, fgroups, comp_ftypes1,comp_ftypes2,comp_ftypes3)




