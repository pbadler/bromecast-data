# conduct individual level analysis of climate and competition
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
library(tidyr)
library(maps)

# load and clean 2021, 2022, and 2023 demography data
source("load_and_clean_demography_data.R")

# pull Daymet data for each site, multiple years
# this takes a few minutes, no need to run more than once
# source("pull_climate_data.R")

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
# 
# test <- merge(D,comp_ftypes1,all=T)
# colSums(is.na(test))
# table(test$SiteCode[is.na(test$annual)])
# 
# # figure out why demography and composition data don't match
# missingC <- test[is.na(test$annual),]
# missingC <- missingC[,c("SiteCode", "Year","Treatment","Transect","Distance")]
# 
# missingD <- test[is.na(test$Emerged),]
# 
# missingC[missingC$SiteCode=="Woodruff",]
# missingD[missingD$SiteCode=="Woodruff",]
allD_ft1 <- merge(D,comp_ftypes1,all.x=T)
allD_ft2 <- merge(D,comp_ftypes2,all.x=T)
allD_ft3 <- merge(D,comp_ftypes3,all.x=T)

#cleanup
rm(D, fgroups, comp_ftypes1,comp_ftypes2,comp_ftypes3)

# fit demography models
source("fit_demography_models.R")


