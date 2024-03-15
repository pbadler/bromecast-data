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

# merge demography and composition data, check and clean


test <- merge(D,comp_ftypes1,all=T)
colSums(is.na(test))
table(test$SiteCode[is.na(test$annual)])

# figure out why demography and composition data don't match
problems <- test[is.na(test$annual),]
problems <- problems[,c("SiteCode", "Year","Treatment","Transect")]
problems<-unique(problems,margin=2)
for(i in 1:nrow(problems)){
  tmp <- which(D$SiteCode==problems$SiteCode[i] &
                 D$Year==problems$Year[i] &
                 D$Treatment==problems$Treatment[i] &
                 D$Transect==problems$Transect[i])
  tmpD <- D[tmp,]
  tmp <- which(comp_ftypes1$SiteCode==problems$SiteCode[i] &
                 comp_ftypes1$Year==problems$Year[i] &
                 comp_ftypes1$Treatment==problems$Treatment[i] &
                 comp_ftypes1$Transect==problems$Transect[i])
  tmpC <- comp_ftypes1[tmp,]
  print(tmpD); print(tmpC)
}

# figure out lots of other missing matches for composition data
# EOARC: distances for removal entered wrong in composition