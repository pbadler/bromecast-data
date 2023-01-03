
# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

# load packages
library(tidyr)
library(lubridate)

doyear <- 2022 # growing season to do
dosite <- "SS" # code for focal site

# import raw growth and phenology data
rawD <- read.csv("../rawdata/phenology_Sheep_Station_2021-2022_MASTER.csv",header=T)

# put Date in date format
rawD$Date <- mdy(rawD$Date)

# remove capital letters from column headers 
# (this will make it easier to harmonize data across sites)
names(rawD) <- tolower(names(rawD))

# reformat continuous plot numbering to block and subplot
names(rawD)[which(names(rawD)=="plot")] <- "cum_plot"
tmp <- data.frame(block=sort(rep(1:10,4)),
                  plot=rep(1:4,10),
                  cum_plot=1:40)
rawD<-merge(rawD,tmp)
rm(tmp)

# assign each individual plant a unique ID, link to grid positions
tmp <- rawD[,c("site","block","plot","x","y")]
plant_key <- unique(tmp,MARGIN=2)
plant_key$plantID <- paste0(dosite,doyear,"_",1:nrow(plant_key))
rawD <- merge(rawD,plant_key)
#write.csv(plant_key,...)
