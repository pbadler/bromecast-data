
# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

# load packages
library(tidyr)
library(dplyr)
library(lubridate)
source("QAQC_functions.R")

###
### 1. Format growth and phenology data by site and year
###

# Sheep Station 2021-2022
source("format_growthphenology_SheepStation.R")



###
### 2. Flag suspicious growth and phenology data by site and year
###

doyear <- 2022 # growing season to do
dosite <- "SS" # code for focal site
source("flag_growthphenology.R")

###
### 3. Combine growth and phenology data from all sites and years
###

# to do

###
### 4. Put everything back together to do an analysis
###

source("example_analyses.R")


