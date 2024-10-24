
# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
# library(rstudioapi)
# current_path <- getActiveDocumentContext()$path 
# setwd(dirname(current_path )) # set working directory to location of this file

# load packages

# MLV: this allows us to get the relative path easy if Bromecast is
# downloaded/accessed through an R Project
library(here) 
library(tidyr)
library(dplyr)
library(lubridate)

###
### 1. Format growth and phenology data by site and year
###

# Sheep Station 2021-2022
source(here("gardens/code/format_growthphenology_SheepStation.R"))

# Boise 2021-2022
source(here("gardens/code/format_growthphenology_Boise.R"))
# NA warning related to missing length values being turned into NAs

# Cheyenne 2021-2022
source(here("gardens/code/format_growthphenology_WY.R"))
# NA warning related to missing length values being turned into NAs

# Bring in QAQC functions
source(here("gardens/code/QAQC_functions.R"))

###
### 2. Flag suspicious growth and phenology data by site and year
###

doyear <- 2022 # growing season to do
dosite <- "CH" # code for focal site
source(here("gardens/code/flag_growthphenology.R"))

###
### 3. Combine growth and phenology data from all sites and years
###

# to do

###
### 4. Put everything back together to do an analysis
###

source("example_analyses.R")


