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



