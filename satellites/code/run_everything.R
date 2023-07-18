# do a not very thorough data cleaning and 
# then run preliminary analyses on satellite site data

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

# load and clean 2021 and 2022 demography data
source("load_and_clean_demography_data.R")

# pull Daymet data for each site, multiple years
# this takes a few minutes, no need to run more than once
# source("pull_climate_data.R")

# load site info and site climate data
source("load_site_climate_data.R")

# analyze site means for prob. reproduction, fecundity, fitness
# and make figures
source("site_means_analysis&figures.R")

# TO DO: format composition data
# assign functional groups
# load composition data
  # join to functional groups
  # aggregate functional groups to individual plant level
  # write to file

# analyze individual plant data
  # join demography data, site info/climate data, and functional group neighborhood data
  # do stats

