# Formatting Cheyenne phenology to be in same structure as Sheep Station

library(lubridate); library(tidyverse)

doyear <- 2022 # growing season to do
dosite <- "Cheyenne" # code for focal site

# Read in data
rawD <-read_csv("gardens/rawdata/WY_census_data_cleaned.csv")

# remove capital letters from column headers 
names(rawD) <- tolower(names(rawD))
