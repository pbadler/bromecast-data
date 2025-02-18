# Create clean version of data for Van Ee probabilistic PCA manuscript

## Preliminaries ####
# Load libraries
library(tidyverse)

# Read in data
cg_full <- read_csv("gardens/deriveddata/cg_fullData_withFlags.csv")

## Data cleaning ####
# Drop out observations for flags of concern (i.e. that could affect survival or
# fitness)
cg_full %>%
  filter(!grepl("smut", note_standard_phen) &
           !grepl("smut", note_standard_harvest) &
           !grepl("herbivory", note_standard_phen) &
           !grepl("herbivory", note_standard_harvest) &
           !grepl("physical_damage", note_standard_phen) &
           !grepl("physical_damage", note_standard_harvest) &
           !grepl("seed_drop", note_standard_phen) &
           !grepl("seed_drop", note_standard_harvest) &
           !grepl("wrong_spp", note_standard_phen) &
           !grepl("wrong_spp", note_standard_harvest) &
           !grepl("missing", note_standard_harvest)) -> cg_clean
# What percent of observations are we losing?
1 - nrow(cg_clean) / nrow(cg_full)

# Get true positives (alive at last phenology check before harvest and
# successfully harvested) and true negatives (not alive at last phenology check
# before harvest and not harvested)
cg_clean %>% 
  filter(last_phen_status == "Y" & (inflor_mass + veg_mass) > 0) -> cg_clean_pos
cg_clean %>% 
  filter(last_phen_status == "N" & (inflor_mass + veg_mass) == 0) -> cg_clean_neg
# See how many additional observations this drops out
(nrow(cg_clean_pos) + nrow(cg_clean_neg)) / nrow(cg_clean)
# Only takes out an additional 5%
rbind(cg_clean_pos, cg_clean_neg) -> cg_model
# This is a very conservative cleaning of the data

# Remove all intermediate datasets
rm(list=setdiff(ls(), "cg_model"))

# Write csv for Justin to use for analysis
write_csv(cg_model, "gardens/deriveddata/cg_cleandata_VanEe_PPCA.csv")
