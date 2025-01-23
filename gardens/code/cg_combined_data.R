# Code to bring together all common garden data

# Bring in 2022 common garden data (harvest and date of first flowering)
source("gardens/code/format_harvest_allSites_2022.R")
# Bring in 2023 common garden data (harvest and date of first flowering)
source("gardens/code/format_harvest_allSites_2023.R")

# Add seed_count_total column to 2023 data with NAs (did not count seeds this
# year)
cg_2023$seed_count_total <- NA

# Set relevant columns to be factors
cols_to_factor <- c("plantID", "site", "year", "density",
                    "albedo", "block", "plot","genotype")

cg_2022 %>% 
  mutate_at(cols_to_factor, factor) -> cg_2022

cg_2023 %>% 
  mutate_at(cols_to_factor, factor) -> cg_2023

# Bring together all data
rbind(cg_2022, cg_2023) -> cg_full

# Filter out data that had at least one of the following conditions:
# (1) smut
# (2) resurrection [marked as dead then alive]
# (3) herbivory
# (4) seed drop
# (5) wrong spp

cg_full %>% 
  filter(!grepl("smut", note_standard_phen) &
           !grepl("smut", note_standard_harvest) &
           !grepl("resurrection", note_standard_phen) &
           !grepl("herbivory", note_standard_phen) &
           !grepl("herbivory", note_standard_harvest) &
           !grepl("physical_damage", note_standard_phen) &
           !grepl("physical_damage", note_standard_harvest) &
           !grepl("seed_drop", note_standard_phen) &
           !grepl("seed_drop", note_standard_harvest) &
           !grepl("wrong_spp", note_standard_phen) & 
           !grepl("wrong_spp", note_standard_harvest) & 
           !grepl("missing", note_standard_harvest)) -> cg_clean

# How many plants were alive at last check and were successfully harvested 
cg_clean %>% 
  filter(last_phen_status == "Y" & (biomass_whole + inflor_mass) > 0) %>% 
  nrow() -> alive_harvest

# How many plants were not alive at last check, but were harvested
cg_clean %>% 
  filter(last_phen_status == "N" & (biomass_whole + inflor_mass) > 0) %>% 
  nrow() -> not_alive_harvest

# How many plants were not alive at last check and were not harvested (true zeros)
cg_clean %>% 
  filter(last_phen_status == "N" & (biomass_whole + inflor_mass) == 0) %>% 
  nrow() -> not_alive_not_harvest

# How many plants were alive but were not harvested
cg_clean %>% 
  filter(last_phen_status == "Y" & (biomass_whole + inflor_mass) == 0) %>% 
  nrow() -> alive_not_harvest

# Where are these all from?
cg_clean %>% 
  filter(complete.cases(first_flower) & (biomass_whole + inflor_mass) == 0) %>% 
  group_by(site, year) %>% 
  summarize(n = n())
