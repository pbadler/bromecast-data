# This code compiles species level Bromus tectorum "competitors" and assigns
# each unique species to a functional group

# Load libraries
library(tidyverse)

# Read in 2020-2021 composition data
comp20 <- read_csv("satellites/deriveddata/Satellite_2020-2021_Composition.csv")
# Read in 2021-2022 composition data
comp21 <- read_csv("satellites/deriveddata/Satellite_2021-2022_Composition.csv")

# Rename columns for brevity
comp20 %>% 
  select(sitecode = SiteCode,
         transect = `Transect (N, E, S, or W)`,
         treatment = `Treatment (control OR removal)`,
         distance_m = `Distance from center (m)`,
         species = Species,
         cover = Cover,
         litter_depth_cm = `Litter depth (if >1cm)`,
         notes = Notes) -> comp20

comp21 %>% 
  select(sitecode = SiteCode,
         transect = `Transect (N, E, S, or W)`,
         treatment = `Treatment (control OR removal)`,
         distance_m = `Distance from center (m)`,
         species = Species,
         cover = Cover,
         litter_depth_cm = `Litter depth (cm)`,
         notes = Notes) -> comp21

# Figure out unique species for each data set
unique(c(comp20$species, comp21$species)) %>% 
  sort() -> species_list

# Write csv file to code these as functional groups manually and to fix any issues
# write_csv(tibble(species = species_list), "satellites/deriveddata/species_list.csv")

# Read in updated csv
species_codes <- read_csv("satellites/deriveddata/species_list.csv")

# Make updates to names when needed
species_codes %>% 
  mutate(species = case_when(complete.cases(update) ~ update,
                             T ~ species)) %>% 
  select(-update) %>% 
  distinct() %>% 
  arrange(species) -> species_codes_to_check

# Write csv to check species codes
write_csv(species_codes_to_check, "satellites/deriveddata/species_codes_to_check.csv")
