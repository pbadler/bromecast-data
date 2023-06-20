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

# Combine species observation lists
comp_all <- rbind(comp20, comp21)

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

# Add information about where unknown coded species are from
species_codes_to_check %>% 
  filter(notes %in% c("not sure based on USDA database")) %>% 
  filter(species != "Draba repens" & species != "Epilobium spp")-> coded_spp

# Write a loop to go through coded species and figure out site
site_codes <- NULL
for (i in 1:nrow(coded_spp)){
  spp_temp <- coded_spp$species[i]
  site_temp <- comp_all %>% filter(species == spp_temp) %>% pull(sitecode)
  if(length(site_codes > 1)){
    site_codes[i] <- paste(unique(site_temp), sep = "_", collapse = "")
  }else{
    site_codes[i] <- site_temp
  }
}

# Put site back onto coded species data frame
coded_spp %>% 
  mutate(sitecode = site_codes) -> coded_spp

# Make %notin% operator
`%notin%` <- Negate(`%in%`)

# Add back the rest of the observations
species_codes_to_check %>% 
  mutate(sitecode = NA) %>% 
  filter(species %notin% coded_spp$species) -> uncoded_spp

species_codes_to_check <- rbind(coded_spp, uncoded_spp) %>% 
  arrange(species)

# Write csv to check species codes
#write_csv(species_codes_to_check, "satellites/deriveddata/species_codes_to_check.csv")

# Get summary stats for groupings
species_codes_to_check %>% 
  group_by(type, duration, c3c4) %>% 
  summarize(n = n())

