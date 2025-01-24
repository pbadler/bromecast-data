# Common garden data cleaning -- This code cleans and brings together phenology
# and harvest datasets for 2022

## Preliminaries ####

# Load libraries
library(tidyverse); library(mgcv); library(gratia); library(geomtextpath);
library(here); library(readr); library(brms); library(RcppCNPy); library(mbend)

## Format Sheep Station phenology data ####

# Read in derived phenology data
phen_SS <- read_csv("gardens/deriveddata/SS2022_growthphenology_with_harvest.csv")
# Read in plant ID info
ids_SS <- read_csv("gardens/deriveddata/SS2022_plantID.csv")
# Read in flagging data
flags_SS <- read_csv("gardens/deriveddata/SS2022_flags.csv")
gardens <- read_csv("gardens/rawdata/garden_treatments.csv")
# Merge together datasets
phen_id_SS <- merge(phen_SS, ids_SS)

# Rename 'garden' column and remove cum_plot column
gardens %>% 
  mutate(site = garden) %>% 
  dplyr::select(-cum_plot, -garden) -> gardens_sub

# Merge together datasets
phen_id_garden_SS <- merge(phen_id_SS, gardens_sub)

# Merge together datasets
phen_SS <- merge(phen_id_garden_SS, flags_SS)

# Set appropriate factors for variables
phen_SS %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = as.factor(growout),
         density = as.factor(density),
         gravel = as.factor(gravel),
         site = as.factor(site),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_SS

## Format Boise phenology data ####

# Read in derived phenology data
phen_Boise <- read_csv("gardens/deriveddata/Boise2022_growthphenology_by_plantID.csv")
# Read in plant ID info
ids_Boise <- read_csv("gardens/deriveddata/Boise2022_plantID.csv")
# Read in flagging data
flags_Boise <- read_csv("gardens/deriveddata/Boise2022_flags.csv")

# Merge together datasets
phen_id_Boise <- merge(phen_Boise, ids_Boise)

# Merge together datasets
phen_id_garden_Boise <- merge(phen_id_Boise, gardens_sub)

# Merge together datasets
phen_Boise <- merge(phen_id_garden_Boise, flags_Boise)

# Set appropriate factors for variables
phen_Boise %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = NA,
         density = as.factor(density),
         gravel = as.factor(gravel),
         site = as.factor(site),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_Boise

## Format Cheyenne phenology data ####
# Read in derived phenology data
phen_CH <- read_csv("gardens/deriveddata/CH2022_growthphenology_by_plantID.csv")
# Read in plant ID info
ids_CH <- read_csv("gardens/deriveddata/CH2022_plantID.csv")
# Read in flagging data
flags_CH <- read_csv("gardens/deriveddata/CH2022_flags.csv")

# Merge together datasets
phen_id_CH <- merge(phen_CH, ids_CH)

# Merge together datasets
phen_CH <- merge(phen_id_CH, flags_CH)

# Set appropriate factors for variables
phen_CH %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = NA,
         density = as.factor(case_when(density == "high" ~ "hi",
                                       density == "low" ~ "lo")),
         gravel = as.factor(gravel),
         site = as.factor(site),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_CH

## Merge all site datasets together ####
# Remove tillers from phenology of SS because other sites didn't collect this
phen <- rbind(phen_SS %>% dplyr::select(-tillers), phen_Boise, phen_CH)

# Check to see if we have the right number of plants
length(unique(phen$plantID)) # 16000
phen %>% 
  group_by(site, block) %>% 
  summarize(n = n_distinct(plantID)) %>% 
  print(n = Inf)
# Everything looks good!

# Create note_standard column and organize notes
phen %>% 
  mutate(resurrection = ifelse(complete.cases(resurrection_date), "resurrection", NA),
         frost_heave = ifelse(frost_heave == "Y", "frost_heave", NA),
         physical_damage = ifelse(other == "physicaldamage", "physical_damage", NA),
         seed_drop = ifelse(other == "seedheadmissing", "seed_drop", NA),
         smut = ifelse(other == "smut", "smut", NA),
         herbivory = ifelse(herbivory == "Y", "herbivory", NA),
         bad_position = ifelse(missing_plant == "Y", "bad_position", NA)) %>% 
  unite("note_standard", c(resurrection, frost_heave, physical_damage,
                           seed_drop, smut, herbivory, bad_position), sep="_", remove = FALSE, na.rm = TRUE) %>% 
  mutate(note_standard = ifelse(note_standard == "", NA, note_standard)) %>% 
  select(plantID, note_standard) %>% 
  filter(complete.cases(note_standard)) %>% 
  distinct() %>% 
  group_by(plantID) %>% 
  mutate(note_standard = paste0(note_standard, collapse = "_")) %>% 
  ungroup() %>% 
  distinct() %>% 
  # Manually fix standard notes because I can't figure out a fast way to do this!!
  mutate(note_standard = case_when(note_standard %in% c("frost_heave_frost_heave_herbivory",
                                                        "frost_heave_herbivory",
                                                        "frost_heave_herbivory_frost_heave",
                                                        "frost_heave_herbivory_herbivory",
                                                        "herbivory_frost_heave_herbivory",
                                                        "frost_heave_frost_heave_herbivory_herbivory",
                                                        "herbivory_frost_heave_herbivory_frost_heave",
                                                        "herbivory_frost_heave") ~ "frost_heave_herbivory",
                                   note_standard %in% c("resurrection_herbivory_resurrection",
                                                        "resurrection_resurrection_herbivory") ~ "resurrection_herbivory",
                                   note_standard %in% c("resurrection_resurrection_frost_heave",
                                                        "resurrection_frost_heave_resurrection") ~ "resurrection_frost_heave",
                                   note_standard %in% c("resurrection_resurrection_frost_heave_herbivory",
                                                        "resurrection_frost_heave_herbivory_resurrection_resurrection_frost_heave",
                                                        "resurrection_frost_heave_resurrection_herbivory_resurrection",
                                                        "resurrection_herbivory_resurrection_frost_heave_resurrection",
                                                        "resurrection_frost_heave_herbivory_resurrection",
                                                        "resurrection_resurrection_frost_heave_resurrection_herbivory",
                                                        "resurrection_resurrection_herbivory_resurrection_frost_heave",
                                                        "resurrection_frost_heave_resurrection_resurrection_herbivory",
                                                        "resurrection_resurrection_frost_heave_herbivory_resurrection_frost_heave",
                                                        "resurrection_frost_heave_resurrection_resurrection_frost_heave_herbivory",
                                                        "resurrection_frost_heave_herbivory_resurrection_frost_heave_resurrection",
                                                        "resurrection_resurrection_frost_heave_resurrection_frost_heave_herbivory",
                                                        "resurrection_herbivory_resurrection_frost_heave_herbivory_resurrection_resurrection_frost_heave",
                                                        "resurrection_herbivory_resurrection_resurrection_frost_heave") ~ "resurrection_frost_heave_herbivory",
                                   note_standard %in% c("physical_damage_physical_damage_herbivory",
                                                        "physical_damage_herbivory_physical_damage") ~ "physical_damage_herbivory",
                                   note_standard %in% c("seed_drop_herbivory_seed_drop", "seed_drop_seed_drop_herbivory") ~ "seed_drop_herbivory",
                                   note_standard == "resurrection_seed_drop_resurrection_seed_drop_herbivory" ~ "resurrection_seed_drop_herbivory",
                                   note_standard == "resurrection_frost_heave_seed_drop_resurrection_seed_drop_herbivory_resurrection_seed_drop" ~ "resurrection_frost_heave_seed_drop_herbivory",
                                   note_standard == "resurrection" ~ "resurrection",
                                   note_standard == "herbivory" ~ "herbivory",
                                   note_standard == "frost_heave" ~ "frost_heave",
                                   note_standard == "smut" ~ "smut",
                                   note_standard == "smut_smut_herbivory" ~ "smut_herbivory",
                                   note_standard == "seed_drop" ~ "seed_drop",
                                   note_standard == "resurrection_seed_drop" ~ "resurrection_seed_drop",
                                   T ~ "FIX ME")) -> phen_notes_summary

# Join notes summary with phenology data at plant level
merge(phen, phen_notes_summary, all.x = T) -> phen
  
# Figure out the first flowering date for each plant
phen %>% 
  filter(v %in% c("FG", "FB", "FP", "FX")) %>% 
  group_by(plantID) %>%
  # Gets minimum day of flowering
  slice(which.min(jday)) %>% 
  select(plantID, site, gravel, density, x, y, block, plot, genotype, first_flower = jday, v_phen = v, note_standard) %>% 
  ungroup() -> flowered

# Create "not in" operator
`%notin%` <- Negate(`%in%`)

# Get all plants that did not flower
phen %>% 
  filter(plantID %notin% flowered$plantID) %>% 
  group_by(plantID) %>%
  # Gets one observation for each plantID (jday doesn't really matter here)
  sample_n(size = 1) %>%  
  select(plantID, site, gravel, density, x, y, block, plot, genotype, first_flower = jday, v_phen = v, note_standard) %>% 
  ungroup() %>% 
  mutate(first_flower = NA) -> not_flowered

flower_phen <- rbind(flowered, not_flowered) %>% 
  arrange(site, block, plot, x, y)

# Also figure out plant status at last phenology check
# Figure out the status of the plant at the last phenology check
phen %>%
  filter(complete.cases(live)) %>% 
  group_by(plantID) %>% 
  slice_max(jday) %>% 
  ungroup() %>% 
  select(plantID, last_phen_status = live)-> last_phen

# Add last phenology status to data sheet
flower_phen %>% 
  merge(last_phen, all.x = T) -> flower_phen

# Remove intermediate datasets for phenology
rm(list=setdiff(ls(), c("flower_phen", "cg_2023")))


## Format Sheep Station harvest data ####
# Read in harvest data (last updated 11/29/2023)
harvest <- read_csv("gardens/rawdata/CG_harvest2022 - 11-29-2023.csv")

# Make column names all lower case
names(harvest) <- tolower(names(harvest))

# Filter to be just sheep station and make unique ID
harvest %>% 
  filter(site == "SheepStation") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestSS

# Create data subset 1: plants that did not survive to harvest
harvestSS %>% 
  filter(is.na(seed_count_sub) & is.na(biomass_whole) & is.na(seed_count_sub)) %>% 
  mutate(seed_count_total = 0) -> calib_data_nosurvive

# Create data subset 2: plants that survived but didn't make seeds
harvestSS %>% 
  filter(live == "Y" & (biomass_whole)> 0 & seed_count_sub == 0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds

# Up through SS plot 14, there was a distinction between seed_count_whole and
# seed_count_sub such that we only will consider all seeds counted if they are
# in the seed_count_whole column. After that plot (and for the rest of the data
# across sites), all counts were recorded as seed_count_sub. For those, if they
# were less than 50 seeds, we can assume all seeds were counted.

# Create data subset 3: harvested seeds that were subsetted for plots (1-14)
harvestSS %>% 
  filter(plot <= 14) %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 1) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%
  # There are 3 reps that have 1 seed with 0 weight
  mutate(seed_count_total = ifelse(seed_count_total == Inf, 1, seed_count_total)) %>% 
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_14

# Create data subset 3: harvested seeds that were subsetted for plots (15-50)
harvestSS %>% 
  filter(plot > 14) %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 1) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%
  # There are 3 reps that have 1 seed with 0 weight
  mutate(seed_count_total = ifelse(seed_count_total == Inf, 1, seed_count_total)) %>% 
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_rest

# Create data subset 4: harvested seeds where all were counted
harvestSS %>% 
  filter(complete.cases(seed_count_whole)) %>% 
  mutate(seed_count_total = seed_count_whole) -> calib_data_whole

# Create data subset 5
harvestSS %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0) %>% 
  mutate(diff = inflor_mass_sub - inflor_mass) %>% 
  # Most of these are just rounding errors
  filter(diff < 0.05) %>% 
  # If ratio is less than 1 or basically equal to 1, there shouldn't be that
  # many more than 50 seeds so just set to be 50 seeds
  mutate(seed_count_total = round(seed_count_sub * 1)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> add_extras

# Create data subset 6
harvestSS %>% 
  filter(id %in% c("8_high_white_8_2", "19_high_white_4_4",
                   "17_high_black_7_1", "19_high_white_5_3")) %>% 
  mutate(seed_count_total = c(0,0,1,1)) -> add_extras2

# Bind data subsets 1-5 back together
rbind(calib_data_subsetted_14, calib_data_subsetted_rest, calib_data_whole,
      calib_data_noseeds, calib_data_nosurvive, add_extras, add_extras2) %>% 
  arrange(id) -> data_allSS

data_allSS %>% 
  distinct() -> data_allSS

# Read in notes information
notes_actions <- read_csv("gardens/deriveddata/SS2022_harvest_notes_actions.csv")
notes_actions %>% 
  filter(action == "action") %>% 
  select(notes, standard_note) -> notes_actions_keep

# Merge together with the rest of the data
merge(data_allSS, notes_actions_keep, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, NA),
         herbivory = ifelse(standard_note == "herbivory", 1, NA),
         mortality = ifelse(standard_note == "mortality", 1, NA),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, NA),
         seed_drop = ifelse(standard_note == "seeddrop", 1,NA),
         smut = ifelse(standard_note == "smut", 1, NA),
         wrong_spp = ifelse(standard_note == "wrongspp", 1, NA)) -> data_allSS

# Add additional seed drop information
data_allSS %>% 
  mutate(seed_drop = ifelse(drop_seed == "Y" | drop_seed == "y", 1, seed_drop)) -> data_allSS

## Format Boise Low (WI) harvest data ####
# Filter to be just WI and make unique ID
harvest %>% 
  filter(site == "BoiseLow") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestWI

# Remove duplicates for 9 entries
harvestWI %>% 
  distinct() -> harvestWI

# Create data subset 1: plants that did not survive to harvest
harvestWI %>% 
  filter(is.na(live) & is.na(seed_count_sub) & is.na(biomass_whole)) %>% 
  mutate(seed_count_total = NA) -> calib_data_nosurvive_WI

# Create data subset 2: plants that survived but didn't make seeds
harvestWI %>% 
  filter(live == "Y" & (biomass_whole)>0 & seed_count_sub ==0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds_WI

# Create data subset 3: harvested seeds that were subsetted
harvestWI %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 1 & seed_count_sub >=50) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_WI

# Create data subset 4
harvestWI %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0 & seed_count_sub >=50) %>% 
  mutate(diff = inflor_mass_sub - inflor_mass) %>% 
  # Most of these are just rounding errors
  filter(diff < 0.05) %>% 
  # Because this is so close to 1, we can assume 50 seeds is reasonable so set
  # ratio to 1
  mutate(seed_count_total = round(seed_count_sub * 1)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> calib_data_notsubset_WI

# Create data subset 5: harvested seeds that weren't subsetted because all were
# counted
harvestWI %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  filter(seed_count_sub < 50 & seed_count_sub > 0) %>% 
  mutate(seed_count_total = seed_count_sub) -> calib_data_nosubsetWI

# Bind data subsets 1-5 back together
rbind(calib_data_subsetted_WI, calib_data_noseeds_WI, calib_data_nosurvive_WI,
      calib_data_notsubset_WI, calib_data_nosubsetWI) %>% 
  arrange(id) -> data_most_WI

# Still missing 1 observation that has issues
harvestWI %>% 
  filter(id == "7.4_low_white_10_4") %>% 
  mutate(seed_count_total = 0) -> add_extra_WI 

data_allWI <- rbind(data_most_WI, add_extra_WI)

# Read in notes information
notes_actions_WI <- read_csv("gardens/deriveddata/WI2022_harvest_notes_actions.csv")
notes_actions_WI %>% 
  filter(action == "action") %>% 
  select(notes, standard_note) -> notes_actions_keep_WI

# Merge together with the rest of the data
merge(data_allWI, notes_actions_keep_WI, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, NA),
         all_unripe = ifelse(standard_note == "allunripe", 1, NA),
         herbivory = ifelse(standard_note == "herbivory", 1, NA),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, NA),
         seed_drop = case_when(standard_note == "seeddrop" ~ 1,
                               T ~ NA),
         smut = ifelse(standard_note == "smut", 1, NA),
         location_issue = ifelse(standard_note == "location_issue", 1, NA)) -> data_allWI

# Add additional seed drop information
data_allWI %>% 
  mutate(seed_drop = ifelse(drop_seed == "Y" | drop_seed == "y", 1, seed_drop)) -> data_allWI

## Format Boise High (BA) harvest data ####

# Filter to be just BA and make unique ID
harvest %>% 
  filter(site == "BoiseHigh") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestBA

# Create data subset 1: plants that did not survive to harvest
harvestBA %>% 
  filter(is.na(live) & is.na(seed_count_sub) & is.na(biomass_whole)) %>% 
  mutate(seed_count_total = 0) -> calib_data_nosurvive_BA

# Create data subset 2: plants that survived but didn't make seeds
harvestBA %>% 
  filter(live == "Y" & (biomass_whole)>0 & seed_count_sub ==0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds_BA

# Create data subset 3: harvested seeds that were subsetted
harvestBA %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 1 & seed_count_sub >=50) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_BA

# Create data subset 4
harvestBA %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0 & seed_count_sub >=50) %>% 
  mutate(diff = inflor_mass_sub - inflor_mass) %>% 
  # Most of these are just rounding errors
  filter(diff < 0.05) %>% 
  # Because this is so close to 1, we can assume 50 seeds is reasonable so set
  # ratio to 1
  mutate(seed_count_total = round(seed_count_sub * 1)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> calib_data_notsubset_BA

# Create data subset 5: harvested seeds that weren't subsetted because all were
# counted
harvestBA %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  filter(seed_count_sub < 50 & seed_count_sub > 0) %>% 
  mutate(seed_count_total = seed_count_sub) -> calib_data_nosubset_BA

# Bind data subsets 1-4 back together
rbind(calib_data_subsetted_BA, calib_data_noseeds_BA, calib_data_nosurvive_BA, calib_data_notsubset_BA,
      calib_data_nosubset_BA) %>% 
  arrange(id) -> data_allBA

# Read in notes information
notes_actions_BA <- read_csv("gardens/deriveddata/BA2022_harvest_notes_actions.csv")
notes_actions_BA %>% 
  filter(action == "flag") %>% 
  select(notes, standard_note) -> notes_actions_keep_BA

# Merge together with the rest of the data
merge(data_allBA, notes_actions_keep_BA, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, NA),
         all_unripe = ifelse(standard_note == "allunripe", 1, NA),
         herbivory = ifelse(standard_note == "herbivory", 1, NA),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, NA),
         seed_drop = ifelse(standard_note == "seeddrop", 1, NA),
         smut = ifelse(standard_note == "smut", 1, NA),
         location_issue = ifelse(standard_note == "location_issue", 1, NA)) -> data_allBA

# Add additional seed drop information
data_allBA %>% 
  mutate(seed_drop = ifelse(drop_seed == "Y" | drop_seed == "y", 1, seed_drop)) -> data_allBA

## Format Cheyenne harvest data ####
harvest %>% 
  filter(site == "Cheyenne") %>% 
  mutate(id = paste(block, density, albedo, x, y, sep = "_")) -> harvestCH

# Create data subset 1: plants that did not survive to harvest
harvestCH %>% 
  filter(is.na(live) & is.na(seed_count_sub) & is.na(biomass_whole)) %>% 
  mutate(seed_count_total = NA) -> calib_data_nosurviveCH

# Create data subset 2: plants that survived but didn't make seeds
harvestCH %>% 
  filter(live == "Y" & (biomass_whole)>0 & inflor_mass == 0 & seed_count_sub == 0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseedsCH

# Create data subset 3: harvested seeds that were subsetted
harvestCH %>% 
  filter(inflor_mass > 0 & biomass_sub > 0 & seed_mass_sub > 0) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 1 & seed_count_sub >= 50) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%
  # There are 3 reps that have 0 seeds with 0 weight
  mutate(seed_count_total = ifelse(seed_count_total == "NaN", 0, seed_count_total)) %>% 
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsettedCH

# Create data subset 4: all seeds were counted (<50)
harvestCH %>% 
  filter(inflor_mass > 0 & seed_mass_sub > 0) %>% 
  filter(seed_count_sub < 50 & inflor_mass > 0) %>% 
  mutate(seed_count_total = seed_count_sub) -> calib_data_notsubsetCH

# Create data subset 5: No seeds but positive inflorescence mass
harvestCH %>% 
  filter(seed_count_sub == 0 & seed_mass_sub == 0 & inflor_mass > 0) %>% 
  mutate(seed_count_total = 0)-> calib_data_inflornoseedsCH

# Bind data subsets 1-5 back together
rbind(calib_data_subsettedCH, calib_data_noseedsCH, calib_data_nosurviveCH,
      calib_data_notsubsetCH, calib_data_inflornoseedsCH) %>% 
  arrange(id) -> data_mostCH

# Create %notin% operator
`%notin%` <- Negate(`%in%`)

# Fix observations by hand that aren't being pulled above
harvestCH %>% 
  filter(id %notin% data_mostCH$id) %>% 
  filter(seed_count_sub == 0 | seed_count_sub == 3 | is.na(seed_count_sub) | seed_count_sub == 24) %>% 
  mutate(seed_count_total = case_when(seed_count_sub == 0 ~ 0,
                                      seed_count_sub == 3 ~ 0,
                                      seed_count_sub == 24 ~ 24,
                                      is.na(seed_count_sub) ~ 0)) -> add_extra_cases

# Fix last set of observations that have close to 1 ratio
harvestCH %>% 
  filter(id %notin% data_mostCH$id) %>% 
  filter(seed_count_sub == 50) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  # Some plants have more inflor_mass_sub than inflor_mass which doesn't make
  # sense
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0 & ratio > 0.95) %>% 
  mutate(seed_count_total = seed_count_sub) %>% 
  select(-positive, -inflor_mass_sub, -ratio) -> add_extra_cases2

rbind(data_mostCH, add_extra_cases, add_extra_cases2) -> data_mostCH
# Still missing two plants, but I think this is as close as I can get!

# Read in notes information
notes_actions_CH <- read_csv("gardens/deriveddata/CH2022_harvest_notes_actions.csv")
notes_actions_CH %>% 
  filter(action == "flag") %>% 
  select(notes, standard_note) -> notes_actions_keep_CH

# Merge together with the rest of the data
merge(data_mostCH, notes_actions_keep_CH, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, NA),
         all_unripe = ifelse(standard_note == "allunripe", 1, NA),
         herbivory = ifelse(standard_note == "herbivory", 1, NA),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, NA),
         seed_drop = ifelse(standard_note == "seeddrop", 1, NA),
         smut = ifelse(standard_note == "smut", 1, NA),
         location_issue = ifelse(standard_note == "location_issue", 1, NA)) -> data_mostCH

# Add additional seed drop information
data_mostCH %>% 
  mutate(seed_drop = ifelse(drop_seed == "Y" | drop_seed == "y", 1, seed_drop)) -> data_mostCH


## Bring harvest data sets together ####
data_allSS %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, tillers, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allSS

data_allBA %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, tillers, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allBA

data_allWI %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, tillers, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allWI

data_mostCH %>% 
  select(site, date, block, plot, density, albedo, x, y, genotype,
         source, live, v, biomass_whole, seed_count_total,
         inflor_mass, tillers, standard_note, all_seed_drop, herbivory,
         physical_damage, seed_drop, smut) -> data_mostCH

harvest <- rbind(data_allSS, data_allBA, data_allWI, data_mostCH)

# Remove all data sets but phenology summarize and harvest
rm(list=setdiff(ls(), c("flower_phen", "harvest", "cg_2023")))

## Bring together phenology and harvest data ####

# Fix and merge Sheep Station
flower_phen %>% 
  filter(site == "SS") %>% 
  group_by(block, plot, gravel, density) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(old_plot = 1:40) %>% 
  select(-n) -> ss_plots

harvest %>% 
  filter(site == "SheepStation") %>% 
  select(old_plot = plot, density, gravel = albedo,
         x, y, seed_count_total, live, v, inflor_mass, biomass_whole, tillers, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) -> ss_harvest_short

merge(ss_plots, ss_harvest_short) %>% 
  select(-old_plot) -> ss_harvest_to_merge

flower_phen %>% 
  filter(site == "SS") -> ss_phen_short

merge(ss_harvest_to_merge, ss_phen_short) -> ss_phen_harvest

# Fix and merge WI
harvest %>% 
  filter(site == "BoiseLow") %>% 
  mutate(block = round(plot),
         plot = round(plot%%1 * 10)) %>% 
  select(block, plot, density, gravel = albedo,
         x, y, seed_count_total, live, v, inflor_mass, biomass_whole, tillers, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> wi_harvest_short

flower_phen %>% 
  filter(site == "WI") %>% 
  mutate(block = as.factor(block)) %>%
  # Fix error in density treatment designation for Block 2
  filter(block == 2) %>% 
  mutate(density = case_when(density == "hi" ~ "lo",
                          density == "lo" ~ "hi")) %>% 
  rbind(flower_phen %>% filter(block != 2 & site == "WI")) -> wi_phen_short

merge(wi_harvest_short, wi_phen_short)-> wi_phen_harvest

# Fix and merge Boise High (BA)
harvest %>% 
  filter(site == "BoiseHigh") %>% 
  mutate(block = round(plot),
         plot = round(plot%%1 * 10)) %>% 
  select(block, plot, density, gravel = albedo,
         x, y, seed_count_total, live, v, inflor_mass, biomass_whole, tillers, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> BA_harvest_short

flower_phen %>% 
  filter(site == "BA") %>% 
  mutate(block = as.factor(block)) -> BA_phen_short

merge(BA_harvest_short, BA_phen_short) -> ba_phen_harvest

# Fix and merge Cheyenne
harvest %>% 
  filter(site == "Cheyenne") %>% 
  select(block, density, gravel = albedo,
         x, y, seed_count_total, live, v, inflor_mass, biomass_whole, tillers, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> CH_harvest_short

flower_phen %>% 
  filter(site == "CH") %>% 
  mutate(block = as.factor(block)) -> CH_phen_short

merge(CH_harvest_short, CH_phen_short) -> ch_phen_harvest

# Bring together all sites one last time
rbind(ch_phen_harvest, ss_phen_harvest, wi_phen_harvest, ba_phen_harvest) -> full_data

## Rename and order columns to match 2023 growing season specifications ####
full_data %>% 
  mutate(note_standard_harvest = case_when(all_seed_drop == 1 ~ "seed_drop",
                                           herbivory == 1 ~ "herbivory",
                                           physical_damage == 1 ~ "physical_damage",
                                           seed_drop == 1 ~ "seed_drop",
                                           smut == 1 ~ "smut")) -> full_data

full_data %>% 
  mutate(year = 2022, growout = NA) %>% 
  select(plantID, site, year, density, albedo = gravel, block, plot, x, y,
         genotype, growout, first_flower, v_phen, last_phen_status, note_standard_phen = note_standard,
         live_harvest = live, v_harvest = v, tillers, biomass_whole, inflor_mass,
         seed_count_total, note_standard_harvest) %>% 
  mutate(tillers = ifelse(is.na(tillers), 0, tillers),
         seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total),
         inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass),
         biomass_whole = ifelse(is.na(biomass_whole), 0, biomass_whole)) -> cg_2022

# Remove any duplicates
cg_2022 %>% 
  distinct() -> cg_2022

# Remove intermediary datasets
rm(list=setdiff(ls(), c("cg_2022", "cg_2023")))
