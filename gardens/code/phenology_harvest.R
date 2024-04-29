# This file reads in and compiles the phenology data
## Load libraries ####
library(tidyverse); library(mgcv); library(gratia); library(geomtextpath);
library(here); library(readr); library(brms); library(RcppCNPy); library(mbend)

#### PHENOLOGY ####
## Read in Sheep Station data ####

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


## Read in Boise data ####
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


## Read in Cheyenne data ####
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
phen <- rbind(phen_SS %>% dplyr::select(-tillers), phen_Boise, phen_CH)

# Check to see if we have the right number of plants
length(unique(phen$plantID)) # 16000
phen %>% 
  group_by(site, block) %>% 
  summarize(n = n_distinct(plantID)) %>% 
  print(n = Inf)
# Everything looks good!

# Figure out the last recorded "live" status of each plant
phen %>% 
  filter(v %in% c("FG", "FB", "FP", "FX")) %>% 
  group_by(plantID) %>%
  # Gets minimum day of flowering
  slice(which.min(jday)) %>% 
  select(site, gravel, density, x, y, block, plot, genotype, first_flower = jday, live, v) %>% 
  ungroup() -> flowered

`%notin%` <- Negate(`%in%`)

phen %>% 
  filter(plantID %notin% flowered$plantID) %>% 
  group_by(plantID) %>%
  # Gets minimum day of flowering
  slice(which.min(jday)) %>% 
  select(site, gravel, density, x, y, block, plot, genotype, first_flower = jday, live, v) %>% 
  ungroup() %>% 
  mutate(first_flower = 0) -> not_flowered

last_phen <- rbind(flowered, not_flowered)

#### HARVEST ####
# Read in harvest data (last updated 11/29/2023)
harvest <- read_csv("gardens/rawdata/CG_harvest2022 - 11-29-2023.csv")

# Make column names all lower case
names(harvest) <- tolower(names(harvest))

## Sheep Station (SS) ####

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

data_allSS %>% 
  filter(seed_count_total < 100 & inflor_mass > 1) %>% 
  select(id, seed_count_sub, notes)

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

## Boise Low (WI) ####
# Filter to be just sheep station and make unique ID
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

# Still missing 1 observations that have issues
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

## Boise High (BA) ####
# Filter to be just sheep station and make unique ID
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

## Cheyenne (CH) ####
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


## Bring data sets together ####
data_allSS %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allSS

data_allBA %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allBA

data_allWI %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                      source, live, v, biomass_whole, seed_count_total,
                      inflor_mass, standard_note, all_seed_drop, herbivory,
                      physical_damage, seed_drop, smut) -> data_allWI

data_mostCH %>% 
  select(site, date, block, plot, density, albedo, x, y, genotype,
         source, live, v, biomass_whole, seed_count_total,
         inflor_mass, standard_note, all_seed_drop, herbivory,
         physical_damage, seed_drop, smut) -> data_mostCH

harvest <- rbind(data_allSS, data_allBA, data_allWI, data_mostCH)



#### Bring together phenology and harvest data ####
harvestSS
phen_SS %>% 
  group_by(block, plot, gravel, density) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(old_plot = 1:40) %>% 
  select(-n) -> ss_plots

data_allSS %>% 
  select(old_plot = plot, density, gravel = albedo,
         x, y, seed_count_total, inflor_mass, biomass_whole, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) -> ss_harvest_short

merge(ss_plots, ss_harvest_short) %>% 
  select(-old_plot) -> ss_harvest_to_merge

last_phen %>% 
  filter(site == "SS") -> ss_phen_short

merge(ss_harvest_to_merge, ss_phen_short) -> ss_phen_harvest

ss_phen_harvest %>% 
  filter(live == "Y" & is.na(biomass_whole)) %>% 
  nrow()

data_allWI %>% 
  mutate(block = round(plot),
         plot = round(plot%%1 * 10)) %>% 
  select(block, plot, density, gravel = albedo,
         x, y, seed_count_total, inflor_mass, biomass_whole, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> wi_harvest_short

last_phen %>% 
  filter(site == "WI") %>% 
  mutate(block = as.factor(block)) %>% 
  filter(block == 2) %>% 
  mutate(density = case_when(density == "hi" ~ "lo",
                          density == "lo" ~ "hi")) %>% 
  rbind(last_phen %>% filter(block != 2 & site == "WI")) -> wi_phen_short

merge(wi_harvest_short, wi_phen_short)-> wi_phen_harvest

wi_phen_harvest %>% 
  filter(live == "N" & biomass_whole>0) %>% 
  nrow()

data_allBA %>% 
  mutate(block = round(plot),
         plot = round(plot%%1 * 10)) %>% 
  select(block, plot, density, gravel = albedo,
         x, y, seed_count_total, inflor_mass, biomass_whole, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> BA_harvest_short

last_phen %>% 
  filter(site == "BA") %>% 
  mutate(block = as.factor(block)) -> BA_phen_short

merge(BA_harvest_short, BA_phen_short) -> ba_phen_harvest

data_mostCH %>% 
  select(block, density, gravel = albedo,
         x, y, seed_count_total, inflor_mass, biomass_whole, standard_note, all_seed_drop,
         herbivory, physical_damage, seed_drop, smut) %>% 
  mutate(density = ifelse(density == "low", "lo", "hi")) %>% 
  mutate(block = as.factor(block))-> CH_harvest_short

last_phen %>% 
  filter(site == "CH") %>% 
  mutate(block = as.factor(block)) %>% 
  select(-plot) -> CH_phen_short

merge(CH_harvest_short, CH_phen_short) -> ch_phen_harvest

ch_phen_harvest %>% 
  filter(live == "N" & biomass_whole >0) %>% 
  nrow()

### Preliminary analysis with Sheep Station and Cheyenne ####

# Remove all datasets except the phen-harvests for all sites
rm(list=setdiff(ls(), c("ss_phen_harvest", "ch_phen_harvest",
                        "ba_phen_harvest", "wi_phen_harvest")))

# Join together datasets
ss_phen_harvest$site <- "SS"
ch_phen_harvest$site <- "CH"
ba_phen_harvest$site <- "BA"
wi_phen_harvest$site <- "WI"

phen_harvest <- rbind(ss_phen_harvest %>% select(-plot), ch_phen_harvest,
                      ba_phen_harvest %>% select(-plot),
                      wi_phen_harvest %>% select(-plot))

phen_harvest %>% 
  mutate(seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total)) -> phen_harvest

phen_harvest %>% 
  filter(is.na(herbivory) & is.na(seed_drop)) %>% 
  filter(first_flower > 0) %>% 
  ggplot(aes(x = first_flower, y = seed_count_total)) +
  geom_point(aes(fill = gravel, color = gravel), pch = 21, size = 3, alpha = 0.2) +
  facet_grid(density~site, scales = "free_y") +
  theme_bw(base_size = 16) +
  labs(x = "First flowering day",
       y = "Number of seeds",
       color = "Gravel",
       fill = "Gravel") +
  scale_fill_manual(values = c("black", "maroon")) +
  scale_color_manual(values = c("black", "gray"))

# Calculate the percentage of plants that produced seed for each treatment combo
phen_harvest %>% 
  mutate(seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total)) %>% 
  mutate(seed_produced = ifelse(seed_count_total > 0, 1, 0)) %>% 
  group_by(site, gravel, density) %>% 
  summarize(prop = sum(seed_produced) / n())

png("~/Desktop/fitness_trt.png", height = 5, width = 8.5, res = 300, units = "in")
phen_harvest %>% 
  mutate(seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total)) %>%
  mutate(treatment = paste(gravel, density, sep = "-")) %>% 
  ggplot(aes(x = treatment, y = seed_count_total)) +
  geom_jitter(aes(color = gravel), alpha = 0.2) +
  facet_wrap(~site) +
  theme_bw(base_size = 16) +
  labs(x = "Treatment",
       y = "Number of seeds") +
  scale_color_manual(values = c("black", "maroon")) +
  theme(legend.position = "none")
dev.off()

### Flowering time vs fitness analysis SS vs WI ####

# Only use plants that match up with phenology and harvest datasets (i.e., if
# phenology data, we have harvest and vice versa)
ss_phen_harvest %>% 
  filter(first_flower > 0 & seed_count_total > 0) -> ss_true_positives
ss_phen_harvest %>% 
  filter(first_flower == 0 & seed_count_total == 0) -> ss_true_negatives

wi_phen_harvest %>% 
  filter(is.na(seed_drop)) %>% 
  filter(first_flower > 0 & seed_count_total > 0) -> wi_true_positives
wi_phen_harvest %>% 
  # Make NAs from seed_count_total into 0s
  mutate(seed_count_total = if_else(is.na(seed_count_total), 0, seed_count_total)) %>% 
  filter(first_flower == 0 & seed_count_total == 0) -> wi_true_negatives

ba_phen_harvest %>% 
  filter(is.na(seed_drop)) %>% 
  filter(first_flower > 0 & seed_count_total > 0) -> ba_true_positives
ba_phen_harvest %>% 
  # Make NAs from seed_count_total into 0s
  mutate(seed_count_total = if_else(is.na(seed_count_total), 0, seed_count_total)) %>% 
  filter(first_flower == 0 & seed_count_total == 0) -> ba_true_negatives

ch_phen_harvest %>% 
  filter(is.na(seed_drop)) %>% 
  filter(first_flower > 0 & seed_count_total > 0) -> ch_true_positives
ch_phen_harvest %>% 
  # Make NAs from seed_count_total into 0s
  mutate(seed_count_total = if_else(is.na(seed_count_total), 0, seed_count_total)) %>% 
  filter(first_flower == 0 & seed_count_total == 0) -> ch_true_negatives

# Bring together all true positive and true negative data sets 
clean_ph <- rbind(ss_true_positives %>% select(names(ch_true_positives)),
                  ss_true_negatives %>% select(names(ch_true_positives)),
                  wi_true_positives %>% select(names(ch_true_positives)),
                  wi_true_negatives %>% select(names(ch_true_positives)),
                  ba_true_positives %>% select(names(ch_true_positives)),
                  ba_true_negatives %>% select(names(ch_true_positives)),
                  ch_true_positives, ch_true_negatives)

# Get average flowering time per genotype. These will fill in observations with
# no observed flowering time to be the average for that genotype.
clean_ph %>% 
  filter(first_flower > 0) %>% 
  group_by(genotype) %>% 
  summarize(mean_ft = mean(first_flower)) -> gt_means

merge(clean_ph, gt_means) %>% 
  mutate(first_flower = ifelse(first_flower == 0, mean_ft, first_flower)) -> clean_ph

# Average across genotype by site
clean_ph %>% 
  group_by(genotype, site) %>% 
  summarize(mean_ft = mean(first_flower),
            mean_seed_count = mean(seed_count_total)) -> genotype_site 

# Make plot of site * flowering time interaction
genotype_site %>%  
  mutate(site = factor(site, levels = c("SS", "CH", "WI", "BA"))) %>% 
  ggplot(aes(x = mean_ft, y = mean_seed_count, color = site)) +
  geom_point(alpha = 0.5) +
  stat_poly_eq(use_label(c("eq", "P"))) +
  geom_smooth(method = "lm") +
  labs(y = "Mean seed count",
       x = "Mean first day of flowering",
       color = "Site") +
  theme_bw(base_size = 14) +
  #facet_wrap(~site, scales = "free") +
  facet_wrap(~site, nrow = 1) +
  theme(legend.position = "none") +
  ggtitle("Flowering vs fitness 2022") + 
  scale_color_manual(values = c("#332288", "#AA4499", "#44AA99", "#6699CC")) -> flower_fit_22

# Fit linear model on genotype averages
fitness_mod <- lm(mean_seed_count ~ mean_ft * site, data = genotype_site)
summary(fitness_mod) # Very strong interaction


ss_phen_harvest %>% 
  select(names(ch_phen_harvest)) -> ss_phen_harvest
wi_phen_harvest %>% 
  select(names(ch_phen_harvest)) -> wi_phen_harvest
ba_phen_harvest %>% 
  select(names(ch_phen_harvest)) -> ba_phen_harvest

phen_harvest_all <- rbind(ss_phen_harvest, wi_phen_harvest, ba_phen_harvest, ch_phen_harvest)

write_csv(phen_harvest_all,"~/Desktop/phen_harvests.csv")

# Read in 2023 data and add to this data set for plotting
ss_phen_harvest_23 <- read_csv("gardens/deriveddata/SS2023_flower_fit.csv")
wi_phen_harvest_23 <- read_csv("gardens/deriveddata/Boise2023_flower_fit.csv")
ch_phen_harvest_23 <- read_csv("gardens/deriveddata/CH2023_flower_fit.csv")

# Join 2023 datasets together
library(ggpmisc)

rbind(ss_phen_harvest_23  %>% select(genotype, site, inflor_mass, jday),
      wi_phen_harvest_23 %>% select(genotype, site, inflor_mass, jday),
      ch_phen_harvest_23 %>% select(genotype, site, inflor_mass, jday)) %>% 
  group_by(site, genotype) %>% 
  summarize(mean_ft = mean(jday),
            mean_inflor_mass = mean(inflor_mass)) %>% 
  ungroup() %>% 
  mutate(site = factor(site, levels = c("SS", "CH", "WI", "BA"))) %>% 
  ggplot(aes(x = mean_ft, y = mean_inflor_mass, color = site)) +
  geom_point(alpha = 0.5) +
  stat_poly_eq(use_label(c("eq", "P"))) +
  geom_smooth(method = "lm") +
  labs(y = "Mean inflorescence mass",
       x = "Mean first day of flowering",
       color = "Site") +
  theme_bw(base_size = 14) +
  facet_wrap(~site) +
  theme(legend.position = "none") +
  ggtitle("Flowering vs fitness 2023") +
  scale_color_manual(values = c("#332288", "#AA4499", "#44AA99", "#6699CC")) -> flower_fit_23

png("~/Desktop/flower_fit.png", height = 7, width = 10, res = 300, units = "in")
flower_fit_22 / flower_fit_23
dev.off()

ch_phen_harvest_23 %>%
  group_by(genotype) %>% 
  summarize(mean = mean(inflor_mass),
            n = n()) %>% 
  filter(mean > 4)

