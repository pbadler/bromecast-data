# Common garden data cleaning -- This code cleans and brings together phenology
# and harvest datasets for 2023

## Preliminaries ####
# Load libraries
library(tidyverse); library(lubridate)
# Source QA/QC functions
source("gardens/code/QAQC_functions.R")

## Bring in harvest data ####

# Read in 2023 harvest data 
harvest <- read_csv("gardens/rawdata/CG_harvest2023.csv")

# Set all columns to be lowercase
names(harvest) <- tolower(names(harvest))

## Format 2023 Sheep Station harvest and phenology data ####
# Subset to just Sheep Station
harvest %>%
  filter(site == "SheepStation") %>% 
  mutate(site = "SS") -> harvest_ss

# Check x and y coordinates
table(harvest_ss$x)
table(harvest_ss$y)
# Makes sense

# Check block and plot ID
table(harvest_ss$plot) # Currently in cumulative plots
# Need to change these to be block and plot
names(harvest_ss)[which(names(harvest_ss)=="plot")] <- "cum_plot"
# Remove current block column
harvest_ss %>% select(-block) -> harvest_ss
tmp <- data.frame(block=sort(rep(1:10,4)),
                  plot=rep(1:4,10),
                  cum_plot=1:40)
harvest_ss <- merge(harvest_ss, tmp)
harvest_ss %>% select(-cum_plot) -> harvest_ss

# Format dates
harvest_ss %>% 
  mutate(date = ifelse(date == "ND", NA, date),
         date = mdy(date)) -> harvest_ss

# Create action list for notes -- this has already been completed
# compile notes
# tmp <- harvest_ss$notes
# tmp[tmp==""] <- NA
# tmp <- tmp[!is.na(tmp)]
# tmp <- unique(tmp,MARGIN=2)
# tmp <- data.frame(notes=tmp,action=NA)
#write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"SS","2023","_harvest_notes.csv"),row.names=F)
# Edited by MLV on 24 April 2024
# rm(tmp)

# Bring notes back in and include standardized notes only
notes_ss <- read_csv("gardens/deriveddata/SS2023_harvest_notes.csv")
notes_ss %>% 
  filter(action == action) %>% 
  merge(harvest_ss, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype,
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FP_FG" ~ "FP",
                       v == "FG_FP" ~ "FP",
                       v == "FP_FG_BS" ~ "FP",
                       v == "FG_FP_BS" ~ "FP",
                       v == "FG_FB_FP" ~ "FB",
                       v == "BS_FG" ~ "FG",
                       v == "FP_BS" ~ "FP",
                       v == "BS_FG_FP" ~ "FP",
                       v == "FB_FP" ~ "FB",
                       v == "FG_BS" ~ "FG",
                       v == "FB_BS" ~ "FB",
                       v == "FP_FB" ~ "FB",
                       v == "FG_FB" ~ "FB",
                       v == "FG/FP" ~ "FP",
                       v == "FP/FG" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FB_FG" ~ "FB",
                       v == "FG_FG" ~ "FG",
                       T ~ v)) -> harvest_ss

table(harvest_ss$v)
table(harvest_ss$live)
# There are some observations where the plant is recorded as dead and a v stage
# is recorded (most of these are early v stages)
harvest_ss %>% 
  filter(complete.cases(v) & live == "N") %>% 
  pull(v) %>% table()
# This might show up when we try and match harvest and phenology data

# Matching harvest and phenology data for SS 

# Read in plant IDs
plantID_ss <- read_csv("gardens/deriveddata/SS2023_plantID.csv")
# Read in phenology data
phen_ss <- read_csv("gardens/deriveddata/SS2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_ss <- read_csv("gardens/deriveddata/SS2023_notes.csv")

# Merge together plantIDs and phenology
merge(phen_ss, plantID_ss) %>% 
  # Merge together with notes
  merge(notes_phen_ss, all = T) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> ss_clean_phenology

# Merge phenology and harvest data sets to figure out which plants flowered
harvest_ss %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, genotype,
          live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(ss_clean_phenology) -> merged_dat_ss 

# For plants that flowered, get first observed flowering date
merged_dat_ss %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology_ss

# Create "not in" operator
`%notin%` <- Negate(`%in%`)

# Figure out which ones flowered *after* last phenology check
merged_dat_ss %>% 
  filter(plantID %notin% plants_flowered_phenology_ss$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> plants_flowered_harvest_ss

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest_ss %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest_ss

# Bring datasets back together
rbind(plants_flowered_phenology_ss, plants_flowered_harvest_ss) -> all_plants_flowered_ss

# Get plants that did not flower
merged_dat_ss %>% 
  filter(plantID %notin% all_plants_flowered_ss$plantID) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> all_plants_no_flower_ss

# Assign plants that didn't flower NA for flowering day
all_plants_no_flower_ss %>% 
  mutate(jday = NA) %>% 
  # And then assign fitness (inflor_mass) to be 0 is it is currently NA
  mutate(inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass)) -> all_plants_no_flower_ss

# Bring together flowering and no flowering subsets to single dataset
rbind(all_plants_flowered_ss, all_plants_no_flower_ss) %>% 
  arrange(block, plot, x, y) -> phen_harvest_ss

# Figure out the status of the plant at the last phenology check
ss_clean_phenology %>%
  group_by(plantID) %>% 
  slice_max(jday) %>% 
  ungroup() %>% 
  select(plantID, last_phen_status = live) -> ss_last_phen

# Add last phenology status to data sheet
phen_harvest_ss %>% 
  merge(ss_last_phen) -> phen_harvest_ss

# Add emergence identifier as well
ss_clean_phenology %>% 
  filter(live == "Y") %>% 
  select(plantID, live) %>% 
  distinct() %>% pull(plantID) -> emerged_ids_ss

phen_harvest_ss %>% 
  mutate(emergence = ifelse(plantID %in% emerged_ids_ss, "Y", "N")) -> phen_harvest_ss

# Collect all notes from phenology dataset to make sure they are included
ss_clean_phenology %>% 
  select(plantID, note_standard) %>% 
  filter(complete.cases(note_standard)) %>% 
  merge(phen_harvest_ss %>% select(-note_standard), all.y = T) -> phen_harvest_ss

# Note any plants that resurrected (were dead at one observation, then alive)
plant_ids <- sort(unique(ss_clean_phenology$plantID))

ss_clean_phenology %>% 
  arrange(plantID, jday) -> ss_clean_phenology

phen_harvest_ss %>% 
  arrange(plantID) -> phen_harvest_ss

phen_harvest_ss$resurrection_date <- NA

for(i in 1:length(plant_ids)){
  plant_data <- ss_clean_phenology %>% filter(plantID == plant_ids[i]) 
  phen_harvest_ss$resurrection_date[i] <- flag_resurrection(plant_data)
}

# Add any herbivory, frost_heave, and resurrection notes to phenology level notes
phen_harvest_ss %>% 
  mutate(note_standard_phen = case_when(herbivory == "Y" ~ "herbivory",
                                   frost_heave == "Y" ~ "frost_heave",
                                   complete.cases(resurrection_date) ~ "resurrection",
                                   T ~ note_standard)) %>% 
  # Remove herbivory and frost_heave columns
  select(-herbivory, -frost_heave) %>% 
  # Organize all columns
        # Basics
  select(plantID, site, year, density, albedo, block, plot, x, y, genotype, 
         # Phenology
         emergence, first_flower = jday, v_phen = v, last_phen_status, note_standard_phen,
         # Harvest
         live_harvest, v_harvest, tillers, biomass_whole, inflor_mass, note_standard_harvest) -> to_merge_ss

# Make biomass_whole and tillers 0 if currently NA
to_merge_ss %>% 
  mutate(tillers = ifelse(is.na(tillers), 0, tillers),
         biomass_whole = ifelse(is.na(biomass_whole), 0, biomass_whole)) -> to_merge_ss

# Remove all intermediary datasets before moving on
rm(list=setdiff(ls(), c("harvest","to_merge_ss", "flag_resurrection", "cg_2022")))

## Format 2023 Cheyenne harvest and phenology data ####
# Subset to just Cheyenne
harvest %>%
  filter(site == "Cheyenne") %>% 
  mutate(site = "CH") -> harvest_ch

# Check x and y coordinates
table(harvest_ch$x)
table(harvest_ch$y)
# All good

# Manually add in plot numbers
harvest_ch %>% 
  group_by(albedo, density, block) %>% 
  summarize() %>% 
  ungroup() %>% 
  arrange(block) %>% 
  mutate(plot = rep(1:4, 10)) -> plot_info
merge(harvest_ch %>% select(-plot), plot_info) -> harvest_ch

# Check block and plot ID
table(harvest_ch$plot) # All good

# Format dates
harvest_ch %>% 
  mutate(date = ifelse(date == "UNKNOWN", NA, date),
         date = mdy(date)) -> harvest_ch

# Create action list for notes -- this is already complete
# compile notes
# tmp <- harvest_ch$notes
# tmp[tmp==""] <- NA
# tmp <- tmp[!is.na(tmp)]
# tmp <- unique(tmp,MARGIN=2)
# tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"CH","2023","_harvest_notes.csv"),row.names=F)
# Edited by MLV on 29 April 2024
# rm(tmp)

# Bring notes back in and include standardized notes only
notes_ch <- read_csv("gardens/deriveddata/CH2023_harvest_notes.csv")
notes_ch %>% 
  filter(action == "action") %>% 
  merge(harvest_ch, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype,
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FB_FG" ~ "FB",
                       v == "FP_FG" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FG_FB" ~ "FB",
                       v == "FB_FP" ~ "FB",
                       v == "FG_FP" ~ "FP",
                       v == "FP_FB" ~ "FB",
                       v == "FB_FG_FP" ~ "FB",
                       v == "FG_FB_FP" ~ "FB",
                       v == "FP_FG_FB" ~ "FB",
                       v == "FB_FP_FG" ~ "FB",
                       v == "FP_FB_FB" ~ "FB",
                       v == "FP_FB_FG" ~ "FB",
                       v == "FP_BS" ~ "FP",
                       T ~ v)) -> harvest_ch

table(harvest_ch$live)
table(harvest_ch$v)

# Set live plants to be live if a v stage was recorded
harvest_ch$live <- ifelse(harvest_ch$v %in% c("BS", "FG", "FP", "FB"), "Y", "N")

# Read in plant IDs
plantID_ch <- read_csv("gardens/deriveddata/CH2023_plantID.csv")
# Read in phenology data
phen_ch <- read_csv("gardens/deriveddata/CH2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_ch <- read_csv("gardens/deriveddata/CH2023_notes_actions.csv")

# Merge together plantIDs and phenology
merge(phen_ch, plantID_ch) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> ch_clean_phenology

# Merge phenology and harvest data sets to figure out which plants flowered
harvest_ch %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(ch_clean_phenology) -> merged_dat_ch 

# Get first flowering date for plants that flowered
merged_dat_ch %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology_ch

# Figure out which ones flowered *after* last phenology check
`%notin%` <- Negate(`%in%`)

merged_dat_ch %>% 
  filter(plantID %notin% plants_flowered_phenology_ch$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> plants_flowered_harvest_ch

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest_ch %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest_ch

# Bring datasets back together
rbind(plants_flowered_phenology_ch, plants_flowered_harvest_ch) -> all_plants_flowered_ch

# Get plants that did not flower
merged_dat_ch %>% 
  filter(plantID %notin% all_plants_flowered_ch$plantID) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> all_plants_no_flower_ch

# Assign plants that didn't flower NA for flowering time
all_plants_no_flower_ch %>% 
  mutate(jday = NA) %>% 
  # And then assign fitness (inflor_mass) to be 0
  mutate(inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass)) -> all_plants_no_flower_ch

# Bring together flowering and no flowering subsets to single dataset
rbind(all_plants_flowered_ch, all_plants_no_flower_ch) %>% 
  arrange(block, plot, x, y) -> phen_harvest_ch

# Figure out the status of the plant at the last phenology check
ch_clean_phenology %>%
  filter(complete.cases(live)) %>% 
  group_by(plantID) %>% 
  slice_max(jday) %>% 
  ungroup() %>% 
  select(plantID, last_phen_status = live)-> ch_last_phen

# Add last phenology status to data sheet
phen_harvest_ch %>% 
  merge(ch_last_phen) -> phen_harvest_ch

# Add emergence identifier as well
ch_clean_phenology %>% 
  filter(live == "Y") %>% 
  select(plantID, live) %>% 
  distinct() %>% pull(plantID) -> emerged_ids_ch

phen_harvest_ch %>% 
  mutate(emergence = ifelse(plantID %in% emerged_ids_ch, "Y", "N")) -> phen_harvest_ch

# Collect all notes from phenology dataset to make sure they are included
ch_clean_phenology %>% 
  select(plantID, note_standard) %>% 
  filter(complete.cases(note_standard)) %>% 
  distinct() %>% 
  group_by(plantID) %>% 
  mutate(note_standard = paste0(note_standard, collapse = "_")) %>% 
  ungroup() %>% 
  distinct() %>% 
  merge(phen_harvest_ch %>% select(-note_standard), all.y = T) -> phen_harvest_ch

# Note any plants that resurrected (were dead at one observation, then alive)
plant_ids_ch <- sort(unique(ch_clean_phenology$plantID))

ch_clean_phenology %>% 
  arrange(plantID, jday) -> ch_clean_phenology

phen_harvest_ch %>% 
  arrange(plantID) -> phen_harvest_ch

phen_harvest_ch$resurrection_date <- NA

for(i in 1:length(plant_ids_ch)){
  plant_data <- ch_clean_phenology %>% filter(plantID == plant_ids_ch[i]) 
  phen_harvest_ch$resurrection_date[i] <- flag_resurrection(plant_data)
}

# Add any herbivory, frost_heave, and resurrection notes to phenology level notes
phen_harvest_ch %>% 
  mutate(note_standard_phen = case_when(herbivory == "Y" ~ "herbivory",
                                        frost_heave == "Y" ~ "frost_heave",
                                        complete.cases(resurrection_date) ~ "resurrection",
                                        T ~ note_standard)) %>% 
  # Remove herbivory and frost_heave columns
  select(-herbivory, -frost_heave) %>% 
  # Organize all columns
  # Basics
  select(plantID, site, year, density, albedo, block, plot, x, y, genotype,
         # Phenology
         emergence, first_flower = jday, v_phen = v, last_phen_status, note_standard_phen,
         # Harvest
         live_harvest, v_harvest, tillers, biomass_whole, inflor_mass, note_standard_harvest) -> to_merge_ch

# Make biomass_whole and tillers 0 if currently NA
to_merge_ch %>% 
  mutate(tillers = ifelse(is.na(tillers), 0, tillers),
         biomass_whole = ifelse(is.na(biomass_whole), 0, biomass_whole)) -> to_merge_ch

# Remove all intermediary datasets before moving on
rm(list=setdiff(ls(), c("harvest","to_merge_ss", "to_merge_ch", "flag_resurrection", "cg_2022")))

## Format 2023 WI data ####
# Subset to just Wildcat
harvest %>%
  filter(site == "BoiseLow") %>% 
  mutate(site = "WI") -> harvest_wi

# Check x and y coordinates
table(harvest_wi$x)
table(harvest_wi$y)
# Issues here

# Remove plants that do not conform
harvest_wi %>% 
  filter((density == "high" & x %in% 1:9 & y %in% 1:10) | (density == "low" & x %in% 1:18 & y %in% 1:5)) -> harvest_wi

# Check block and plot ID
table(harvest_wi$plot) # plot 5?
harvest_wi[harvest_wi$block == 4 & harvest_wi$density == "low" & harvest_wi$albedo == "white","plot"] <- 2
# Should be plot = 2

# Format dates
harvest_wi %>% 
  mutate(date = ifelse(date %in% c("NO DATE", "no date"), NA, date),
         date = mdy(date)) -> harvest_wi

# Fix some of the observations that are missing dates (checked by Boise crew)
harvest_wi %>% 
  mutate(date = case_when(is.na(date) & block == 2 & plot == 1 & biomass_whole > 0 ~ as.Date("2023-05-31"),
                          is.na(date) & block == 4 & plot == 3 & biomass_whole > 0 ~ as.Date("2023-05-31"),
                          T ~ date)) -> harvest_wi

# Create action list for notes -- already complete
# compile notes
# tmp <- harvest_wi$notes
# tmp[tmp==""] <- NA
# tmp <- tmp[!is.na(tmp)]
# tmp <- unique(tmp,MARGIN=2)
# tmp <- data.frame(notes=tmp,action=NA)
# # write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"Boise","2023","_harvest_notes.csv"),row.names=F)
# # Edited by MLV on 25 April 2024
# rm(tmp)

# Bring notes back in and include standardized notes only
notes_wi <- read_csv("gardens/deriveddata/Boise2023_harvest_notes.csv")
notes_wi %>% 
  filter(action == "action") %>% 
  merge(harvest_wi, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype, 
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FB_FG" ~ "FB",
                       v == "FB_FG_FP" ~ "FB",
                       v == "FB_FP" ~ "FB",
                       v == "FG_BS" ~ "FG",
                       v == "FG_FB" ~ "FB",
                       v == "FG_FP" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FP_FB" ~ "FB",
                       v == "FP_FG" ~ "FP",
                       v == "FP_FG_FB" ~ "FB",
                       v == "UNK" ~ NA,
                       T ~ v)) -> harvest_wi

table(harvest_wi$live)

# Set live plants to be live if a v stage was recorded
harvest_wi$live <- ifelse(harvest_wi$v %in% c("BS", "FG", "FP", "FB"), "Y", "N")

## Bring in phenology WI ##
# Read in plant IDs
plantID_wi <- read_csv("gardens/deriveddata/Boise2023_plantID.csv")
# Read in phenology data
phen_wi <- read_csv("gardens/deriveddata/Boise2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_wi <- read_csv("gardens/deriveddata/Boise2023_notes_actions.csv")

# Merge together plantIDs and phenology
merge(phen_wi, plantID_wi) %>% 
  # Merge together with notes
  merge(notes_phen_wi, all = T) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> wi_clean_phenology

# Merge phenology and harvest data sets to figure out which plants flowered
harvest_wi %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(wi_clean_phenology) -> merged_dat_wi 

# Get first flowering date for plants that flowered
merged_dat_wi %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology_wi

# Figure out which ones flowered *after* last phenology check
`%notin%` <- Negate(`%in%`)

merged_dat_wi %>% 
  filter(plantID %notin% plants_flowered_phenology_wi$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_harvest_wi

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest_wi %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest_wi

# Bring datasets back together
rbind(plants_flowered_phenology_wi, plants_flowered_harvest_wi) -> all_plants_flowered_wi

# Get plants that did not flower
merged_dat_wi %>% 
  filter(plantID %notin% all_plants_flowered_wi$plantID) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> all_plants_no_flower_wi

# Assign plants that didn't flower NA for flowering time
all_plants_no_flower_wi %>%
  mutate(jday = NA) %>% 
  # And then assign fitness (inflor_mass) to be 0
  mutate(inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass)) -> all_plants_no_flower_wi

# Bring flowered and non-flowered plants back together
rbind(all_plants_flowered_wi, all_plants_no_flower_wi) -> phen_harvest_wi

# Figure out the status of the plant at the last phenology check
wi_clean_phenology %>%
  filter(complete.cases(live)) %>% 
  group_by(plantID) %>% 
  slice_max(jday) %>% 
  ungroup() %>% 
  select(plantID, last_phen_status = live)-> wi_last_phen

# Add last phenology status to data sheet
phen_harvest_wi %>% 
  merge(wi_last_phen) -> phen_harvest_wi

# Add emergence identifier as well
wi_clean_phenology %>% 
  filter(live == "Y") %>% 
  select(plantID, live) %>% 
  distinct() %>% pull(plantID) -> emerged_ids_wi

phen_harvest_wi %>% 
  mutate(emergence = ifelse(plantID %in% emerged_ids_wi, "Y", "N")) -> phen_harvest_wi

# Collect all notes from phenology dataset to make sure they are included
wi_clean_phenology %>% 
  select(plantID, note_standard) %>% 
  filter(complete.cases(note_standard)) %>% 
  distinct() %>% 
  group_by(plantID) %>% 
  mutate(note_standard = paste0(note_standard, collapse = "_")) %>% 
  ungroup() %>% 
  distinct() %>% 
  merge(phen_harvest_wi %>% select(-note_standard), all.y = T) -> phen_harvest_wi

# Note any plants that resurrected (were dead at one observation, then alive)
plant_ids_wi <- sort(unique(wi_clean_phenology$plantID))

wi_clean_phenology %>% 
  arrange(plantID, jday) -> wi_clean_phenology

phen_harvest_wi %>% 
  arrange(plantID) -> phen_harvest_wi

phen_harvest_wi$resurrection_date <- NA

for(i in 1:length(plant_ids_wi)){
  plant_data <- wi_clean_phenology %>% filter(plantID == plant_ids_wi[i]) 
  phen_harvest_wi$resurrection_date[i] <- flag_resurrection(plant_data)
}

# Add any herbivory, frost_heave, and resurrection notes to phenology level notes
phen_harvest_wi %>% 
  mutate(note_standard_phen = case_when(herbivory == "Y" ~ "herbivory",
                                        frost_heave == "Y" ~ "frost_heave",
                                        complete.cases(resurrection_date) ~ "resurrection",
                                        T ~ note_standard)) %>% 
  # Remove herbivory and frost_heave columns
  select(-herbivory, -frost_heave) %>% 
  # Organize all columns
  # Basics
  select(plantID, site, year, density, albedo, block, plot, x, y, genotype,
         # Phenology
         emergence, first_flower = jday, v_phen = v, last_phen_status, note_standard_phen,
         # Harvest
         live_harvest, v_harvest, tillers, biomass_whole, inflor_mass, note_standard_harvest) -> to_merge_wi

# Make biomass_whole and tillers 0 if currently NA
to_merge_wi %>% 
  mutate(tillers = ifelse(is.na(tillers), 0, tillers),
         biomass_whole = ifelse(is.na(biomass_whole), 0, biomass_whole),
         inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass)) -> to_merge_wi

# Remove duplicates and bad position for WI (these were issues that we could not
# resolve)
to_merge_wi %>% 
  filter(!grepl("bad_position", note_standard_harvest)) %>% 
  filter(!grepl("duplicate", note_standard_harvest)) -> to_merge_wi

# Remove all intermediary datasets before moving on
rm(list=setdiff(ls(), c("to_merge_ss", "to_merge_ch", "to_merge_wi", "cg_2022")))

## Bring all data sets together ####
# Bring all data sets together
rbind(to_merge_ss, to_merge_ch, to_merge_wi) -> cg_2023

# Remove any duplicates
cg_2023 %>% 
  distinct() -> cg_2023

# Rename biomass_whole to veg_biomass to make it more sensible
cg_2023 %>% 
  rename(veg_mass = biomass_whole) -> cg_2023

# Rename density levels
cg_2023 %>% 
  mutate(density = ifelse(density == "high", "hi", "lo")) -> cg_2023

# Fix inflor_mass for one observation that is an obvious typo MLV 1/29/2025
cg_2023 %>% 
  mutate(inflor_mass = ifelse(inflor_mass == 472, 4.72, inflor_mass)) -> cg_2023

# Remove three observations that are duplicated 1/29/2025
cg_2023 %>% 
  filter(plantID != "Boise2023_1633" | complete.cases(note_standard_phen)) %>% 
  filter(plantID != "Boise2023_1768" | v_phen == "FG") %>% 
  filter(plantID != "Boise2023_2538" | complete.cases(note_standard_phen)) -> cg_2023
  
# Remove intermediary datasets
rm(list=setdiff(ls(), c("cg_2023", "cg_2022")))
