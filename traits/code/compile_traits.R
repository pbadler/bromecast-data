# Code for bringing together different trait datasets
library(janitor); library(tidyverse)
theme_set(theme_bw(base_size = 16))

# Bring in Walker root trait data
walker <- read_csv("traits/data/rawdata/walker_greenhouse/data/Raw-Data-BRTE-Study.csv")
# Bring in Davidson physiology data
davidson <- read_csv("traits/data/rawdata/davidson_physiology/WC_BZ_2022_2023.csv")
# Bring in Gamba growth chamber data
gamba_biomass <- read_csv("traits/data/rawdata/gamba_growthchamber/cheatgrass_biomass_cg.csv")
gamba_phen <- read_csv("traits/data/rawdata/gamba_growthchamber/cheatgrass_Rpheno_cg.csv")
gamba_seed <- read_csv("traits/data/rawdata/gamba_growthchamber/cheatgrass_seed_cg.csv")
gamba_phen_veg <- read_csv("traits/data/rawdata/gamba_growthchamber/cheatgrass_Vpheno_cg.csv")

# Merge together Gamba datasets and clean up column names
merge(gamba_phen, gamba_biomass, all.y = T) %>% 
  merge(gamba_seed, all.y = T) %>% 
  merge(gamba_phen_veg, all.y = T) %>% 
  arrange(PopNum, tray) %>% 
  clean_names() -> gamba

# Gamba data uses population number which is not the same as genotype number.
# Read in csv that will help convert pop_num to genotype coding that the rest of
# the datasets use
pop_to_gen <- read_csv("traits/data/rawdata/gamba_growthchamber/BRTEcg_genotypesCode.csv")
pop_to_gen %>% 
  select(genotype, pop_num = PopNum) %>% 
  # Merge with gamba dataset
  merge(gamba, all.y = T) -> gamba_ids
# We are missing genotype numbers for pop_num 1 and 3: need to check with Diana
# on this

# Clean up column names
walker %>% 
  clean_names() -> walker
davidson %>% 
  clean_names() -> davidson

# Summarize traits at the genotype level
davidson %>% 
  group_by(genotype) %>% 
  summarize(across(phen_score:sla_m2_kg, \(x) mean(x, na.rm = T))) %>% 
  select(-phen_score_plus1) -> davidson_gen

gamba_ids %>% 
  group_by(genotype) %>% 
  summarize(across(days_to_flower:leaves_n, \(x) mean(x, na.rm = T))) -> gamba_gen

# Replace all "x" with NA first
walker[walker == "."] <- NA

walker %>% 
  as_tibble() %>% 
  mutate_if(is.character, as.numeric) %>% 
  group_by(genotype_id) %>% 
  summarize(across(seed_wt_g:root_length_cm_by_diam_class_mm_l_4_5, \(x) mean(x, na.rm = T))) %>% 
  rename(genotype = genotype_id) -> walker_gen

# Merge together datasets
merge(davidson_gen, gamba_gen, all = T) %>% 
  merge(walker_gen, all = T) -> small_datasets


# Replace all NaN with NAs
small_datasets[small_datasets == "NaN"] <- NA

# Make version that has 1 if data and NA if no data for each genotype
small_datasets[,2:ncol(small_datasets)] -> small_datasets_nogen
small_datasets_nogen[!is.na(small_datasets_nogen)] <- 1
small_datasets_nogen[is.na(small_datasets_nogen)] <- 0
cbind(genotype = small_datasets[,1], small_datasets_nogen) -> to_catalog

# Make searchable catalog
to_catalog %>% 
  gather(key = trait, value = "measured?", -genotype) %>% 
  arrange(genotype) %>% 
  mutate(`measured?` = ifelse(`measured?`== 1, "yes", "no")) -> catalog_v2

catalog_v2 %>% 
  mutate(study = case_when(trait %in% colnames(davidson_gen) ~ "davidson",
                   trait %in% colnames(walker_gen) ~ "walker",
                   trait %in% colnames(gamba_gen) ~ "gamba")) -> catalog_v2

# Read in genotype information (source, lat, lon)
genotype_codes <- read_csv("gardens/rawdata/sitecode2genotypenumber.csv")
genotype_gps <- read_csv("gardens/rawdata/SeedCollectionData_wClimAndLavinBackground_23Jan2020 - SeedCollectionData_wClimAndLavinBackground_23Jan2020.csv")

genotype_codes %>% 
  select(genotype = genotypeID, site_code = Site.code) %>% 
  mutate(genotype = parse_number(genotype)) -> genotype_codes

genotype_gps %>% 
  select(latitude = Latitude, longitude = Longitude,
         site_code = Site.code) -> genotype_gps

merge(genotype_codes, genotype_gps) -> genotype_info

merge(catalog_v2, genotype_info, all.x = T) %>% 
  # Remove duplicates
  distinct() %>% 
  select(genotype, site_code, latitude, longitude, study, trait, `measured?`) -> full_catalog

