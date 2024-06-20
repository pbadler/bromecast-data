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
# Bring in phenology data from common garden
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

phen %>% 
  filter(v %in% c("FG", "FB", "FP", "FX")) %>% 
  group_by(plantID) %>%
  # Gets minimum day of flowering
  slice(which.min(jday)) %>% 
  select(site, gravel, density, x, y, block, plot, genotype, first_flower = jday, live, v) %>% 
  ungroup() -> flowered

flowered %>% 
  group_by(genotype) %>% 
  summarize(first_flower = mean(first_flower)) %>% 
  ungroup() %>% 
  # Add columns for fitness data in cg 
  mutate(inflor_mass = 1,
         veg_mass = 1,
         seed_count = 1)-> cg_gen
  
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

# Read in inflor_mass and veg_mass from harvest info
harvest <- read_csv("~/Desktop/phen_harvests.csv")
harvest %>% 
  group_by(genotype) %>% 
  summarize(inflor_mass = mean(inflor_mass, na.rm = T),
            veg_mass = mean(biomass_whole, na.rm = T),
            seed_count = mean(seed_count_total, na.rm = T)) %>% 
  ungroup()-> harvest_extras

# Calculate correlation for Gamba et al. paper
gamba_gen %>% 
  merge(cg_gen %>% select(genotype, first_flower), all = T) -> cg_gamba 

cg_gamba %>% 
  ggplot(aes(x = first_flower, y = days_to_flower)) + 
  geom_point() +
  labs(x = "First flower DOY\n(Common garden 2022)",
       y = "Days to flower\n(Gamba study)") 

cor(cg_gamba$days_to_flower, cg_gamba$first_flower,
    use = "complete.obs", method = "pearson")
cor(cg_gamba$days_to_flower, cg_gamba$first_flower,
    use = "complete.obs", method = "spearman")

# Merge together datasets
merge(davidson_gen, gamba_gen, all = T) %>% 
  merge(walker_gen, all = T) %>% 
  merge(cg_gen %>% select(genotype, first_flower), all = T) %>% 
  merge(harvest_extras, all = T)-> all_datasets

# Replace all NaN with NAs
all_datasets[all_datasets == "NaN"] <- NA

# Make version that has 1 if data and NA if no data for each genotype
all_datasets[,2:ncol(all_datasets)] -> all_datasets_nogen
all_datasets_nogen[!is.na(all_datasets_nogen)] <- 1
all_datasets_nogen[is.na(all_datasets_nogen)] <- 0
cbind(genotype = all_datasets[,1], all_datasets_nogen) -> to_catalog

# Make searchable catalog
to_catalog %>% 
  gather(key = trait, value = "measured?", -genotype) %>% 
  arrange(genotype) %>% 
  mutate(`measured?` = ifelse(`measured?`== 1, "yes", "no")) -> catalog_v2

catalog_v2 %>% 
  mutate(study = case_when(trait %in% colnames(davidson_gen) ~ "davidson",
                   trait %in% colnames(walker_gen) ~ "walker",
                   trait %in% colnames(gamba_gen) ~ "gamba",
                   trait %in% colnames(cg_gen) ~ "cg_2022")) -> catalog_v2

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

#write_csv(full_catalog, "traits/data/deriveddata/genotype_catalog.csv")

full_catalog %>% 
  filter(`measured?` == "yes") %>% 
  group_by(trait, study) %>% 
  summarize(n = n()) -> genotype_count

write_csv(genotype_count, "traits/data/deriveddata/genotypes_by_trait.csv")  
