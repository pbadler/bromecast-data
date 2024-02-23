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

# Compare phenology in field vs in growth chamber
gamba_ids %>% 
  filter(genotype %in% davidson$genotype) %>% 
  group_by(genotype) %>% 
  summarize(dtf = mean(days_to_flower, na.rm = T)) %>% 
  merge(davidson_gen %>% select(genotype, phen_score)) %>% 
  ggplot(aes(x = dtf, y = phen_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Days to flower\n (Gamba growth chamber)",
       y = "Phenology score\n (Davidson field )")


