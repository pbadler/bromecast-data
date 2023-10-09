# Load libraries
library(tidyverse)

# Set 'not in' operator
`%notin%` <- Negate(`%in%`)

# Read in harvest data (last updated 10/2/2023)
harvest <- read_csv("gardens/rawdata/CG_harvest2022 - 10-9-23.csv")

# Make column names all lower case
names(harvest) <- tolower(names(harvest))

# Filter to be just sheep station and make unique ID
harvest %>% 
  filter(site == "SheepStation") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestSS

# Create data subset 1: plants that did not survive to harvest
harvestSS %>% 
  filter(live == "N" & is.na(seed_count_sub) & is.na(biomass_whole)) %>% 
  mutate(seed_count_total = NA) -> calib_data_nosurvive

# Create data subset 2: plants that survived but didn't make seeds
harvestSS %>% 
  filter(live == "Y" & (biomass_whole)>0 & inflor_mass ==0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds

# Create data subset 3: harvested seeds that were subsetted
harvestSS %>% 
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
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted

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
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> calib_data_notsubset

# Bind data subsets 1-4 back together
rbind(calib_data_subsetted, calib_data_whole, calib_data_noseeds, calib_data_nosurvive, calib_data_notsubset) %>% 
  arrange(id) -> data_most

data_most %>% 
  ggplot(aes(x = seed_count_total)) +
  geom_histogram() +
  theme_bw(base_size = 16) +
  xlab("total seed count")

data_most %>% 
  ggplot(aes(x = seed_count_total, y = biomass_whole, color = albedo)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +
  theme_bw(base_size = 16) +
  xlab("total seed count") +
  ylab("total aboveground biomass (g)") +
  ggtitle("Sheep Station (n = 3004)") +
  scale_color_manual(values = c("black", "gray85"))

data_most %>% 
  ggplot(aes(x = seed_count_total, y = biomass_whole, color = density)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +
  theme_bw(base_size = 16) +
  xlab("total seed count") +
  ylab("total aboveground biomass (g)") +
  ggtitle("Sheep Station (n = 3004)")

# See which observations we are still missing
harvestSS %>% 
  filter(id %notin% data_most$id) %>% 
  filter(is.na(inflor_mass)) %>% 
  select(plot, density, albedo, x, y, biomass_whole, inflor_mass, seed_count_sub, biomass_sub, seed_mass_sub)

harvestSS %>% 
  filter(id %notin% data_most$id) %>% 
  filter(inflor_mass > 0) %>% 
  select(plot, density, albedo, x, y, biomass_whole, inflor_mass, seed_count_sub, biomass_sub, seed_mass_sub) -> out

# Read in notes information
notes_actions <- read_csv("gardens/deriveddata/SS2022_harvest_notes_actions.csv")
notes_actions %>% 
  filter(action == "action") %>% 
  select(notes, standard_note) -> notes_actions_keep

# Merge together with the rest of the data
merge(data_most, notes_actions_keep, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(notes == "allseeddrop", 1, 0),
         herbivory = ifelse(notes == "herbivory", 1, 0),
         mortality = ifelse(notes == "mortality", 1, 0),
         physical_damage = ifelse(notes == "physicaldamage", 1, 0),
         seed_drop = ifelse(notes == "seeddrop", 1, 0),
         smut = ifelse(notes == "smut", 1, 0),
         wrong_spp = ifelse(notes == "wrongspp", 1, 0)) -> data_most

library(lme4)

data_most$genotype <- factor(data_most$genotype)
data_most$plot <- factor(data_most$plot)
data_most %>% 
  filter(seed_count_total > 0) -> total_seeds

mod <- lmer(log(seed_count_total) ~ density*albedo + (1|genotype) + (1|plot), data = total_seeds)
performance::icc(mod)
