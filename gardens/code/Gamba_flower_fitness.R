# Compares flowering time vs fitness for Sheep Station and Wildcat in 2021-2022
# and 2022-2023

## Preliminaries ####
# Load libraries
library(tidyverse)

# Read in data
cg_full <- read_csv("gardens/deriveddata/cg_fullData_withFlags.csv")

## Data cleaning ####
# Drop out observations for flags of concern (i.e. that could affect survival or
# fitness)
cg_full %>%
  filter(!grepl("smut", note_standard_phen) &
           !grepl("smut", note_standard_harvest) &
           !grepl("seed_drop", note_standard_phen) &
           !grepl("seed_drop", note_standard_harvest) &
           !grepl("wrong_spp", note_standard_phen) &
           !grepl("wrong_spp", note_standard_harvest) &
           !grepl("missing", note_standard_harvest)) -> cg_clean
# What percent of observations are we losing?
1 - nrow(cg_clean) / nrow(cg_full)
  
# Get true positives (alive at last phenology check before harvest and
# successfully harvested) and true negatives (not alive at last phenology check
# before harvest and not harvested)
cg_clean %>% 
  filter(last_phen_status == "Y" & (inflor_mass + veg_mass) > 0) -> cg_clean_pos
cg_clean %>% 
  filter(last_phen_status == "N" & (inflor_mass + veg_mass) == 0) -> cg_clean_neg
# See how many additional observations this drops out
(nrow(cg_clean_pos) + nrow(cg_clean_neg)) / nrow(cg_clean)
# Only takes out an additional 5%
rbind(cg_clean_pos, cg_clean_neg) -> cg_model
# This is a very conservative cleaning of the data

# Remove all intermediate datasets
rm(list=setdiff(ls(), "cg_model"))

## Data manipulation ####

# Get average flowering time per genotype. These will fill in observations with
# no observed flowering time to be the average for that genotype x site x year
# combination
cg_model %>% 
  # Only interested in Sheep Station and Wildcat
  filter(site %in% c("SS", "WI")) %>% 
  filter(complete.cases(first_flower)) %>% 
  group_by(genotype, site, year) %>% 
  summarize(mean_ft = mean(first_flower)) %>% 
  ungroup() -> gt_means

# Merge together with cg data
merge(cg_model, gt_means) -> cg_model

# Set first flowering date to be average if did not flower
cg_model %>% 
  mutate(first_flower = ifelse(is.na(first_flower),
                               mean_ft, first_flower)) -> cg_model

## Flowering x fitness analysis for 2022 (seed count) ####
# Average across genotype x site combination
cg_model %>% 
  filter(year == 2022) %>% 
  group_by(genotype, site) %>% 
  summarize(mean_ft = mean(first_flower),
            mean_seed_count = mean(seed_count_total)) -> genotype_site_2022 

# Make plot of site * flowering time interaction
genotype_site_2022 %>%  
  ggplot(aes(x = mean_ft, y = mean_seed_count, color = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Mean seed count",
       x = "Mean first day of flowering",
       color = "Site") +
  theme_bw(base_size = 14) 

# Fit linear model on genotype averages
fitness_mod_2022 <- lm(mean_seed_count ~ mean_ft * site, data = genotype_site_2022)
summary(fitness_mod_2022) # Significant interaction

## Flowering x fitness analysis for 2023 (inflorescence mass) ####
# Average across genotype x site combination
cg_model %>% 
  filter(year == 2023) %>% 
  group_by(genotype, site) %>% 
  summarize(mean_ft = mean(first_flower),
            mean_inflor_mass = mean(inflor_mass)) -> genotype_site_2023 

# Make plot of site * flowering time interaction
genotype_site_2023 %>%  
  ggplot(aes(x = mean_ft, y = mean_inflor_mass, color = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Mean inflorescence mass (g)",
       x = "Mean first day of flowering",
       color = "Site") +
  theme_bw(base_size = 14) 

# Fit linear model on genotype averages
fitness_mod_2023 <- lm(mean_inflor_mass ~ mean_ft * site, data = genotype_site_2023)
summary(fitness_mod_2023) # Significant interaction

