# Compares flowering time vs fitness for Sheep Station and Wildcat in 2022-2023 

# Load libraries
library(tidyverse)

# Read in data
ss_phen_harvest <- read_csv("~/Desktop/ss_phen_harvest_2023.csv")
wi_phen_harvest <- read_csv("~/Desktop/wi_phen_harvest_2023.csv")

# Create not in operator
`%notin%` <- Negate(`%in%`)

# Remove plants with bad notes
ss_phen_harvest %>% 
  filter(note_standard %notin% c("bad_position", "duplicate", "no_date",
                                 "physical_damage", "smut", "seed_drop", "wrong_spp")) %>% 
  # A lot of plants with smut
  filter(note_standard_harvest %notin% c("bad_position", "duplicate", "no_date",
                                         "physical_damage", "smut", "seed_drop", "wrong_spp")) -> ss_phen_harvest_clean

wi_phen_harvest %>% 
  filter(note_standard %notin% c("bad_position", "duplicate", "no_date",
                                 "physical_damage", "smut", "seed_drop", "wrong_spp")) %>% 
  filter(note_standard_harvest %notin% c("bad_position", "duplicate", "no_date",
                                         "physical_damage", "smut", "seed_drop", "wrong_spp")) -> wi_phen_harvest_clean

# Filter so we are only have plants with recorded harvest dates
wi_phen_harvest_clean %>% 
  filter(complete.cases(first_flower)) -> wi_phen_harvest_clean

ss_phen_harvest_clean %>% 
  filter(complete.cases(first_flower)) -> ss_phen_harvest_clean

# Get true positives and true negatives (cleanest data)
ss_phen_harvest_clean %>% 
  filter(first_flower > 0 & inflor_mass > 0) -> ss_true_positives
ss_phen_harvest_clean %>% 
  filter(first_flower == 0 & inflor_mass == 0) -> ss_true_negatives

wi_phen_harvest_clean %>% 
  filter(first_flower > 0 & inflor_mass > 0) -> wi_true_positives
wi_phen_harvest_clean %>% 
  filter(first_flower == 0 & inflor_mass == 0) -> wi_true_negatives

# Calculate how much data we are including here
(nrow(ss_true_positives) + nrow(ss_true_negatives)) / nrow(ss_phen_harvest) # ~91% Most data here are dropped due to smut
(nrow(wi_true_positives) + nrow(wi_true_negatives)) / nrow(wi_phen_harvest) # ~80%

# Bring together all true positive and true negative data sets 
clean_ph <- rbind(ss_true_positives %>% select(names(wi_true_positives)),
                  ss_true_negatives %>% select(names(wi_true_negatives)),
                  wi_true_positives, wi_true_negatives)

# Get average flowering time per genotype. These will fill in observations with
# no observed flowering time to be the average for that genotype.
clean_ph %>% 
  filter(first_flower > 0) %>% 
  group_by(genotype, site) %>% 
  summarize(mean_ft = mean(first_flower)) -> gt_means

merge(clean_ph, gt_means) -> merged_dat

merged_dat %>% 
  mutate(first_flower = ifelse(first_flower == 0, mean_ft, first_flower)) -> clean_ph

# Average across genotype by site
clean_ph %>% 
  group_by(genotype, site) %>% 
  summarize(mean_ft = mean(first_flower),
            mean_inflor_mass = mean(inflor_mass)) -> genotype_site 

# Make plot of site * flowering time interaction
genotype_site %>%  
  ggplot(aes(x = mean_ft, y = mean_inflor_mass, color = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Mean inflorescence mass (g)",
       x = "Mean first day of flowering",
       color = "Site") +
  theme_bw(base_size = 14)

# Fit linear model on genotype averages
fitness_mod <- lm(mean_inflor_mass ~ mean_ft * site, data = genotype_site)
summary(fitness_mod) # Very strong interaction
