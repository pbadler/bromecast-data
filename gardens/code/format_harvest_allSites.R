# Load libraries
library(tidyverse)

# Set 'not in' operator
`%notin%` <- Negate(`%in%`)

# Read in harvest data (last updated 10/2/2023)
harvest <- read_csv("gardens/rawdata/CG_harvest2022 - 11-13-2023.csv")

# Make column names all lower case
names(harvest) <- tolower(names(harvest))

## Sheep Station (SS) ####

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

# Bind data subsets 1-5 back together
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
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, 0),
         herbivory = ifelse(standard_note == "herbivory", 1, 0),
         mortality = ifelse(standard_note == "mortality", 1, 0),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, 0),
         seed_drop = case_when(standard_note == "seeddrop" ~ 1,
                               drop_seed == "Y" ~ 1,
                               T ~ 0),
         smut = ifelse(standard_note == "smut", 1, 0),
         wrong_spp = ifelse(standard_note == "wrongspp", 1, 0)) -> data_most

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
  filter(live == "Y" & (biomass_whole)>0 & inflor_mass ==0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds_WI

# Create data subset 3: harvested seeds that were subsetted

# Fix inflor_mass for one observation
harvestWI[which(harvestWI$plot == 5.2 &
                  harvestWI$x == 16 &
                  harvestWI$y == 5),"inflor_mass"] <- 0.9

harvestWI %>% 
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
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_WI

# Create data subset 4
harvestWI %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0) %>% 
  mutate(diff = inflor_mass_sub - inflor_mass) %>% 
  # Most of these are just rounding errors
  filter(diff < 0.05) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> calib_data_notsubset_WI

# Bind data subsets 1-4 back together
rbind(calib_data_subsetted_WI, calib_data_noseeds_WI, calib_data_nosurvive_WI, calib_data_notsubset_WI) %>% 
  arrange(id) -> data_most_WI

# Still missing 23 observations that have issues
harvestWI %>% 
  filter(id %notin% data_most_WI$id) %>% 
  select(id, biomass_whole, inflor_mass, seed_count_sub, seed_mass_sub, biomass_sub) %>% 
  print(n = Inf) 

# Read in notes information
notes_actions_WI <- read_csv("gardens/deriveddata/WI2022_harvest_notes_actions.csv")
notes_actions_WI %>% 
  filter(action == "action") %>% 
  select(notes, standard_note) -> notes_actions_keep_WI

# Merge together with the rest of the data
merge(data_most_WI, notes_actions_keep_WI, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, 0),
         all_unripe = ifelse(standard_note == "allunripe", 1, 0),
         herbivory = ifelse(standard_note == "herbivory", 1, 0),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, 0),
         seed_drop = case_when(standard_note == "seeddrop" ~ 1,
                               drop_seed == "Y" ~ 1,
                               T ~ 0),
         smut = ifelse(standard_note == "smut", 1, 0),
         location_issue = ifelse(standard_note == "location_issue", 1, 0)) -> data_most_WI

## Boise High (BA) ####
# Filter to be just sheep station and make unique ID
harvest %>% 
  filter(site == "BoiseHigh") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestBA

# Create data subset 1: plants that did not survive to harvest
harvestBA %>% 
  filter(is.na(live) & is.na(seed_count_sub) & is.na(biomass_whole)) %>% 
  mutate(seed_count_total = NA) -> calib_data_nosurvive_BA

# Create data subset 2: plants that survived but didn't make seeds
harvestBA %>% 
  filter(live == "Y" & (biomass_whole)>0 & inflor_mass ==0) %>% 
  mutate(seed_count_total = 0)-> calib_data_noseeds_BA

# Create data subset 3: harvested seeds that were subsetted
harvestBA %>% 
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
  select(-positive, -inflor_mass_sub, -ratio) -> calib_data_subsetted_BA

# Create data subset 4
harvestBA %>% 
  filter(complete.cases(inflor_mass) & complete.cases(biomass_sub) & complete.cases(seed_mass_sub)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio >= 1, 1, 0)) %>% 
  filter(positive == 0) %>% 
  mutate(diff = inflor_mass_sub - inflor_mass) %>% 
  # Most of these are just rounding errors
  filter(diff < 0.05) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>% 
  select(-inflor_mass_sub, - positive, -ratio, -diff) -> calib_data_notsubset_BA

# Bind data subsets 1-4 back together
rbind(calib_data_subsetted_BA, calib_data_noseeds_BA, calib_data_nosurvive_BA, calib_data_notsubset_BA) %>% 
  arrange(id) -> data_most_BA

# Still missing 3 observations that have issues
harvestBA %>% 
  filter(id %notin% data_most_BA$id) %>% 
  select(id, biomass_whole, inflor_mass, seed_count_sub, seed_mass_sub, biomass_sub) %>% 
  print(n = Inf)

# Read in notes information
notes_actions_BA <- read_csv("gardens/deriveddata/BA2022_harvest_notes_actions.csv")
notes_actions_BA %>% 
  filter(action == "flag") %>% 
  select(notes, standard_note) -> notes_actions_keep_BA

# Merge together with the rest of the data
merge(data_most_BA, notes_actions_keep_BA, all.x = T) %>% 
  mutate(all_seed_drop = ifelse(standard_note == "allseeddrop", 1, 0),
         all_unripe = ifelse(standard_note == "allunripe", 1, 0),
         herbivory = ifelse(standard_note == "herbivory", 1, 0),
         physical_damage = ifelse(standard_note == "physicaldamage", 1, 0),
         seed_drop = case_when(standard_note == "seeddrop" ~ 1,
                               drop_seed == "Y" ~ 1,
                               T ~ 0),
         smut = ifelse(standard_note == "smut", 1, 0),
         location_issue = ifelse(standard_note == "location_issue", 1, 0)) -> data_most_BA


## Cheyenne (CH) ####
harvest %>% 
  # Right now we have complete data up to block 8
  filter(site == "Cheyenne" & block < 9) %>% 
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

data_mostCH %>% 
  distinct() -> data_mostCH

harvestCH %>% 
  filter(id %notin% data_mostCH$id) %>% 
  filter(seed_count_sub == 0 | seed_count_sub == 3 | is.na(seed_count_sub) | seed_count_sub == 24) %>% 
  mutate(seed_count_total = case_when(seed_count_sub == 0 ~ 0,
                   seed_count_sub == 3 ~ 0,
                   seed_count_sub == 24 ~ 24,
                   is.na(seed_count_sub) ~ 0)) -> add_extra_cases

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

## Bring data sets together ####
data_most %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                     source, live, v, biomass_whole, seed_count_total,
                     inflor_mass, standard_note, all_seed_drop, herbivory,
                     physical_damage, seed_drop, smut) -> data_most

data_most_BA %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                        source, live, v, biomass_whole, seed_count_total,
                        inflor_mass, standard_note, all_seed_drop, herbivory,
                        physical_damage, seed_drop, smut) -> data_most_BA

data_most_WI %>% select(site, date, block, plot, density, albedo, x, y, genotype,
                        source, live, v, biomass_whole, seed_count_total,
                        inflor_mass, standard_note, all_seed_drop, herbivory,
                        physical_damage, seed_drop, smut) -> data_most_WI

data_mostCH %>% 

data_all <- rbind(data_most, data_most_BA, data_most_WI)

# Subset out seed_drop, physical damage, and herbivory
data_all %>% 
  mutate(all_seed_drop = ifelse(is.na(all_seed_drop), 0, all_seed_drop),
         physical_damage = ifelse(is.na(physical_damage), 0, physical_damage),
         smut = ifelse(is.na(smut), 0, smut),
         herbivory = ifelse(is.na(herbivory), 0, herbivory)) -> data_all

data_all %>% 
  filter(seed_drop == 0 & all_seed_drop == 0 & herbivory == 0 & physical_damage == 0 & smut == 0) -> data_all_sub

data_all_sub %>% 
  ggplot(aes(x = seed_count_total, y = inflor_mass)) +
  geom_point()

data_all_sub %>% 
  filter(seed_count_total == 0 & inflor_mass > 0)

data_all_sub %>% 
  filter(seed_count_total > 0) -> data_all_sub_seeds

library(lme4)

data_all_sub_seeds %>% 
  mutate(site_plot = as.factor(paste(site, plot, sep = "_")),
         genotype = as.factor(genotype)) -> data_all_sub_seeds

data_all_sub %>% 
  mutate(site_plot = as.factor(paste(site, plot, sep = "_"))) -> data_all_sub

data_all_sub_model <- data_all_sub %>% filter(seed_count_total > 0 & inflor_mass > 0)
data_all_sub_model <- data_all_sub_model %>% mutate(genotype = as.factor(genotype))

## Comparison of analyses: seed count, inflor mass, total plant biomass

# Get summary stats
nrow(data_all_sub_model) 
# 4949
nrow(data_all %>% filter(seed_count_total > 0 & inflor_mass > 0))
# 5760
nrow(data_all)
# 11856

# Get counts by site × density treatment
data_all_sub_model %>% 
  group_by(site, density) %>% 
  summarize(n = n())

# Fit model to seed count data
seed_mod <- lmer(log(seed_count_total) ~ density*albedo*site +
                   (1 + albedo*density + site | genotype) +
                   (1|site_plot), data = data_all_sub_model)

# Fit model to inflorescence mass data
inflor_mass_mod <- lmer(log(inflor_mass) ~ density*albedo*site +
                          (1 + albedo*density + site | genotype) +
                          (1|site_plot), data = data_all_sub_model)

# Fit model to total plant biomass data
data_all_sub_model %>% 
  mutate(whole_biomass = inflor_mass + biomass_whole) -> data_all_sub_model

whole_mass_mod <- lmer(log(whole_biomass) ~ density*albedo*site +
                          (1 + albedo*density + site | genotype) +
                          (1|site_plot), data = data_all_sub_model)

# Compare the relative weight of random effects
seed_re <- as.data.frame(VarCorr(seed_mod))$sdcor[c(1:7,23)]
inflor_re <- as.data.frame(VarCorr(inflor_mass_mod))$sdcor[c(1:7,23)]
whole_re <- as.data.frame(VarCorr(whole_mass_mod))$sdcor[c(1:7,23)]

# These seem pretty similar
cbind(seed_re/sum(seed_re), inflor_re/sum(inflor_re), whole_re/sum(whole_re))

# Get predicted means by treatment for each model
seed_plot <- sjPlot::plot_model(seed_mod, type = "emm", terms = c("density", "albedo", "site")) + theme_bw()
inflor_plot <- sjPlot::plot_model(inflor_mass_mod, type = "emm", terms = c("density", "albedo", "site")) + theme_bw()
whole_plot <- sjPlot::plot_model(whole_mass_mod, type = "emm", terms = c("density", "albedo", "site")) + theme_bw()

seed_plot/inflor_plot/whole_plot

# Get predicted GxE interactions
seed_plot_RE <- sjPlot::plot_model(seed_mod, type = "pred", pred.type = "re", terms = c("site", "genotype")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = rep("black", 95)) + geom_line()

inflor_plot_RE <- sjPlot::plot_model(inflor_mass_mod, type = "pred", pred.type = "re", terms = c("site", "genotype")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = rep("black", 95)) + geom_line()

whole_plot_RE <- sjPlot::plot_model(whole_mass_mod, type = "pred", pred.type = "re", terms = c("site", "genotype")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = rep("black", 95)) + geom_line()

seed_plot_RE + inflor_plot_RE + whole_plot_RE

# Plot predicted means
plot(predict(seed_mod), predict(inflor_mass_mod), xlab = "seed model predictions", ylab = "inflor. mass model predictions")
summary(lm(predict(seed_mod) ~ predict(inflor_mass_mod)))
plot(predict(seed_mod), predict(whole_mass_mod), xlab = "seed model predictions", ylab = "inflor. mass model predictions")
summary(lm(predict(seed_mod) ~ predict(whole_mass_mod)))

# Plot raw data
plot(log(data_all_sub_model$seed_count_total), log(data_all_sub_model$inflor_mass),
     xlab = "log(seed count)", ylab = "log(inflor. mass)")
plot(log(data_all_sub_model$seed_count_total), log(data_all_sub_model$whole_biomass),
     xlab = "log(seed count)", ylab = "log(total mass)")
