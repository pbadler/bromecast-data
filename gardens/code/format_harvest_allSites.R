# Load libraries
library(tidyverse)

# Set 'not in' operator
`%notin%` <- Negate(`%in%`)

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

data_all <- rbind(data_allSS, data_allBA, data_allWI, data_mostCH)

# Remove all other sub data sets 
rm(list=setdiff(ls(), "data_all"))

# We have some plants that are highlighted in the CG seeds document. These
# either mean the plant should have been there but wasn't (most), or that a
# plant was there at harvest that shouldn't be there (a few). Upon a cursory
# look, it's not clear why there were so many plants (n = 223) where this was a
# problem (no systematic patterns). 
data_problems <- read_csv("gardens/rawdata/CG_harvest2022_problems.csv")
names(data_problems) <- tolower(names(data_problems))
data_problems$id <- paste(data_problems$plot, data_problems$density,
                          data_problems$albedo, data_problems$x,
                          data_problems$y, sep = "_")
# Remove extra NA rows
data_problems <- data_problems[1:223,]

# Subset out seed_drop, physical damage, and herbivory
data_all %>% 
  mutate(all_seed_drop = ifelse(is.na(all_seed_drop),NA, all_seed_drop),
         physical_damage = ifelse(is.na(physical_damage), NA, physical_damage),
         smut = ifelse(is.na(smut), NA, smut),
         herbivory = ifelse(is.na(herbivory), NA, herbivory)) -> data_all


data_all %>% 
  filter(seed_count_total > 0 & inflor_mass > 0) -> data_all_sub_seeds

data_all_sub_seeds %>% 
  mutate(site_plot = as.factor(paste(site, plot, sep = "_")),
         genotype = as.factor(genotype)) -> data_all_sub_seeds

## Comparison of analyses: seed count, inflor mass, total plant biomass

# Get summary stats
nrow(data_all_sub_seeds) 
# 7050
nrow(data_all)
# 15998

# Get counts by site × density treatment
data_all_sub_seeds %>% 
  group_by(site, density) %>% 
  summarize(n = n())

# Fit model to seed count data
seed_mod <- lmer(log(seed_count_total) ~ density*albedo*site +
                   (1|genotype:site) + (1|genotype) +
                   (1|site_plot), data = data_all_sub_seeds)

seed_mod_pois <- glmer(seed_count_total ~ density*albedo*site +
                   (1|genotype:site)+ (1|genotype)+
                   (1|site_plot), data = data_all_sub_seeds,
                   family = "poisson")

anova(seed_mod_pois)

logLik(seed_mod)

sjPlot::plot_model(seed_mod_pois, type = "diag")


summary(seed_mod)

anova(seed_mod)

MuMIn::r.squaredGLMM(seed_mod)

seed_mod2 <- lmer(log(seed_count_total) ~ density*albedo*site +
                   (0+site|genotype) +
                   (1|site_plot), data = data_all_sub_seeds)
plot(seed_mod)

summary(seed)

sjPlot::plot_model(seed_mod_pois, type = "pred", pred.type = "re",
                   terms = c("site", "genotype")) +
  theme(legend.position = "none") +
  geom_line()


plot(exp(predict(seed_mod_pois)), exp(predict(seed_mod)),
     xlim = c(0,4000))
plot(exp(predict(seed_mod)), data_all_sub_seeds$seed_count_total,
     xlim = c(0,4000))

abline(a = 0, b = 1, col = "blue")

MuMIn::r.squaredGLMM(seed_mod_pois)

sjPlot::plot_model(seed_mod2, type = "pred", pred.type = "re",
                   terms = c("site", "genotype"), ci.lvl = NA) +
  theme(legend.position = "none") + scale_color_manual(values = rainbow(95)) +
  geom_line()

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
