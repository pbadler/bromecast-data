# This script helps us figure out if we really need to count all seeds or if we
# can use seed weight as a proxy for harvest year 2.

## Preliminaries ####
library(tidyverse); library(here); library(mgcv); library(gratia)

theme_set(theme_bw(base_size = 14))

# Read in harvest data
harv <- read_csv(here("gardens/rawdata/CG_harvest2022_4May23.csv"))
names(harv) <- tolower(names(harv))

# Filter out to just SS
harv %>% 
  filter(site == "SheepStation") -> harvss

## Make plots on correlations between seed count and weight ####

# Start with those that have all seeds counted (plants with 1-3 tillers)
harvss %>% 
  filter(complete.cases(seed_count_whole)) %>% 
  mutate(treatment = paste0(density, sep = "-", albedo)) %>% 
  ggplot(aes(x = seed_mass_whole, y = seed_count_whole, color = treatment)) +
  geom_point(alpha = 0.5) +
  labs(y = "total seed count",
       x = "total seed mass (g)") +
  geom_smooth(method = "lm", se = F) +
  ggtitle("seed count ~ seed mass for plants with 1-3 tillers")

# Then for those where we only have a subset
harvss %>% 
  filter(is.na(seed_count_whole)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub) %>% 
  # Take out some outliers  
  filter(inflor_mass_sub < 0.2 |seed_count_sub > 25) %>% 
  # Look only at ones that aren't 50s
  filter(seed_count_sub !=50) %>% 
  mutate(treatment = paste0(density, sep = "-", albedo)) %>% 
  ggplot(aes(x = inflor_mass_sub, y = seed_count_sub, color = treatment)) +
  geom_point(alpha = 0.5) +
  labs(y = "subset seed count",
       x = "subset inforescence mass (g)") +
  geom_smooth(method = "lm", se = F) +
  ggtitle("subset seed count ~ inflor. mass")

# Estimate total seed count given inflorescence masses
harvss %>% 
  filter(complete.cases(inflor_mass)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio < 1, 0, 1)) %>% 
  filter(positive == 1) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>% 
  mutate(treatment = paste0(density, sep = "-", albedo)) %>% 
  # Take out outlier
  filter(seed_count_total < 4500) %>% 
  ggplot(aes(x = inflor_mass, y = seed_count_total, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F)

# Create data frame of calibration data
harvss %>% 
  filter(complete.cases(inflor_mass)) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio < 1, 0, 1)) %>% 
  filter(positive == 1) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>% 
  mutate(treatment = paste0(density, sep = "-", albedo)) %>% 
  # Take out outlier
  filter(seed_count_total < 4500 & seed_count_total > 0) -> calib_data

# Get summary stats of calibration data
calib_data %>% 
  group_by(treatment) %>% 
  summarize(n = n())

# Fit log-log linear model
calib_model <- lm(log(seed_count_total) ~ treatment*log(inflor_mass),
                   data = calib_data)

# Check assumptions
plot(calib_model)

# Check accuracy
plot(exp(predict(calib_model)), calib_data$seed_count_total)

# See if there is a significant treatment slope
car::Anova(calib_model) # not significant but close

# Fit log-log model without treatment and see if there is a difference in models
calib_model_notrt <- lm(log(seed_count_total) ~ log(inflor_mass),
                        data = calib_data)

anova(calib_model, calib_model_notrt)
# Definitely should keep treatment in model

# Get individual equations for each treatment

# High density - black gravel
calib_model_hb <- lm(log(seed_count_total) ~ log(inflor_mass),
                     data = calib_data %>% filter(albedo == "black" & density == "high"))
summary(calib_model_hb)

# High density - white gravel
calib_model_hw <- lm(log(seed_count_total) ~ log(inflor_mass),
                     data = calib_data %>% filter(albedo == "white" & density == "high"))
summary(calib_model_hw)

# Low density - black gravel
calib_model_lb <- lm(log(seed_count_total) ~ log(inflor_mass),
                     data = calib_data %>% filter(albedo == "black" & density == "low"))
summary(calib_model_lb)

# Low density - white gravel
calib_model_lw <- lm(log(seed_count_total) ~ log(inflor_mass),
                     data = calib_data %>% filter(albedo == "white" & density == "low"))
summary(calib_model_lw)
