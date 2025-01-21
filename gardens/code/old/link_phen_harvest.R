# Link Wildcat phenology and harvest datasets for Gamba et al. manuscript

# Load libraries
library(tidyverse); library(stringr); library(lme4); library(sjPlot)

# Read in datasets
phen_flower <- read_csv("~/Desktop/phen_flower.csv")
harvest <- read_csv("~/Desktop/data_harvest.csv")
plantids <- read_csv("gardens/deriveddata/Boise2022_plantID.csv")

harvest %>% 
  filter(seed_count_total > 0 & site == "BoiseLow")

# Set plot theme
theme_set(theme_bw(base_size = 16))

# Subset phenology data to just be WI
phen_flower %>% 
  filter(site == "WI") %>% 
  # Filter out any resurrections
  filter(is.na(resurrection_date)) %>% 
  select(-genotype) -> phen_WI

# Subset harvest data to just be WI
harvest %>% 
  filter(site == "BoiseLow") %>% 
  mutate(site = "WI") %>% 
  # Make block and plot designations match other files
  mutate(block = trunc(plot),
         plot = round((plot-block)*10)) %>% 
  # Filter out plants with physical damage or seed drop
  filter(is.na(physical_damage) & is.na(seed_drop)) %>% 
  select(site, block, plot, x, y, density,genotype,
         albedo, seed_count_total) -> harvest_WI

# Subset plant ID data to be just WI
plantids %>% 
  filter(site == "WI") %>% 
  select(plantID, site, block, plot, x, y) -> plantids_WI

# Merge plant ID and harvest data
merge(plantids_WI, harvest_WI) -> merge_id_harvest

# Get phenology data columns that we need (plantID and jday)
phen_WI %>% select(plantID, jday) -> phen_WI

# Merge phenology data with harvest data.
merge(phen_WI, merge_id_harvest) -> selection_data

# Filter to just low density and calculate mean flowering time by genotype. 
selection_data %>% 
  filter(density == "low" & jday > 0) %>% 
  group_by(genotype) %>% 
  summarize(jday = mean(jday, na.rm = T)) %>% 
  ungroup() -> gen_ft

# Set jday for plants that germinated but did not flower to be genotype means
selection_data %>% 
  filter(density == "low" & jday == 0) %>% 
  filter(is.na(seed_count_total) | seed_count_total == 0) %>% 
  select(-jday) -> selection_zeros

# Make jday the genotype average for plants that didn't flower merge back with
# rest of data
merge(selection_zeros, gen_ft) -> selection_zeros_jday
rbind(selection_zeros_jday, selection_data %>%
        filter(density == "low" & jday > 0)) %>% 
  mutate(seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total)) %>% 
  arrange(block, plot, x, y) -> selection_data_low

# Average across genotype and mean flowering time
selection_data_low %>% 
  group_by(genotype, albedo) %>% 
  summarize(mean_ft = mean(jday),
            mean_seeds = mean(seed_count_total)) -> selection_data_gen

# Plot seed count as a function of flowering time for each albedo treatment
selection_data_gen %>% 
  ggplot(aes(x = mean_ft, y = mean_seeds)) + 
  geom_point(aes(color = albedo, fill = albedo), shape = 21, size = 4) +
  geom_smooth(method = "lm", aes(color = albedo, linetype = albedo),
              fill = "gray") +
  labs(x = "mean first day of flowering", y = "mean seed count") +
  scale_color_manual(values = c("black", "gray47")) +
  scale_fill_manual(values = c("gray37", "white")) +
  scale_linetype_manual(values = c("solid", "dashed"))

# Run linear model
mod_gen <- lm(mean_seeds ~ mean_ft * albedo, data = selection_data_gen)
summary(mod_gen)
confint(mod_gen)
# Not significant interaction but steeper slope for black gravel

# Run linear mixed model at observation-level (i.e., not averaging at the
# genotype level)
mod_obs <- lmer(seed_count_total ~ jday * albedo + (1|genotype),
                data = selection_data_low)
summary(mod_obs)
confint(mod_obs)
# Stronger evidence of differences in slope

# Make plot of model marginal means
plot_model(mod_obs, type = "emm", terms = c("jday", "albedo")) +
  scale_color_manual(values = c("black", "gray")) + 
  scale_fill_manual(values = c("gray", "gray")) 
