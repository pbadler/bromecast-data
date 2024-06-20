# Load libraries
library(tidyverse); library(lubridate)

# Read in harvest data 
harvest <- read_csv("gardens/rawdata/CG_harvest2023.csv")

# Set all columns to be lowercase
names(harvest) <- tolower(names(harvest))

## Format SS harvest data ####
# Subset to just Sheep Station
harvest %>%
  filter(site == "SheepStation") %>% 
  mutate(site = "SS") -> harvest_ss

# Check x and y coordinates
table(harvest_ss$x)
table(harvest_ss$y)
# Makes sense

# Check block and plot ID
table(harvest_ss$plot) # Currently in cumulative plots
# Need to change these to be block and plot
names(harvest_ss)[which(names(harvest_ss)=="plot")] <- "cum_plot"
# Remove current block column
harvest_ss %>% select(-block) -> harvest_ss
tmp <- data.frame(block=sort(rep(1:10,4)),
                  plot=rep(1:4,10),
                  cum_plot=1:40)
harvest_ss <- merge(harvest_ss, tmp)
harvest_ss %>% select(-cum_plot) -> harvest_ss

# Format dates
harvest_ss %>% 
  mutate(date = ifelse(date == "ND", NA, date),
         date = mdy(date)) -> harvest_ss

# Create action list for notes
# compile notes
tmp <- harvest_ss$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
#write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"SS","2023","_harvest_notes.csv"),row.names=F)
# Edited by MLV on 24 April 2024
rm(tmp)

# Bring notes back in and include standardized notes only
notes_ss <- read_csv("gardens/deriveddata/SS2023_harvest_notes.csv")
notes_ss %>% 
  filter(action == action) %>% 
  merge(harvest_ss, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype, growout,
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FP_FG" ~ "FP",
                       v == "FG_FP" ~ "FP",
                       v == "FP_FG_BS" ~ "FP",
                       v == "FG_FP_BS" ~ "FP",
                       v == "FG_FB_FP" ~ "FB",
                       v == "BS_FG" ~ "FG",
                       v == "FP_BS" ~ "FP",
                       v == "BS_FG_FP" ~ "FP",
                       v == "FB_FP" ~ "FB",
                       v == "FG_BS" ~ "FG",
                       v == "FB_BS" ~ "FB",
                       v == "FP_FB" ~ "FB",
                       v == "FG_FB" ~ "FB",
                       v == "FG/FP" ~ "FP",
                       v == "FP/FG" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FB_FG" ~ "FB",
                       v == "FG_FG" ~ "FG",
                       T ~ v)) -> harvest_ss

table(harvest_ss$v)
table(harvest_ss$live)
# There are some observations where the plant is recorded as dead and a v stage
# is recorded (most of these are early v stages)
harvest_ss %>% 
  filter(complete.cases(v) & live == "N") %>% 
  pull(v) %>% table()
# This might show up when we try and match harvest and phenology data

## Matching harvest and phenology data for SS ####

# Read in plant IDs
plantID_ss <- read_csv("gardens/deriveddata/SS2023_plantID.csv")
# Read in phenology data
phen_ss <- read_csv("gardens/deriveddata/SS2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_ss <- read_csv("gardens/deriveddata/SS2023_notes.csv")

# Merge together plantIDs and phenology
merge(phen_ss, plantID_ss) %>% 
  # Merge together with notes
  merge(notes_phen_ss, all = T) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, growout, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> ss_clean_phenology

# Get last phenology check for each plant
ss_clean_phenology %>% 
  group_by(block, plot, x, y) %>% 
  slice_max(jday) -> ss_last_phen

# Merge phenology data with harvest data
harvest_ss %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, genotype,
         growout, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(ss_last_phen) -> phen_harvest_ss

# See how well phenology and harvest match up
phen_harvest_ss %>% 
  # If we recorded a weight, then it was definitely harvested
  mutate(harvest = ifelse(is.na(biomass_whole), "N", "Y")) -> phen_harvest_ss

# Check accuracy  
phen_harvest_ss %>% 
  group_by(live, harvest) %>% 
  summarize(n = n())

phen_harvest_ss %>% 
  filter(live == "Y" & harvest == "N") # Most of these are wrong species or early stages

phen_harvest_ss %>% 
  filter(live == "N" & harvest == "Y") # Just seems random

# write_csv(phen_harvest_ss, "~/Desktop/ss_phen_harvest_2023.csv")

## SS Fitness and flowering time ####
# Merge phenology and harvest data sets to figure out which plants flowered
harvest_ss %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, genotype,
         growout, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(ss_clean_phenology) -> merged_dat 
  
merged_dat %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology

# Figure out which ones flowered *after* last phenology check
`%notin%` <- Negate(`%in%`)

merged_dat %>% 
  filter(plantID %notin% plants_flowered_phenology$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> plants_flowered_harvest

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest
  
# Bring datasets back together
rbind(plants_flowered_phenology, plants_flowered_harvest) -> all_plants_flowered

# Get plants that did not flower
merged_dat %>% 
  filter(plantID %notin% all_plants_flowered$plantID) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> all_plants_no_flower

# Get average flowering time of genotypes
all_plants_flowered %>% 
  group_by(genotype) %>% 
  summarize(mean_flower = mean(jday)) %>% 
  ungroup() -> genotype_averages

# Assign plants that didn't flower the average flowering time for genotype
all_plants_no_flower %>% 
  # merge(genotype_averages) %>% 
  mutate(jday = 0) %>% 
  # And then assign fitness (inflor_mass) to be 0
  mutate(inflor_mass = ifelse(is.na(inflor_mass), 0, inflor_mass)) -> all_plants_no_flower

# Bring flowered and non-flowered plants back together
rbind(all_plants_flowered, all_plants_no_flower) -> flower_fit

flower_fit %>% 
  group_by(live, live_harvest) %>% 
  summarize(n = n())

flower_fit %>% 
  rename(first_flower = jday) -> flower_fit

write_csv(flower_fit, "~/Desktop/ss_phen_harvest_2023.csv")

# Write file to save in derived data sets
write_csv(flower_fit, "gardens/deriveddata/SS2023_flower_fit.csv")

# Group by genotype and get average fitness and flowering time
# flower_fit %>% 
#   group_by(genotype) %>% 
#   summarize(jday_avg = mean(jday),
#             fitness_avg = mean(inflor_mass)) %>% 
#   ggplot(aes(x = jday_avg, fitness_avg)) + 
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_bw(base_size = 16) + 
#   labs(y = "Mean inflorescence mass (g)",
#        x = "Mean first day of flowering")
# Same pattern as we see in 2022, which is good!

## Format Cheyenne harvest data ####
# Subset to just Wildcat
harvest %>%
  filter(site == "Cheyenne") %>% 
  mutate(site = "CH") -> harvest_ch

# Check x and y coordinates
table(harvest_ch$x)
table(harvest_ch$y)
# All good

# Manually add in plot numbers
harvest_ch %>% 
  group_by(albedo, density, block) %>% 
  summarize() %>% 
  ungroup() %>% 
  arrange(block) %>% 
  mutate(plot = rep(1:4, 10)) -> plot_info
merge(harvest_ch %>% select(-plot), plot_info) -> harvest_ch

# Check block and plot ID
table(harvest_ch$plot) # All good

# Format dates
harvest_ch %>% 
  mutate(date = ifelse(date == "UNKNOWN", NA, date),
         date = mdy(date)) -> harvest_ch

# Create action list for notes
# compile notes
tmp <- harvest_ch$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"CH","2023","_harvest_notes.csv"),row.names=F)
# Edited by MLV on 29 April 2024
rm(tmp)

# Bring notes back in and include standardized notes only
notes_ch <- read_csv("gardens/deriveddata/CH2023_harvest_notes.csv")
notes_ch %>% 
  filter(action == "action") %>% 
  merge(harvest_ch, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype, growout,
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FB_FG" ~ "FB",
                       v == "FP_FG" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FG_FB" ~ "FB",
                       v == "FB_FP" ~ "FB",
                       v == "FG_FP" ~ "FP",
                       v == "FP_FB" ~ "FB",
                       v == "FB_FG_FP" ~ "FB",
                       v == "FG_FB_FP" ~ "FB",
                       v == "FP_FG_FB" ~ "FB",
                       v == "FB_FP_FG" ~ "FB",
                       v == "FP_FB_FB" ~ "FB",
                       v == "FP_FB_FG" ~ "FB",
                       v == "FP_BS" ~ "FP",
                       T ~ v)) -> harvest_ch

table(harvest_ch$live)
table(harvest_ch$v)

# Set live plants to be live if a v stage was recorded
harvest_ch$live <- ifelse(harvest_ch$v %in% c("BS", "FG", "FP", "FB"), "Y", "N")

# Read in plant IDs
plantID_ch <- read_csv("gardens/deriveddata/CH2023_plantID.csv")
# Read in phenology data
phen_ch <- read_csv("gardens/deriveddata/CH2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_ch <- read_csv("gardens/deriveddata/CH2023_notes_actions.csv")

# Merge together plantIDs and phenology
merge(phen_ch, plantID_ch) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> ch_clean_phenology

# Get last phenology check for each plant
ch_clean_phenology %>% 
  filter(live %in% c("N", "Y")) %>% 
  group_by(block, plot, x, y) %>% 
  slice_max(jday) -> ch_last_phen

# Merge phenology data with harvest data
harvest_ch %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, live_harvest = live,
         v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>%  
  merge(ch_last_phen) -> phen_harvest_ch

# See how well phenology and harvest match up
phen_harvest_ch %>% 
  # If we recorded a weight, then it was definitely harvested
  mutate(harvest = ifelse(is.na(biomass_whole), "N", "Y")) -> phen_harvest_ch

# Check accuracy  
phen_harvest_ch %>% 
  group_by(live, harvest) %>% 
  summarize(n = n())

## Cheyenne fitness and flowering time ####
# Merge phenology and harvest data sets to figure out which plants flowered
harvest_ch %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(ch_clean_phenology) -> merged_dat 

merged_dat %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology

# Figure out which ones flowered *after* last phenology check
`%notin%` <- Negate(`%in%`)

merged_dat %>% 
  filter(plantID %notin% plants_flowered_phenology$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> plants_flowered_harvest

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest

# Bring datasets back together
rbind(plants_flowered_phenology, plants_flowered_harvest) -> all_plants_flowered

# Get plants that did not flower
merged_dat %>% 
  filter(plantID %notin% all_plants_flowered$plantID) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> all_plants_no_flower

# Get average flowering time of genotypes
all_plants_flowered %>% 
  group_by(genotype) %>% 
  summarize(mean_flower = mean(jday)) %>% 
  ungroup() -> genotype_averages

# Assign plants that didn't flower the average flowering time for genotype
all_plants_no_flower %>% 
  merge(genotype_averages) %>% 
  mutate(jday = mean_flower) %>% 
  # And then assign fitness (inflor_mass) to be 0
  mutate(inflor_mass = 0) -> all_plants_no_flower

# Bring flowered and non-flowered plants back together
rbind(all_plants_flowered, all_plants_no_flower) -> flower_fit


# Remove plants with bad notes
# flower_fit %>% 
#   filter(note_standard %notin% c("bad_position", "duplicate", "no_date",
#                                  "physical_damage", "smut", "seed_drop")) -> flower_fit_clean

# Filter so we are only have plants with recorded harvest dates
flower_fit %>% 
  filter(complete.cases(jday)) -> flower_fit

# Replace inflor_mass NA to be 0
flower_fit$inflor_mass <- ifelse(is.na(flower_fit$inflor_mass), 0, flower_fit$inflor_mass)

# Fix outlier point
flower_fit[flower_fit$inflor_mass > 100,"inflor_mass"] <- 4.72

# Write file to save in derived data sets
write_csv(flower_fit, "gardens/deriveddata/CH2023_flower_fit.csv")

# Group by genotype and get average fitness and flowering time
flower_fit %>% 
  group_by(genotype) %>% 
  summarize(jday_avg = mean(jday),
            fitness_avg = mean(inflor_mass)) %>% 
  ggplot(aes(x = jday_avg, fitness_avg)) + 
  geom_point() +
  # Maybe a quadratic fit here?
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  geom_smooth(method = "lm", color = "red") + 
  theme_bw(base_size = 16) + 
  labs(y = "Mean inflorescence mass (g)",
       x = "Mean first day of flowering")

## Format WI harvest data ####
# Subset to just Wildcat
harvest %>%
  filter(site == "BoiseLow") %>% 
  mutate(site = "WI") -> harvest_wi

# Check x and y coordinates
table(harvest_wi$x)
table(harvest_wi$y)
# Issues here

# Remove plants that do not conform
harvest_wi %>% 
  filter((density == "high" & x %in% 1:9 & y %in% 1:10) | (density == "low" & x %in% 1:18 & y %in% 1:5)) -> harvest_wi

# Check block and plot ID
table(harvest_wi$plot) # plot 5?
harvest_wi[harvest_wi$block == 4 & harvest_wi$density == "low" & harvest_wi$albedo == "white","plot"] <- 2
# Should be plot = 2

# Format dates
harvest_wi %>% 
  mutate(date = ifelse(date %in% c("NO DATE", "no date"), NA, date),
         date = mdy(date)) -> harvest_wi

# Fix some of the observations that are missing dates (checked by Boise crew)
harvest_wi %>% 
  mutate(date = case_when(is.na(date) & block == 2 & plot == 1 & biomass_whole > 0 ~ as.Date("2023-05-31"),
                          is.na(date) & block == 4 & plot == 3 & biomass_whole > 0 ~ as.Date("2023-05-31"),
                          T ~ date)) -> harvest_wi

# Create action list for notes
# compile notes
tmp <- harvest_wi$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),"Boise","2023","_harvest_notes.csv"),row.names=F)
# Edited by MLV on 25 April 2024
rm(tmp)

# Bring notes back in and include standardized notes only
notes_wi <- read_csv("gardens/deriveddata/Boise2023_harvest_notes.csv")
notes_wi %>% 
  filter(action == "action") %>% 
  merge(harvest_wi, all = T) %>% 
  # Select only the columns that we want
  select(site, date, block, plot, density, albedo, x, y, genotype, growout,
         live, v, harvest, tillers, biomass_whole, inflor_mass, note_standard) %>% 
  # Arrange by position
  arrange(block, plot, density, albedo, x, y) %>% 
  # Recode v stage (choose the most advanced stage if multiple v stages
  # recorded)
  mutate(v = case_when(v == "FB_FG" ~ "FB",
                       v == "FB_FG_FP" ~ "FB",
                       v == "FB_FP" ~ "FB",
                       v == "FG_BS" ~ "FG",
                       v == "FG_FB" ~ "FB",
                       v == "FG_FP" ~ "FP",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FP_FB" ~ "FB",
                       v == "FP_FG" ~ "FP",
                       v == "FP_FG_FB" ~ "FB",
                       v == "UNK" ~ NA,
                       T ~ v)) -> harvest_wi

table(harvest_wi$live)

# Set live plants to be live if a v stage was recorded
harvest_wi$live <- ifelse(harvest_wi$v %in% c("BS", "FG", "FP", "FB"), "Y", "N")

## Bring in phenology WI ####
# Read in plant IDs
plantID_wi <- read_csv("gardens/deriveddata/Boise2023_plantID.csv")
# Read in phenology data
phen_wi <- read_csv("gardens/deriveddata/Boise2023_growthphenology_by_plantID.csv")
# Read in phenology standardized notes
notes_phen_wi <- read_csv("gardens/deriveddata/Boise2023_notes_actions.csv")

# Merge together plantIDs and phenology
merge(phen_wi, plantID_wi) %>% 
  # Merge together with notes
  merge(notes_phen_wi, all = T) %>% 
  # Select only columns that we need
  select(plantID, site, year, block, plot, x, y, genotype, jday, live, v, herbivory, frost_heave, note_standard) %>% 
  # Rearrange by location
  arrange(block, plot, x, y) -> wi_clean_phenology

## WI Fitness and flowering time ####
# Merge phenology and harvest data sets to figure out which plants flowered
harvest_wi %>% 
  select(harvest_date = date, block, plot, density, albedo, x, y, live_harvest = live, v_harvest =  v, harvest, tillers, biomass_whole, inflor_mass,
         note_standard_harvest = note_standard) %>% 
  merge(wi_clean_phenology) -> merged_dat 

merged_dat %>% 
  filter(v %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_min(jday) -> plants_flowered_phenology

# Figure out which ones flowered *after* last phenology check
`%notin%` <- Negate(`%in%`)

merged_dat %>% 
  filter(plantID %notin% plants_flowered_phenology$plantID) %>% 
  filter(v_harvest %in% c("FG", "FP", "FB")) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> plants_flowered_harvest

# If first time flowered when harvested, adjust v stage and date
plants_flowered_harvest %>% 
  mutate(jday = yday(harvest_date),
         jday = ifelse(jday < 270, jday, jday-365),
         v = v_harvest) -> plants_flowered_harvest

# Bring datasets back together
rbind(plants_flowered_phenology, plants_flowered_harvest) -> all_plants_flowered

# Get plants that did not flower
merged_dat %>% 
  filter(plantID %notin% all_plants_flowered$plantID) %>% 
  group_by(plantID) %>% 
  slice_max(jday) -> all_plants_no_flower

# Get average flowering time of genotypes
all_plants_flowered %>% 
  group_by(genotype) %>% 
  summarize(mean_flower = mean(jday)) %>% 
  ungroup() -> genotype_averages

# Assign plants that didn't flower the average flowering time for genotype
all_plants_no_flower %>% 
  # merge(genotype_averages) %>% 
  mutate(jday = 0) %>% 
  # And then assign fitness (inflor_mass) to be 0
  mutate(inflor_mass = 0) -> all_plants_no_flower

# Bring flowered and non-flowered plants back together
rbind(all_plants_flowered, all_plants_no_flower) -> flower_fit

flower_fit %>% 
  group_by(live, live_harvest) %>% 
  summarize(n = n())

write_csv(flower_fit, "~/Desktop/wi_phen_harvest_2023.csv")

# Remove plants with bad notes
# flower_fit %>% 
#   filter(note_standard %notin% c("bad_position", "duplicate", "no_date",
#                                  "physical_damage", "smut", "seed_drop")) -> flower_fit_clean

# Filter so we are only have plants with recorded harvest dates
# flower_fit_clean %>% 
#   filter(complete.cases(jday)) -> flower_fit_clean

# Replace inflor_mass NA to be 0
flower_fit$inflor_mass <- ifelse(is.na(flower_fit$inflor_mass), 0, flower_fit$inflor_mass)

flower_fit %>% 
  group_by(live, live_harvest) %>% 
  summarize(n = n())

flower_fit %>% 
  rename(first_flower = jday) -> flower_fit

write_csv(flower_fit, "~/Desktop/wi_phen_harvest_2023.csv")

# Write csv of flower fitness data
write_csv(flower_fit_clean, "gardens/deriveddata/Boise2023_flower_fit.csv")

# Group by genotype and get average fitness and flowering time
flower_fit_clean %>% 
  group_by(genotype) %>% 
  summarize(jday_avg = mean(jday),
            fitness_avg = mean(inflor_mass)) %>% 
  ggplot(aes(x = jday_avg, fitness_avg)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 16) + 
  labs(y = "Mean inflorescence mass (g)",
       x = "Mean first day of flowering")
# Same pattern as we see in 2022, which is good!

