# Exact phenology information from harvest data and join with rest of phenology
# data

library(tidyverse)
library(here)
library(lubridate)

# Read in all phenology data but harvest phenology
phen <- read_csv(here("gardens/deriveddata/Boise2022_growthphenology_by_plantID.csv"))
# Read in harvest data
harvest <- read_csv(here("gardens/rawdata/CG_harvest2022_4May23.csv"))
# Read in plant ID info
plantID <- read_csv(here("gardens/deriveddata/Boise2022_plantID.csv"))

# Reformat harvest columns to match phenology/plantID

# Make column names lowercase
names(harvest) <- tolower(names(harvest))

# Select the columns we need and reformat date to jday
harvest %>% 
  filter(site %in% c("BoiseLow", "BoiseHigh")) %>% 
  dplyr::select(site, date, cum_plot = plot, density, albedo, x, y, live, v,
         length_mm, tillers, biomass_whole, notes) %>% 
  # Fix some date formatting
  mutate(date = case_when(date == "6/29" ~ "6/29/22",
                          date == "7/11" ~ "7/11/22",
                          date == "5/18" ~ "5/18/22",
                          date == "5/24/0222" ~ "5/24/22",
                          date == "7/112/22" ~ "7/11/22",
                          T ~ date)) %>% 
  # Recode jday to have anything before Jan 1 be negative
  mutate(jday = ifelse(yday(mdy(date)) < 270, yday(mdy(date)), yday(mdy(date)) - 365)) %>% 
  # Create block and plot from cum_plot
  mutate(block = trunc(cum_plot),
         plot = round((cum_plot - block)*10),
         site = case_when(site == "BoiseLow" ~ "WI",
                          site == "BoiseHigh" ~ "BA")) -> harvest

# Merge with plantID
merge(harvest, plantID) %>% 
  # Add columns from phen that aren't in harvest
  mutate(herbivory = NA,
         frost_heave = NA,
         harvested = ifelse(biomass_whole > 0, "Y", NA)) %>% 
  # Select columns of interest
  dplyr::select(names(phen)) -> harvest_phen

# Figure out what possible phenology statuses there are
unique(harvest_phen$v)

# Rename harvest phenology into categories: grouping by latest stage if there
# are multiple stages. If DEAD is listed with other stages, then I put it in the
# last possible stage before DEAD.
harvest_phen %>% 
  mutate(v_new = case_when(v %in% c("FP_FB", "FB", "BS_FB", "FG_FB", "FB_FG_BS", "BS_FP_FB",
                                    "BS_FG_FB", "PB", "FB_FG", "FB_FP", "FB_BS") ~ "FB",
                           v %in% c("FG", "BS_FG", "FG_BS") ~ "FG",
                           v %in% NA ~ NA,
                           v == ">V3" ~ "V3+",
                           v %in% c("BS/dead", "BS", "D_BS") ~ "BS",
                           v %in% c("FP", "FG_FP", "FP_FG", "BS_FG_FP", "BS_FP", "FP_BS") ~ "FP",
                           T ~ v)) %>% 
  mutate(v = v_new) %>% 
  dplyr::select(-v_new) -> harvest_phen

# Get only the plantIDs where we have harvest level phenology data
unique(harvest_phen$jday) 
# Day 55? This probably isn't right


# Check on jday 55 harvest plants
# harvest_phen %>% filter(jday == 55)
# all_phen %>% filter(plantID == "WI2022_313")
# all_phen %>% filter(plantID == "WI2022_837")
# Ya these can't be right. Drop these plants for now

harvest_phen %>% 
  filter(complete.cases(jday) & jday != 55) -> harvest_phen_sub

# Bring together phenology data set and harvest phenology
rbind(phen, harvest_phen_sub) -> all_phen

# Subset for plantIDs that we have harvest level data for
all_phen %>% 
  filter(plantID %in% harvest_phen_sub$plantID) -> all_phen

# Write csv into derived dataset
write_csv(all_phen,here("gardens/deriveddata/Boise2022_growthphenology_with_harvest.csv"))

