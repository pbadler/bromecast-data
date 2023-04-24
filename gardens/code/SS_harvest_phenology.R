# Exact phenology information from harvest data and join with rest of phenology
# data

library(tidyverse)
library(here)

# Read in all phenology data but harvest phenology
phen <- read_csv(here("gardens/deriveddata/SS2022_growthphenology_by_plantID.csv"))
# Read in harvest data
harvest <- read_csv(here("gardens/rawdata/CG_harvest2022 4-17-23.csv"))
# Read in plant ID info
plantID <- read_csv(here("gardens/deriveddata/SS2022_plantID.csv"))

# Reformat harvest columns to match phenology/plantID

# Make column names lowercase
names(harvest) <- tolower(names(harvest))

# Select the columns we need and reformat date to jday
harvest %>% 
  filter(site == "SheepStation") %>% 
  select(site, date, cum_plot = plot, density, albedo, x, y, live, v,
         length_mm, tillers, biomass_whole, notes) %>% 
  # Fix some date formatting
  mutate(date = case_when(date == "6/29" ~ "6/29/22",
                          date == "7/11" ~ "7/11/22",
                          date == "5/18" ~ "5/18/22",
                          date == "5/24/0222" ~ "5/24/22",
                          date == "7/112/22" ~ "7/11/22",
                          T ~ date)) %>% 
  mutate(jday = ifelse(yday(mdy(date)) < 270, yday(mdy(date)), yday(mdy(date)) - 365)) %>% 
  # Create block and plot from cum_plot
  mutate(block = case_when(cum_plot %in% 1:4 ~ 1,
                           cum_plot %in% 5:8 ~ 2,
                           cum_plot %in% 9:12 ~ 3,
                           cum_plot %in% 13:16 ~ 4,
                           cum_plot %in% 17:20 ~ 5,
                           cum_plot %in% 21:24 ~ 6,
                           cum_plot %in% 25:28 ~ 7,
                           cum_plot %in% 29:32 ~ 8,
                           cum_plot %in% 33:36 ~ 9,
                           cum_plot %in% 37:40 ~ 10),
         plot = case_when(cum_plot %in% 1:4 ~ cum_plot,
                          cum_plot %in% 5:8 ~ cum_plot - 4,
                          cum_plot %in% 9:12 ~ cum_plot - 4*2,
                          cum_plot %in% 13:16 ~ cum_plot - 4*3,
                          cum_plot %in% 17:20 ~ cum_plot - 4*4,
                          cum_plot %in% 21:24 ~ cum_plot - 4*5,
                          cum_plot %in% 25:28 ~ cum_plot - 4*6,
                          cum_plot %in% 29:32 ~ cum_plot - 4*7,
                          cum_plot %in% 33:36 ~ cum_plot - 4*8,
                          cum_plot %in% 37:40 ~ cum_plot - 4*9),
         site = "SS") -> harvest
  
# Merge with plantID
merge(harvest, plantID) %>% 
  # Add columns from phen that aren't in harvest
  mutate(herbivory = NA,
         frost_heave = NA,
         harvested = ifelse(biomass_whole > 0, "Y", NA)) %>% 
  # Select columns of interest
  select(names(phen)) -> harvest_phen

# Figure out what possible phenology statuses there are
unique(harvest_phen$v)
