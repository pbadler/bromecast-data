# Formatting Cheyenne phenology to be in same structure as Sheep Station

library(lubridate); library(tidyverse); library(here)

doyear <- 2022 # growing season to do
dosite <- "CH" # code for focal site

# Read in data
juneB <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_June22b.csv"))
juneA <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_June22a.csv"))
mayB <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_May22b.csv"))
mayA <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_May22a.csv"))
aprB <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_April22b.csv"))
aprA <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_April22a.csv"))
marB <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_March22b.csv"))
marA <- suppressMessages(read_csv("gardens/rawdata/WY_census_data_March22a.csv"))

# Create a list of data
all_phen <- list(juneB, juneA, mayB, mayA, aprB, aprA, marB, marA)

# Create vector of days (these are just placeholders for now)
dates <- c("juneB", "juneA", "mayB", "mayA", "aprB", "aprA", "marB", "marA")

for(i in 1:length(dates)){
  
  # Set temp data name
  rawD <- all_phen[[i]]
  
  # remove capital letters from column headers 
  names(rawD) <- tolower(names(rawD))
  
  # Look at data
  glimpse(rawD)
  
  # Fix issues with different locations of column headers -- treatments
  rawD %>% 
    filter(complete.cases('...2')) -> rawD
  
  rawD_trts <- rawD[2:nrow(rawD),1:4]
  names(rawD_trts) <- as.character(rawD[1,1:4])
  
  # Fix issues with different locations of column headers -- plants
  rawD_plants <- rawD[2:nrow(rawD),5:ncol(rawD)]
  names(rawD_plants) <- c("x", paste(rep(c("plant_length", "v", "notes"), 20), rep(1:20, each = 3), sep = "_"))
  
  # Bring two data sets back together
  newD <- cbind(rawD_trts, rawD_plants)
  
  # Bring plant length columns to rows
  newD %>% 
    gather(key = y, value = plant_length, contains("plant_length")) %>% 
    mutate(y = as.numeric(parse_number(y))) %>% 
    dplyr::select(date, albedo, density, block, x, y, plant_length) %>% 
    distinct() -> newD_plant_length
  
  newD %>% 
    gather(key = y, value = v, contains("v")) %>% 
    mutate(y = as.numeric(parse_number(y))) %>% 
    dplyr::select(date, albedo, density, block, x, y, v) %>% 
    distinct() -> newD_v
  
  newD %>% 
    gather(key = y, value = notes, contains("notes")) %>% 
    mutate(y = as.numeric(parse_number(y))) %>% 
    dplyr::select(date, albedo, density, block, x, y, notes) %>% 
    distinct() -> newD_notes
  
  cbind(newD_plant_length, v = newD_v$v, notes = newD_notes$notes)-> longD
  
  # Remove data for y = 11:20 for high density treatment
  longD %>% 
    filter(density == "high" & x %in% 1:10 & y %in% 1:10) -> longD_high
  
  # Remove data for x = 6:10 for low density treatment
  longD %>% 
    filter(density == "low" & x %in% 1:5 & y %in% 1:20) -> longD_low
  
  # Bring together total dataset
  deriveD <- rbind(longD_high, longD_low)
  deriveD$block <- as.numeric(deriveD$block)
  
  # Manually add in plot numbers
  deriveD %>% 
    group_by(albedo, density, block) %>% 
    summarize() %>% 
    ungroup() %>% 
    arrange(block) %>% 
    mutate(plot = rep(1:4, 10)) -> plot_info
  
  # Reorder and create unique identifier
  merge(deriveD, plot_info) %>% 
    arrange(block, plot, x, y) %>% 
    mutate(plantID = paste(dosite, doyear, 1:nrow(deriveD), sep = "_"),
           live = ifelse(complete.cases(v), "Y", "N"),
           jday = yday(mdy(date))) %>% 
    mutate(jday = ifelse(jday < 270, jday, jday-365)) %>% 
    dplyr::select(plantID, density, gravel = albedo, block, plot, x, y, jday, live, v, length_mm = plant_length, notes) -> phen_sample
  
  assign(paste0(dates[i], sep = "_", "phen"), phen_sample)
  
}

# Bring all data together
rbind(marA_phen, marB_phen, aprA_phen, aprB_phen, mayA_phen, mayB_phen, juneA_phen, juneB_phen) -> phen_WY

# Switch x and y so that x = 1:10 or 1:20 and y = 1:10 or 1:5
phen_WY %>% 
  mutate(x_new = y,
         y_new = x) %>% 
  mutate(x = x_new,
         y = y_new)-> phen_WY

# Check notes
phen_WY %>% 
  arrange(plantID, jday) %>% 
  pull(notes) %>% 
  unique()

# Remove data that has the note "recording error, no data exists for this date"
phen_WY %>% 
  filter(notes != "recording error, no data exists for this date" | is.na(notes)) -> phen_WY

# Create final columns
phen_WY %>% 
  mutate(herbivory = case_when(grepl("herbivory", notes) ~ "Y",
                               grepl("herbivory, only seed head gone", notes) ~ "Y",
                               grepl("herbivoery", notes) ~ "Y",
                               grepl("H herbivory", notes) ~ "Y",
                               grepl("h herbivory", notes) ~ "Y",
                               T ~ NA),
         frost_heave = NA,
         tillers = NA,
         harvested = case_when(v == "H" ~ "Y",
                               T ~ "N"),
         v = case_when(v == "IB" ~ "BS",
                       v == "?" ~ NA,
                       T ~ v)) -> phen_WY_format

# Create not in operator
`%notin%` <- Negate(`%in%`)

# Figure out which plants flowered over the course of phenology checks
phen_WY_format %>% 
  filter(v %in% c("FG", "FB", "FP")) %>% 
  group_by(plantID) %>%
  slice(which.min(jday)) %>% 
  ungroup() %>% 
  pull(plantID) -> flowered

# Figure out which plants did NOT flower over the course of phenology checks
phen_WY_format %>% 
  filter(plantID %notin% flowered) %>% 
  group_by(plantID) %>% 
  slice(which.max(jday)) %>% 
  filter(live == "Y") -> not_flowered

# Read in harvest matrix data
harvest <- suppressMessages(read_csv("gardens/rawdata/WY_harvest_matrix.csv"))

# This code figures out which plants weren't harvested by the end of the
# experiment but they should have been. Most of these were not harvested because
# they didn't flower (?) but about 1/4 of them had previously flowered.
harvest %>% 
  filter(check != 0) %>% 
  gather(key = date, value = harvested, `5/9/22`:`7/27/22`) %>% 
  group_by(block, density, albedo, X, Y, genotype, check) %>% 
  summarize(harvested_yes = length(which(complete.cases(harvested)))) %>% 
  filter(harvested_yes == 0)

# Assume that if they were harvested at some point, they had flowered. If they
# were NOT harvested, they did not flower.

harvest %>% 
  mutate(density = density_long,
         x = X,
         y = Y,
         gravel = albedo,
         genotype = parse_number(genotype)) %>% 
  gather(key = date, value = harvested, `5/9/22`:`7/27/22`) %>% 
  mutate(jday = yday(mdy(date))) %>% 
  mutate(jday = ifelse(jday < 270, jday, jday-365)) %>%
  # When a seed head or whole plant has been harvested, assume it flowered. This
  # is a big assumption that we need to check with Seth!!!
  mutate(v = case_when(complete.cases(harvested) ~ "FG",
                       # Set rest to NF = not flowered. This doesn't necessarily
                       # mean the plant has flowered already, but rather is used
                       # to catch instances when this would be its first time
                       # flowering.
                       T ~ "NF")) %>% 
  select(site, jday, block, density, gravel, x, y, genotype, v) %>% 
  filter(complete.cases(density, gravel))-> harvest_flowering
  
# Merge with plantID info
phen_WY %>% 
  select(plantID, block, density, gravel, x, y) %>% 
  distinct() %>% 
  merge(harvest_flowering) -> harvest_flowering

# Filter observations where plants were collected (assumed flowered). Only do
# this for plants that hadn't yet flowered during census.
harvest_flowering %>% 
  filter(plantID %in% not_flowered$plantID) %>% 
  filter(v == "FG") %>% 
  group_by(plantID) %>% 
  slice(which.min(jday)) %>% 
  ungroup() %>% 
  arrange(block, gravel, density, x, y) -> check

# write_csv(check,"~/Desktop/check.csv")

# The envelopes for these plant IDs were checked manually to see if there was
# evidence of flowering by the time of harvest

# Read this data frame in
ids_checked <- suppressMessages(read_csv("gardens/deriveddata/WY_harvest_flower_check.csv"))

# Filter out plants that flowered
ids_checked %>% 
  filter(`flowered?` == "y") %>% 
  # Set all to be FX. I didn't check the phenology carefully, jut figured out if
  # it had flowered or not so it could be FG, FP, or FB. Can fill this in later
  # when the seeds get processed.
  mutate(v = "FX",
         live = "Y",
         # Set these as NA now
         length_mm = NA,
         herbivory = NA,
         frost_heave = NA,
         notes = NA,
         # At least partial harvest here
         harvested = "Y") %>% 
  select(plantID, jday, live, v, length_mm, herbivory, frost_heave, harvested, notes) -> ids_checked_format

# Combine harvest flowering data with rest of phenology data
phen_WY_format %>% 
  select(names(ids_checked_format)) %>% 
  rbind(ids_checked_format) %>% 
  arrange(plantID, jday) -> phen_WY_format

# Set data frame to be same name format as Boise and SS
pgD <- phen_WY_format

# pull out and format phenology and growth data and notes for each plant
pgD <- pgD[order(pgD$plantID,pgD$jday),] # sort by plantID then Julian day

# check for bad "live" values
table(pgD$live) # N Y
#change live column to factor
pgD$live<-as.factor(pgD$live)

# check for bad "v" values
table(pgD$v)  # BS   FB   FG   FP   FX    H   V0   V1   V2   V3  V3+ 
# FX deals with the plants that flowered at some point during harvest but I
# didn't check the stage carefully.

# H means that the plant was already harvested, so I think we can change these
# to NA
pgD %>% 
  mutate(v = ifelse(v == "H", NA, v)) -> pgD

# check for bad length values
# first turn missing values into NAs then make length numeric
pgD$length_mm <- as.numeric(pgD$length_mm)
hist(pgD$length_mm) # a few very high values
# looks good

# compile notes
tmp <- pgD$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_notes_actions.csv"),row.names=F)
# MLV updated on 29 June 2023

# write pgD to file
write.csv(pgD,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)

# Write plantID key data to file
genotype_info <- suppressMessages(read_csv("gardens/rawdata/WY_genotypes.csv"))

genotype_info %>% 
  dplyr::select(block, density = density_long, gravel = albedo, source, growout = growout_yr,
         x = X, y = Y, genotype) -> genotype_info

merge(genotype_info, phen_WY) %>% 
  mutate(site = dosite,
         year = doyear,
         genotype = parse_number(genotype)) %>% 
  dplyr::select(plantID, site, year, block, plot, x, y, genotype, growout, density, gravel) %>% 
  distinct() -> plant_key

write.csv(plant_key,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_plantID.csv"),row.names=F)

# Remove all objects but pgD
rm(list=setdiff(ls(), "pgD"))