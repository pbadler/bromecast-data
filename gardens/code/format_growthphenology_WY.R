# Formatting Cheyenne phenology to be in same structure as Sheep Station

library(lubridate); library(tidyverse)

doyear <- 2022 # growing season to do
dosite <- "CH" # code for focal site

# Read in data
juneA <- read_csv("gardens/rawdata/WY_census_data_June22a.csv")
mayB <- read_csv("gardens/rawdata/WY_census_data_May22b.csv")
mayA <- read_csv("gardens/rawdata/WY_census_data_May22a.csv")
aprB <- read_csv("gardens/rawdata/WY_census_data_April22b.csv")
aprA <- read_csv("gardens/rawdata/WY_census_data_April22a.csv")
marB <- read_csv("gardens/rawdata/WY_census_data_March22b.csv")
marA <- read_csv("gardens/rawdata/WY_census_data_March22a.csv")

# Create a list of data
all_phen <- list(juneA, mayB, mayA, aprB, aprA, marB, marA)

# Create vector of days (these are just placeholders for now)
jdays <- c(152, 135, 121, 105, 91, 74, 60)
dates <- c("juneA", "mayB", "mayA", "aprB", "aprA", "marB", "marA")

for(i in 1:length(jdays)){
  
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
           live = ifelse(complete.cases(plant_length) & v != "H", "Y", "N"),
           # Placeholder June 1st until we figure out what the real date is
           jday = jdays[i]) %>% 
    dplyr::select(plantID, density, gravel = albedo, block, plot, x, y, jday, live, v, length_mm = plant_length, notes) -> phen_sample
  
  assign(paste0(dates[i], sep = "_", "phen"), phen_sample)
  
}

# Bring all data together
rbind(marA_phen, marB_phen, aprA_phen, aprB_phen, mayA_phen, mayB_phen, juneA_phen) -> phen_WY

# Check notes
phen_WY %>% 
  arrange(plantID, jday) %>% 
  pull(notes) %>% 
  unique()

# Create final columns
phen_WY %>% 
  mutate(herbivory = case_when(grepl("herbivory", notes) ~ "Y",
                               T ~ NA),
         frost_heave = NA,
         tillers = NA,
         harvested = case_when(v == "H" ~ "Y",
                               T ~ "N"),
         v = case_when(v == "IB" ~ "BS",
                       v %in% c("H", "?") ~ NA,
                       T ~ v)) %>% 
  dplyr::select(plantID, jday, live, v, length_mm, herbivory, frost_heave, tillers, harvested, notes) -> phen_WY_format
  
# Write phenology data to file
write.csv(phen_WY_format,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)

# Write plantID key data to file
genotype_info <- read_csv("gardens/rawdata/WY_genotypes.csv")

genotype_info %>% 
  dplyr::select(block, density = density_long, gravel = albedo, source, growout = growout_yr,
         x = Y, y = X, genotype) -> genotype_info

merge(genotype_info, phen_WY) %>% 
  mutate(site = dosite,
         year = doyear,
         genotype = parse_number(genotype)) %>% 
  dplyr::select(plantID, site, year, block, plot, x, y, genotype, growout, density, gravel) %>% 
  distinct() -> plant_key

write.csv(plant_key,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_plantID.csv"),row.names=F)


  
