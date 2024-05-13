# Formatting Cheyenne phenology to be in same structure as Sheep Station 2023

library(lubridate); library(tidyverse); library(here)

doyear <- 2023 # growing season to do
dosite <- "CH" # code for focal site

# Read in data
julyB <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_July2.csv"))
julyA <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_July1.csv"))
juneB <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_June2.csv"))
juneA <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_June1.csv"))
mayB <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_May2.csv"))
mayA <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_May1.csv"))
aprB <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_April2.csv"))
aprA <- suppressMessages(read_csv("gardens/rawdata/Wy_Census_data2023_FINAL_April1.csv"))

# Create a list of data
all_phen <- list(julyB, julyA, juneB, juneA, mayB, mayA, aprB, aprA)

# Create vector of days (these are just placeholders for now)
dates <- c("julyB", "julyA","juneB", "juneA", "mayB", "mayA", "aprB", "aprA")

for(i in 1:length(dates)){
  
  # Set temp data name
  rawD <- all_phen[[i]]
  
  # Fix alignment of column names
  rawD_col_1_4 <- rawD[3:302,1:4]
  names(rawD_col_1_4) <- tolower(rawD[2,1:4])
  
  # Fix issues with different locations of column headers -- plants
  rawD_plants <- rawD[3:302,5:37]
  names(rawD_plants) <- c("x", paste(rep(c("v", "notes"), 16), rep(1:16, each = 2), sep = "_"))
  
  # Bring two data sets back together
  newD <- cbind(rawD_col_1_4, rawD_plants)
  
  # Bring v stage columns to rows
  newD %>% 
    gather(key = y, value = v, contains("v")) %>% 
    mutate(y = as.numeric(parse_number(y))) %>%
    dplyr::select(date, albedo, density, block, x, y, v) %>% 
    distinct() -> newD_v
    
  # Bring notes columns to rows
  newD %>% 
    gather(key = y, value = notes, contains("notes")) %>% 
    mutate(y = as.numeric(parse_number(y))) %>% 
    dplyr::select(date, albedo, density, block, x, y, notes) %>% 
    distinct() -> newD_notes
  
  cbind(newD_v, notes = newD_notes$notes)-> longD
  
  # Remove data for y = 9:16 for high density treatment
  longD %>% 
    filter(density == "high" & x %in% 1:10 & y %in% 1:8) -> longD_high
  
  # Remove data for x = 6:10 for low density treatment
  longD %>% 
    filter(density == "low" & x %in% 1:5 & y %in% 1:16) -> longD_low
  
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
           live = case_when(v == "VX" ~ "N",
                            v == "H" ~ NA,
                            v == "n/a" ~ NA,
                            T ~ "Y"),
           jday = yday(mdy(date))) %>% 
    mutate(jday = ifelse(jday < 270, jday, jday-365)) %>% 
    dplyr::select(plantID, density, gravel = albedo, block, plot, x, y, jday, live, v, notes) -> phen_sample
  
  assign(paste0(dates[i], sep = "_", "phen"), phen_sample)
  
}

# Bring all data together
rbind(aprA_phen, aprB_phen, mayA_phen, mayB_phen,
      juneA_phen, juneB_phen, julyA_phen, julyB_phen) -> phen_WY

# Switch x and y so that x = 1:10 or 1:20 and y = 1:10 or 1:5
phen_WY %>% 
  mutate(x_new = y,
         y_new = x) %>% 
  mutate(x = x_new,
         y = y_new)-> phen_WY

# Arrange by plant ID and see if things make sense
phen_WY %>% 
  arrange(plantID, jday)

# Write notes csv to alter manually
tmp <- phen_WY$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_notes_actions.csv"),row.names=F)
# MLV completed 13 May 2024

# Create final columns
phen_WY %>% 
  mutate(herbivory = NA,
         frost_heave = NA,
         tillers = NA,
         harvested = case_when(v == "H" ~ "Y",
                               T ~ "N"),
         v = case_when(# If VX, that means it is dead
                       v == "VX" ~ NA,
                       v == "n/a" ~ NA,
                       # Flowering red is the same as flowering purple?
                       v == "FR" ~ "FP",
                       v == "IB" ~ "BS",
                       v == "H" ~ NA,
                       T ~ v)) %>% 
  select(-x_new, -y_new) -> phen_WY_format

# Set data frame to be same name format as Boise and SS
pgD <- phen_WY_format

# pull out and format phenology and growth data and notes for each plant
pgD <- pgD[order(pgD$plantID,pgD$jday),] # sort by plantID then Julian day

# check for bad "live" values
table(pgD$live) # N Y
#change live column to factor
pgD$live<-as.factor(pgD$live)

# check for bad "v" values
table(pgD$v)  # Already fixed

# compile notes
tmp <- pgD$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_notes_actions.csv"),row.names=F)
# MLV updated on 29 June 2023
tmp <- read_csv("gardens/deriveddata/CH2023_notes_actions.csv")
# Merge back together with data
merge(pgD, tmp) -> pgD

# Order and select pgD columns
pgD %>% 
  select(plantID, jday, live, v, herbivory, frost_heave, harvested, note_standard) -> pgD

# write pgD to file
write.csv(pgD,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)

# Write plantID key data to file
genotype_info <- suppressMessages(read_csv("gardens/rawdata/WY_genotypes_2023.csv"))

genotype_info %>% 
  dplyr::select(block, density = density_long, gravel = albedo, source, growout = bulkYear_final,
                x = X, y = Y, genotype = genotypeID) -> genotype_info

merge(genotype_info, phen_WY) %>% 
  mutate(site = dosite,
         year = doyear,
         genotype = parse_number(genotype)) %>% 
  dplyr::select(plantID, site, year, block, plot, x, y, genotype, growout, density, gravel) %>% 
  distinct() -> plant_key

write.csv(plant_key,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_plantID.csv"),row.names=F)

# Remove all objects but pgD
rm(list=setdiff(ls(), "pgD"))
