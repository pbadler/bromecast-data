# This code 
# 1. cleans up and combines compositional data from all years into one composition
# file written to deriveddata,
# 2. compiles and cleans species names for Bromus tectorum "competitors" and 
# 3. joins functional type data to species names 


# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

# Load libraries
library(tidyverse)

# Make %notin% operator
`%notin%` <- Negate(`%in%`)

# Read in 2020-2021 composition data
comp20 <- read_csv("../rawdata/Satellite_composition_2020-2021.csv")
# Read in 2021-2022 composition data
comp21 <- read_csv("../rawdata/Satellite_composition_2021-2022.csv")
# Read in 2022-2023 composition data
comp22 <- read_csv("../rawdata/Satellite_composition_2022-2023.csv")

# Rename columns for brevity
comp20 %>% mutate(year=2021) %>%
  select(sitecode = SiteCode,
         year=year,
         transect = `Transect (N, E, S, or W)`,
         treatment = `Treatment (control OR removal)`,
         distance_m = `Distance from center (m)`,
         species = Species,
         cover = Cover) -> comp20
         #litter_depth_cm = `Litter depth (if >1cm)`,
         #notes = Notes) -> comp20
#add missing columns
comp20$litter_depth_cm <- NA
comp20$notes <- NA


comp21 %>% mutate(year=2022) %>%
  select(sitecode = SiteCode,
         year=year,
         transect = `Transect (N, E, S, or W)`,
         treatment = `Treatment (control OR removal)`,
         distance_m = `Distance from center (m)`,
         species = Species,
         cover = Cover,
         litter_depth_cm = `Litter depth (cm)`,
         notes = Notes) -> comp21

comp22 %>% mutate(year=2023) %>%
  select(sitecode = SiteCode,
         year=year,
         transect = `Transect (N, E, S, or W)`,
         treatment = `Treatment (control OR removal)`,
         distance_m = `Distance from center (m)`,
         species = Species,
         cover = Cover,
         litter_depth_cm = `Litter depth (if >1cm)`,
         notes = Notes) -> comp22

# Combine species observation lists
comp_all <- rbind(comp20, comp21, comp22)
comp_all <- comp_all[order(comp_all$sitecode,comp_all$year,comp_all$transect,comp_all$distance_m),]
rm(comp20,comp21,comp22)

# PBA: I'm ignoring the problematic "notes" because I cleaned the demography
# data very carefully. That catches the real problems. Leftover notes for composition
# mostly concern species ID, which don't matter much at functional group level.


# Write csv file to code species as functional groups manually and to fix any issues
species_list <- sort(unique(comp_all$species))
write_csv(tibble(species = species_list), "../deriveddata/species_list_raw.csv" )

# did a previous species list exist?
tmp <- file.exists("../deriveddata/species_list_updates.csv")
  # if so, add any new entries to that list 
if(tmp==T){
    update_list <- read.csv("../deriveddata/species_list_updates.csv")
    update_list <- update_list[,c("species","update")]
    tmp <- species_list %notin% update_list$species
    if(sum(tmp)>0){
      tmp <- data.frame("species" = species_list[tmp], "update" = NA)
      update_list <- rbind(update_list,tmp)
      update_list <- update_list[order(update_list$species),]
      #write to file
      write.csv(update_list,"../deriveddata/species_list_updates.csv",row.names=F)
    }
}

# DONE BY HAND: fill in "update" column in 
# "../deriveddata/species_list_updates.csv" as needed

# # look up sitecode for a species code
# findspp <- "ERSP"
# comp_all[comp_all$species==findspp,]

# Read in species updates
updated_names <- read.csv("../deriveddata/species_list_updates.csv",header=T)
updated_names <- updated_names[,c("species","update")] #drop site column
# fill missing values with NA
tmp <- which(updated_names$update=="")
updated_names$update[tmp] <- NA

# update species names
comp_all <- merge(comp_all, updated_names, all.x = T)
tmp <- which(!is.na(comp_all$update))
comp_all$species[tmp] <- comp_all$update[tmp]

comp_all <- dplyr::select(comp_all,-update) # drop update column

# write updated species list (to join with functional trait data)
species_list_clean <- data.frame(species=sort(unique(comp_all$species)))
write.csv(species_list_clean,"../deriveddata/species_list_clean.csv",row.names=F)

# clean up
rm(tmp,update_list,updated_names,species_list_clean)

###

# join species list with functional type data
spp_list <- read.csv("../deriveddata/species_list_clean.csv",header=T)
fgroups <- read.csv("../deriveddata/species2functionalgroups.csv",header=T)
spp_list <- merge(spp_list,fgroups, all.x=T)
write.csv(spp_list,"../deriveddata/species2functionalgroups.csv", row.names=F)
rm(spp_list)

# fill out functional type data by hand

# read in latest version of functional type data
fgroups <- read.csv("../deriveddata/species2functionalgroups.csv",header=T)



### define functional groups

# annual, perennial, shrub, biocrust, unknown
fgroups$ftypes1 <- NA
fgroups$ftypes1[fgroups$type=="non-plant"] <- "groundcover"
fgroups$ftypes1[fgroups$duration=="annual" | fgroups$duration=="biennial"] <- "annual"
fgroups$ftypes1[fgroups$duration=="perennial"] <- "perennial"
fgroups$ftypes1[fgroups$duration=="unknown"] <- "unknown"
fgroups$ftypes1[fgroups$type=="shrub"] <- "shrub"
fgroups$ftypes1[fgroups$type=="biocrust"] <- "biocrust"
# unknowns  NAs, these will be CUT 

# annual forb, annual grass, perennial forb, perennial grass, shrub, biocrust
fgroups$ftypes2 <- fgroups$ftypes1
tmp <- which(fgroups$ftypes1=="annual")
fgroups$ftypes2[tmp] <- paste0(fgroups$ftypes2[tmp],fgroups$type[tmp])
tmp <- which(fgroups$ftypes1=="perennial")
fgroups$ftypes2[tmp] <- paste0(fgroups$ftypes2[tmp],fgroups$type[tmp])

# annual forb, annual grass, perennial forb, perennial grass c3,  perennial grass c4, shrub, biocrust
fgroups$ftypes3 <- fgroups$ftypes2
tmp <- which(fgroups$ftypes2=="perennialgrass")
fgroups$ftypes3[tmp] <- paste0(fgroups$ftypes3[tmp],fgroups$c3c4[tmp])

fgroups <- fgroups[,c("species","ftypes1","ftypes2","ftypes3")]



### join functional group data to composition data and do some cleaning
comp_all <- merge(comp_all,fgroups,all.x=T)

# remove records where cover == NA. These are missing toothpicks and 2 bare ground obs 
tmp <- which(is.na(comp_all$cover))
comp_all <- comp_all[-tmp,]

# remove other missing records flagged by value of cover = -1
tmp <- which(comp_all$cover == -1)
comp_all <- comp_all[-tmp,]
tmp <- which(comp_all$cover == "missing")
comp_all <- comp_all[-tmp,]
tmp <- which(comp_all$cover == "M")
comp_all <- comp_all[-tmp,]

# other checks
table(comp_all$ftypes1)
table(comp_all$cover)

# fix non-numeric cover values
comp_all$cover[comp_all$cover == "<1"] <- 0.5
comp_all$cover[comp_all$cover == ">5"] <- 7.5
comp_all$cover[comp_all$cover == "105"] <- 100
comp_all$cover[comp_all$cover == "775"] <- 75  # PBA: I checked this one in the raw data, plant cover at this site = 25
comp_all$cover <- as.numeric(comp_all$cover)

# remove non-plant records
table(comp_all$species[is.na(comp_all$ftypes1)]) # first check NAs in ftypes
comp_all <- subset( comp_all, !is.na(comp_all$ftypes1))

# check NOTES? PBA: I don't see high priority problems here

# edit column names to match demography file
names(comp_all)[names(comp_all)=="sitecode"] <- "SiteCode"
names(comp_all)[names(comp_all)=="year"] <- "Year"
names(comp_all)[names(comp_all)=="transect"] <- "Transect"
names(comp_all)[names(comp_all)=="treatment"] <- "Treatment"
names(comp_all)[names(comp_all)=="distance_m"] <- "Distance"

# clean up bad Treatment values
comp_all$Treatment[comp_all$Treatment == "Control"] <- "control"
comp_all$Treatment[comp_all$Treatment == "Removal"] <- "removal"
tmp <- which(comp_all$SiteCode=="EnsingS1" & comp_all$Year==2022 & comp_all$Transect=="W")
comp_all$Treatment[tmp] <- "removal"
tmp <- which(comp_all$SiteCode=="EnsingS1" & comp_all$Year==2022 & comp_all$Transect=="E")
comp_all$Treatment[tmp] <- "removal"

# clean up bad distances
tmp <- which(comp_all$SiteCode=="EOARC" & comp_all$Year==2022 & comp_all$Transect=="E")
comp_all$Distance[tmp] <- comp_all$Distance[tmp] + 2
tmp <- which(comp_all$SiteCode=="EOARC" & comp_all$Year==2022 & comp_all$Transect=="S")
comp_all$Distance[tmp] <- comp_all$Distance[tmp] + 2

# make sure site names match demography file
comp_all$SiteCode[comp_all$SiteCode=="EnsingS1"] <- "EnsingS1 SuRDC"
comp_all$SiteCode[comp_all$SiteCode=="EnsingS2"] <- "EnsingS2 Summerland-Princeton"
comp_all$SiteCode[comp_all$SiteCode=="EnsingS3"] <- "EnsingS3 Bear Creek"
comp_all$SiteCode[comp_all$SiteCode=="EnsingS4"] <- "EnsingS4 Lundbom"
comp_all$SiteCode[comp_all$SiteCode=="SymstadS1"] <- "Symstad1"
comp_all$SiteCode[comp_all$SiteCode=="South Eden"] <- "SouthEden"


### aggregate neighborhood cover to functional group level for each individual

comp_ftypes1 <- comp_all %>% group_by(SiteCode,Year,Transect,Treatment,Distance, ftypes1) %>%
                  summarize(cover = sum(cover)) %>%
                  pivot_wider(names_from = ftypes1, values_from = cover, values_fill = 0)
comp_ftypes1 <- as.data.frame(comp_ftypes1)

comp_ftypes2 <- comp_all %>% group_by(SiteCode,Year,Transect,Treatment,Distance, ftypes2) %>%
                  summarize(cover = sum(cover)) %>%
                  pivot_wider(names_from = ftypes2, values_from = cover, values_fill = 0)
comp_ftypes2 <- as.data.frame(comp_ftypes2)

comp_ftypes3 <- comp_all %>% group_by(SiteCode,Year,Transect,Treatment,Distance, ftypes3) %>%
                 summarize(cover = sum(cover)) %>%
                 pivot_wider(names_from = ftypes3, values_from = cover, values_fill = 0)
comp_ftypes2 <- as.data.frame(comp_ftypes2)

rm(comp_all)

# Ensing Treatments don't match
# EOARC removal distances don't match


