#Boise Version
#01/20/22

#changes:
#line 22 dosite changed to "Boise" for file naming purposes
#line 32 changing column names to match sheep station
#line 36 changed column name from "bulkyear" to "growout" *I belive this are equivalent
#line 47 change sites names (balzor/wildcat) to BA and WI
#line 52 comment out plot key section (not needed)
#line 63 added in "plant_key$site" to plant key to identify between BA (Balzor)
#and WI (Wildcat) sites
#Removed "tillers" as column
#line 78 only 9 columns in pgD table, changed numbers respectively
#line 83 changed "live" column entries to match sheep station and removed both
#missing and "unknown" values
#line 106 v values changed to match sheep station
#line 114 subset out bad length values (>250mm)
####################
#library
library(lubridate); library(tidyverse); library(here)

doyear <- 2022 # growing season to do
dosite <- "Boise" # code for focal site

# Create %notin% operator
`%notin%` <- Negate(`%in%`)

# import raw growth and phenology data
rawD <- read.csv(here("gardens/rawdata/BromeCast_2021-2022_All_Idaho_Sites_MLVedits.csv"),header=T)

# remove capital letters from column headers 
# (this will make it easier to harmonize data across sites)
names(rawD) <- tolower(names(rawD))

#rename column names to match sheepstation
rawD$plot<-rawD$subplot
rawD$x<-rawD$x.coordinate
rawD$y<-rawD$y.coordinate
rawD$genotype <- parse_number(rawD$genotypeid)
rawD$growout<-rawD$bulkyear
rawD$live<-rawD$seedling.present.
rawD$v<-rawD$phenology
rawD$length_mm<-rawD$max.leaf.length..mm.
rawD$herbivory<- ifelse(rawD$herbivory.present. == "Yes", "Y", "N") 
rawD$frost_heave<-ifelse(rawD$frost.heaved. == "Yes", "Y", "N")
rawD$harvested<- ifelse(rawD$plant.harvested. == "Yes", "Y", "N")
rawD %>% 
  mutate(gravel = case_when(gravel.color %in% c("Black", "BlacK", "Back", "Black ") ~ "Black",
                            gravel.color %in% c("White", "Whtie", "white") ~ "White"),
         density = case_when(density %in% c("High", "high", "HIgh", "hIgh") ~ "High",
                             density %in% c("Low", "low") ~ "Low")) -> rawD

# Replace site names 
rawD %>% 
  mutate(site = case_when(site %in% c("Balzor", "Baltzor") ~ "BA",
                          site == "Wildcat" ~ "WI")) -> rawD
# change date field from character to date format
rawD$date <- mdy(rawD$date)

# IF plots are numbered 1 to 40, reformat to block and subplot 
# names(rawD)[which(names(rawD)=="plot")] <- "cum_plot"
# tmp <- data.frame(block=sort(rep(1:10,4)),
#                   plot=rep(1:4,10),
#                   cum_plot=1:40)
# rawD<-merge(rawD,tmp)
# rm(tmp)

sort(unique(rawD$plot)) # All good
sort(unique(rawD$block)) # All good

# assign each individual plant a unique ID, link to grid positions
plant_key <- rawD[,c("site","block","plot","x","y","gravel", "density")]
plant_key %>% 
  distinct(site, block, plot, gravel, density, x, y) -> plant_key

nrow(plant_key)
# There are 8011 unique combinations of site, block, plot, gravel, density, x &
# y (11 too many). MLV is fixed what we could manually but there are 11
# observations that I can't figure out.

# Pull out times where there is just one observation of a unique ID
rawD %>% 
  mutate(ids = paste(site, block, plot, gravel, density, x, y, sep = "_")) -> rawD 

rawD %>% 
  group_by(ids) %>% 
  summarize(n = n()) %>% 
  filter(n == 1) %>% 
  pull(ids) -> bad_ids

# Remove remaining bad_ids that I can't figure out from the dataset
rawD %>% 
  filter(ids %notin% bad_ids) -> rawD

# Check to see how many unique plants there are
length(unique(rawD$ids))

## Fix coordinate switching that happened after the first census ####

# Get the first census for each plant position that has the CORRECT genotype
rawD %>% 
  filter(year(date) == 2021) -> rawD_survey1

# Create data frame of just positions and correct genotype codes
rawD_survey1 %>% 
  select(site, block, plot, density, gravel, x, y, genotype, source) -> genotype_codes_correct

# Get the rest of the data
rawD %>% 
  filter(year(date) != 2021) -> rawD_surveyrest

# For the rest of the data, need to switch direction of the Y coordinates
rawD_surveyrest %>% 
  mutate(y= case_when(density == "Low" & y == 1 ~ 5,
                       density == "Low" & y == 2 ~ 4,
                       density == "Low" & y == 4 ~ 2,
                       density == "Low" & y == 5 ~ 1,
                       density == "Low" & y == 3 ~ 3,
                       density == "High" & y == 1 ~ 10,
                       density == "High" & y == 2 ~ 9,
                       density == "High" & y == 3 ~ 8,
                       density == "High" & y == 4 ~ 7,
                       density == "High" & y == 5 ~ 6,
                       density == "High" & y == 6 ~ 5,
                       density == "High" & y == 7 ~ 4,
                       density == "High" & y == 8 ~ 3,
                       density == "High" & y == 9 ~ 2,
                       density == "High" & y == 10 ~ 1)) -> rawD_surveyrest

# Reset y column and remove genotype and source columns
rawD_surveyrest %>% 
  dplyr::select(-genotype, -source) -> rawD_surveyrest

# Merge dataset with genotype information
merge(rawD_surveyrest, genotype_codes_correct) -> rawD_surveyrest

# Put two datasets back together
rawD_surveyrest %>% 
  select(names(rawD_survey1)) %>% 
  rbind(rawD_survey1) %>% 
  arrange(site, block, plot, x, y, date) -> rawD

# Format IDs to be the same as Sheep Station
plant_key <- rawD_survey1[,c("site","block","plot","x","y", "genotype")]

plant_key$plantID <- paste0(dosite,doyear,"_",1:nrow(plant_key))
rawD <- merge(rawD,plant_key)

plant_key$year <- doyear
plant_key <- plant_key %>% dplyr::select(plantID, site, year, block, plot, x, y, genotype)

write.csv(plant_key,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_plantID.csv"),row.names=F)
rm(plant_key)

# pull out and format phenology and growth data and notes for each plant
pgD <- rawD[,c("plantID","date","live","v","length_mm","herbivory","frost_heave","harvested","notes")]
# get Julian day
pgD$jday <-yday(pgD$date)
# make fall days negative
pgD$jday <- ifelse(pgD$jday < 270, pgD$jday, pgD$jday-365)
pgD <- dplyr::select(pgD, -date) # drop date column
pgD <- pgD[,c(1,9,2:8)] # reorder columns
pgD <- pgD[order(pgD$plantID,pgD$jday),] # sort by plantID then Julian day

# check for bad "live" values
table(pgD$live) # Null, Dead, N, no, No, unk, Unknown, Y, yes, Yes
#change live column to factor
pgD$live<-as.factor(pgD$live)
#Change ("Dead","No", "no") to "N" + ("Yes" and "yes")to "Y"
levels(pgD$live)[levels(pgD$live) %in%  c("Dead","No","no")] <- 'N'
levels(pgD$live)[levels(pgD$live) %in%  c("Yes","yes")] <- 'Y'
# PBA: I inspected each plant with one or more missing 
# values, decided that cleanest solution is to simply
# remove all missing records (dates). Most occur at
# first visit or after a plant had already died.
##If  technicians were unsure of plant location, unknown was entered (Boise)
# remove missing and unknown values
pgD <- subset(pgD, pgD$live %in% c("Y","N"))
       
# check for bad "v" values
table(pgD$v)  # Blank, >V3, bootstage, Bootstage, Fb, Fg, Fp, V0, V1, V2, V3
#change v values to match sheep station (NA, V3+, BS, FB?, FG, FP?, etc.)
#convert all entries to uppercase
pgD$v <- toupper(pgD$v)
#change column to factor
pgD$v<-as.factor(pgD$v)
#Change blanks to "NA", ">V3" to "V3+" and "Bootstage" to "BS"
levels(pgD$v)[levels(pgD$v)==""] <- 'NA'
levels(pgD$v)[levels(pgD$v)==">V3"] <- 'V3+'
levels(pgD$v)[levels(pgD$v)=="BOOTSTAGE"] <- 'BS'
levels(pgD$v)[levels(pgD$v)=="FB "] <- 'FB'
levels(pgD$v)[levels(pgD$v)=="FP "] <- 'FP'
# There are also two "FL". For the phenology analyses we can change this to "FP"
# without any consequence, but should check with ID folks (MLV)
levels(pgD$v)[levels(pgD$v)=="FL"] <- 'FP'

unique(pgD$v)

# check for bad length values
# first turn missing values into NAs then make length numeric
pgD$length_mm <- as.numeric(pgD$length_mm)
hist(pgD$length_mm) # a few very high values
#subset out values less than 250
pgD<-subset(pgD, pgD$length_mm<250 | is.na(length_mm)) 
min(pgD$length_mm,na.rm=T) # looks good

# compile notes
tmp <- pgD$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
# write.csv(tmp,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_notes_actions.csv"),row.names=F)
# Mlva updated on 22 June 2023

rm(rawD)

# write pgD to file
write.csv(pgD,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)
