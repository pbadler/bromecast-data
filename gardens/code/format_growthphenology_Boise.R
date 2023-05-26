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

# import raw growth and phenology data
rawD <- read.csv(here("gardens/rawdata/BromeCast_2021-2022_All_Idaho_Sites-Full_Dataset_Collated.xlsx - All Sites_All Data.csv"),header=T)

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
rawD$herbivory<- ifelse(rawD$herbivory.present. == TRUE, "Y", "N") 
rawD$frost_heave<-ifelse(rawD$frost.heaved. == TRUE, "Y", "N")
rawD$harvested<- ifelse(rawD$plant.harvested. == TRUE, "Y", "N")


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

unique(rawD$plot) # There's some issues with plot numbering in the entered data

# Fix these manually
rawD %>% 
  mutate(plot = case_when(plot == 10 ~ 1,
                          plot == 11 & block == 6 ~ 1,
                          plot == 11 & block == 4 ~ 2,
                          is.na(plot) ~ 1,
                          T ~ plot)) -> rawD

# Check to make sure that worked
unique(rawD$plot) # All good

# assign each individual plant a unique ID, link to grid positions
plant_key <- rawD[,c("site","block","plot","x","y","genotype","growout")]
plant_key <- unique(plant_key,MARGIN=2)
plant_key$plantID <- paste0(plant_key$site,doyear,"_",1:nrow(plant_key))
rawD <- merge(rawD,plant_key)
plant_key$site <- dosite
plant_key$year <- doyear
plant_key <- plant_key[,c(8,1,9,2:7)]
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
# open as a spreadsheet, fill in action column by hand
# This has been edited as a raw file by MLV on 19 April 2023
# MLV NEED TO REDO THIS FOR UPDATED DATA!!

rm(rawD)

# write pgD to file
write.csv(pgD,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)
