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
plant_key <- rawD[,c("site","block","plot","x","y","gravel", "density")]
plant_key %>% 
  # Some genotype values are NA so need to fix this
  #filter(complete.cases(genotype)) %>% 
  distinct(site, block, plot, gravel, density, x, y) -> plant_key

# There are 8326 unique combinations of site, block, plot, gravel, density, x &
# y (326 too many!!). MLV is fixing what we can manually but need to check in
# with Boise folks

# Pull out times where there is just one observation of a unique ID
rawD %>% 
  mutate(ids = paste(site, block, plot, gravel, density, x, y, sep = "_")) -> rawD 
  
rawD %>% 
  group_by(ids) %>% 
  summarize(n = n()) %>% 
  filter(n < 3) %>% 
  pull(ids) -> bad_ids


## Fixing manually by date ####

# Create datasets that do and don't include bad-ids
`%notin%` <- Negate(`%in%`)

rawD %>% 
  filter(ids %notin% bad_ids) -> rawD_good

rawD %>% 
  filter(ids %in% bad_ids) -> rawD_bad

# 05/18/2022
rawD_bad_0518 <- rawD_bad %>% filter(date == "2022-05-18")
rawD_bad_0518 %>% 
  filter(complete.cases(x,y)) %>% 
  mutate(x = 2) -> rawD_good_0518

# 05/23/2022
rawD_bad_0523 <- rawD_bad %>% filter(date == "2022-05-23")
rawD_bad_0523 %>% 
  mutate(density = "Low",
         plot = 2) -> rawD_good_0523

# 05/24/2022
rawD_good_0524 <- rawD_bad %>% filter(date == "2022-05-24")
# In block 2 plot 3, some should be high white but can't tell which easily

# 05/25/2022
rawD_bad_0525 <- rawD_bad %>% filter(date == "2022-05-25")

rawD_bad_0525[which(rawD_bad_0525$block == 5 & rawD_bad_0525$plot == 2), "x"] <- 19
rawD_bad_0525[which(rawD_bad_0525$block == 5 & rawD_bad_0525$plot == 2), "y"] <- 3
rawD_bad_0525[which(rawD_bad_0525$block == 5 & rawD_bad_0525$plot == 3), "plot"] <- 2
rawD_bad_0525[which(rawD_bad_0525$block == 10 & rawD_bad_0525$plot == 3), "x"] <- 6
rawD_bad_0525[which(rawD_bad_0525$block == 10 & rawD_bad_0525$plot == 3), "y"] <- 10

# Still need to fix stuff
rawD_bad_0525 -> rawD_good_0525

# 05/27/2022
rawD_bad_0527 <- rawD_bad %>% filter(date == "2022-05-27")

rawD_bad_0527[which(rawD_bad_0527$block == 3 & rawD_bad_0527$plot == 4), "x"] <- 3
rawD_bad_0527[which(rawD_bad_0527$block == 3 & rawD_bad_0527$plot == 4), "y"] <- 4
rawD_bad_0527[which(rawD_bad_0527$block == 7 & rawD_bad_0527$plot == 2), "density"] <- rep("Low", 3)
rawD_bad_0527[which(rawD_bad_0527$block == 8 & rawD_bad_0527$plot == 1), "y"] <- 2

rawD_bad_0527 %>% 
  filter(block != 8 | plot != 4) -> rawD_good_0527

# 05/31/2022
# Still can't figure these out
rawD_good_0531 <- rawD_bad %>% filter(date == "2022-05-31")

# 06/02/2022
rawD_bad_0602 <- rawD_bad %>% filter(date == "2022-06-02")

rawD_bad_0602[which(rawD_bad_0602$block == 3 & rawD_bad_0602$plot == 4), "density"] <- rep("Low", 14)
rawD_bad_0602[which(rawD_bad_0602$block == 3 & rawD_bad_0602$plot == 1), "density"] <- rep("High", 58)
rawD_bad_0602[which(rawD_bad_0602$block == 9 & rawD_bad_0602$plot == 4), "density"] <- rep("High", 41)

rawD_good_0602 <- rawD_bad_0602

# 06/03/2022
rawD_bad_0603 <- rawD_bad %>% filter(date == "2022-06-03")

rawD_bad_0603[which(rawD_bad_0603$block == 1 & rawD_bad_0603$plot == 2), "x"][1] <- 10
rawD_bad_0603[which(rawD_bad_0603$block == 1 & rawD_bad_0603$plot == 2), "y"][1] <- 4
rawD_bad_0603[which(rawD_bad_0603$block == 8 & rawD_bad_0603$plot == 4), "x"] <- 8
rawD_bad_0603[which(rawD_bad_0603$block == 8 & rawD_bad_0603$plot == 4), "y"] <- 2
rawD_bad_0603[which(rawD_bad_0603$block == 9 & rawD_bad_0603$plot == 2), "gravel"] <- "Black"

rawD_bad_0603 %>% 
  filter(block != 8 | plot != 3 | x !=3) -> rawD_good_0603

# 06/23/2022
rawD_bad_0623 <- rawD_bad %>% filter(date == "2022-06-23")
rawD_bad_0623[which(rawD_bad_0623$x == 7), "x"] <- 1

rawD_good_0623 <- rawD_bad_0623

# 06/28/2022
# Still can't figure out
rawD_good_0628 <- rawD_bad %>% filter(date == "2022-06-28")

# 06/29/2022
rawD_bad %>% 
  filter(date == "2022-06-29") %>% 
  filter(block != 1 | plot != 1 | x != 5) -> rawD_good_0629

# 06/30/2022
rawD_bad_0630 <- rawD_bad %>% filter(date == "2022-06-30")
rawD_bad_0630$density <- rep("Low", 7)
rawD_good_0630 <- rawD_bad_0630

# 07/07/2022
# Still can't figure out
rawD_good_0707 <- rawD_bad %>% filter(date == "2022-07-07")

# 07/08/2022
# Still can't figure out
rawD_good_0708 <- rawD_bad %>% filter(date == "2022-07-08")

# Bind together all the 'good' datasets
rbind(rawD_good_0518,
      rawD_good_0523,
      rawD_good_0524,
      rawD_good_0525,
      rawD_good_0527,
      rawD_good_0602,
      rawD_good_0603,
      rawD_good_0623,
      rawD_good_0628,
      rawD_good_0629,
      rawD_good_0630,
      rawD_good_0707,
      rawD_good_0708) -> updated_rawD_bad

rbind(rawD_good, updated_rawD_bad) %>% 
  mutate(ids = paste(site, block, plot, gravel, density, x, y, sep = "_")) %>% 
  group_by(ids) %>% 
  summarize(n = n()) %>% 
  filter(n == 1) %>% pull(ids) -> bad_ids_left

rawD %>% 
  filter(ids %in% bad_ids_left) -> to_check_Boise

write_csv(to_check_Boise,"gardens/deriveddata/bad_ids_Boise.csv")

plant_key$plantID <- paste0(plant_key$site,doyear,"_",1:nrow(plant_key))
# This is still dropping 100+ observations... why???
rawD <- merge(rawD, plant_key %>% select(-genotype))

rawD_test <- merge(rawD, plant_key %>% select(-genotype), all.x = T)
rawD_test %>% 
  filter(site == "BA" & date == "2022-05-24") %>% 
  group_by(block, plot) %>% 
  summarize(n = n()) %>% 
  arrange(block, plot) %>% 
  print(n = Inf)

rawD_test %>% 
  filter(site == "WI" & block == 7 & plot == 2)%>% 
  group_by(density, gravel.color,date) %>% 
  summarize(n = n()) %>% 
  print(n = Inf)

rawD_test %>% 
  filter(site == "BA" & block == 3 & plot == 4 & date == "2022-07-07") %>% 
  select(x,y)

rawD_test %>% 
  group_by(site, block, plot, density, gravel.color) %>% 
  summarize(n = n()) %>% 
  print(n = Inf)

plant_key$year <- doyear
plant_key <- plant_key %>% select(plantID, site, year, block, plot, x, y, genotype)
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
