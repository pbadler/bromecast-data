


doyear <- 2022 # growing season to do
dosite <- "SS" # code for focal site

# import raw growth and phenology data
rawD <- read.csv(here("gardens/rawdata/phenology_Sheep_Station_2021-2022_MASTER.csv"),header=T)

# remove capital letters from column headers 
# (this will make it easier to harmonize data across sites)
names(rawD) <- tolower(names(rawD))

# change date field from character to date format
rawD$date <- mdy(rawD$date)

# IF plots are numbered 1 to 40, reformat to block and subplot 
names(rawD)[which(names(rawD)=="plot")] <- "cum_plot"
tmp <- data.frame(block=sort(rep(1:10,4)),
                  plot=rep(1:4,10),
                  cum_plot=1:40)
rawD<-merge(rawD,tmp)
rm(tmp)

# assign each individual plant a unique ID, link to grid positions
plant_key <- rawD[,c("site","block","plot","x","y","genotype","growout")]
plant_key <- unique(plant_key,MARGIN=2)
plant_key$plantID <- paste0(dosite,doyear,"_",1:nrow(plant_key))
rawD <- merge(rawD,plant_key)
plant_key$site <- dosite
plant_key$year <- doyear
plant_key <- plant_key[,c(8,1,9,2:7)]
write.csv(plant_key,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_plantID.csv"),row.names=F)
rm(plant_key)

# pull out and format phenology and growth data and notes for each plant
pgD <- rawD[,c("plantID","date","live","v","length_mm","herbivory","frost_heave","tillers","harvested","notes")]
# get Julian day
pgD$jday <-yday(pgD$date)
# make fall days negative
pgD$jday <- ifelse(pgD$jday < 270, pgD$jday, pgD$jday-365)
pgD <- dplyr::select(pgD, -date) # drop date column
pgD <- pgD[,c(1,10,2:9)] # reorder columns
pgD <- pgD[order(pgD$plantID,pgD$jday),] # sort by plantID then Julian day

# check for bad "live" values
table(pgD$live) # missing, N, Y 
pgD[pgD$live=="missing",] 
# PBA: I inspected each plant with one or more missing 
# values, decided that cleanest solution is to simply
# remove all missing records (dates). Most occur at
# first visit or after a plant had already died.
pgD <- subset(pgD,live!="missing")

# check for bad "v" values
table(pgD$v)  # looks good 

# check for bad length values
# first turn missing values into NAs then make length numeric
pgD$length_mm <- as.numeric(pgD$length_mm)
hist(pgD$length_mm) # looks good
min(pgD$length_mm,na.rm=T) # looks good

# compile notes
tmp <- pgD$notes
tmp[tmp==""] <- NA
tmp <- tmp[!is.na(tmp)]
tmp <- unique(tmp,MARGIN=2)
tmp <- data.frame(notes=tmp,action=NA)
write.csv(tmp,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_notes.csv"),row.names=F)
# open as a spreadsheet, fill in action column by hand

rm(rawD)

# write pgD to file
write.csv(pgD,file=paste0(here("gardens/deriveddata/"),dosite,doyear,"_growthphenology_by_plantID.csv"),row.names=F)
