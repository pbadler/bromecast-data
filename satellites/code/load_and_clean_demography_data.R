
# called from run_everything.R

###
### import 2021 data ---------------------------------------------------
###

D <- read.csv("../rawdata/Satellite_demography_2020-2021.csv",header=T)

#print(unique(D$SiteCode))

# rename some columns
names(D)[which(names(D)=="Treatment..control.OR.removal.")] <- "Treatment"
names(D)[which(names(D)=="Emerged..Y.or.No.")] <- "Emerged1"
names(D)[which(names(D)=="Emerged..Y.or.No..1")] <- "Emerged2"
names(D)[which(names(D)=="Seeds..Y.or.No.")] <- "Reproduced"
names(D)[which(names(D)=="Seeds.produced")] <- "Fecundity"
names(D)[which(names(D)=="Transect..N..E..S..or.W.")] <- "Transect"
names(D)[which(names(D)=="Distance.from.center..m.")] <- "Distance"
names(D)[which(names(D)=="Notes..Herbivore.damage..Disease..")] <- "Notes"

D$Year <- 2021

# combine emergence columns
D$Emerged <-ifelse(D$Emerged1=="Y" | D$Emerged2=="Y", "Y", "N")
# fix NAs from missing second census
D$Emerged[D$Emerged1=="N" & is.na(D$Emerged2)] <- "N"

# # fix bad values
D$Treatment[D$Treatment=="removal "] <- "removal"
D$Treatment[D$Treatment=="Removal"] <- "removal"
D$Treatment[D$Treatment=="Control"] <- "control"

D$Fecundity[D$Fecundity=="unk"] <- NA
tmp <- which(D$Fecundity=="*seed head broke")
D$Fecundity[tmp] <- NA
D$Fecundity <- as.numeric(D$Fecundity)

# replace NAs with real zeros (missing plants will be removed later based on Notes)
D$Reproduced[is.na(D$Reproduced) & D$Emerged=="N"] <- "N"
D$Fecundity[is.na(D$Fecundity) & D$Reproduced=="N"] <- 0

# fix some bad values
D$Reproduced[D$Reproduced==""] <- "N"
# fill NAs for "Reproduced" when we know fecundity > 0
tmp <- which(is.na(D$Reproduced) & D$Fecundity > 0)
D$Reproduced[tmp] <- "Y"
# fill 0's for fecundity when Reproduced==No
tmp <- which(is.na(D$Fecundity) & D$Reproduced == "N")
D$Fecundity[tmp] <- 0

# flag records where Reproduced = Y and Fecundity is NA
D$fecundityflag <- ifelse(D$Reproduced=="Y" & is.na(D$Fecundity),1,0)

# subset to complete observations
D <- subset(D,!is.na(D$Fecundity))

# remove and reorder columns
D2021 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag","Notes")]

sapply(D2021, function(x) sum(is.na(x)))

rm(D)

###
### import 2022 data ---------------------------------------------------
###

D <- read.csv("../rawdata/Satellite_demography_2021-2022.csv",header=T)

# print(unique(D$SiteCode))

# rename some columns
names(D)[which(names(D)=="Treatment..control.OR.removal.")] <- "Treatment"
names(D)[which(names(D)=="Emerged..Yes.or.No.")] <- "Emerged"
names(D)[which(names(D)=="Seeds..Yes.or.No.")] <- "Reproduced"
names(D)[which(names(D)=="Seeds.produced")] <- "Fecundity"
names(D)[which(names(D)=="Transect..N..E..S..or.W.")] <- "Transect"
names(D)[which(names(D)=="Distance.from.center..m.")] <- "Distance"
names(D)[which(names(D)=="Notes..Herbivore.damage..Disease..")] <- "Notes"

D$Year <- 2022

# fix bad values in Treatment column
D$Treatment[D$Treatment=="removal "] <- "removal"
D$Treatment[D$Treatment=="Removal"] <- "removal"
D$Treatment[D$Treatment=="Control"] <- "control"

# fix bad values in Emerged column
D$Emerged[D$Emerged=="-"] <- "missing"
D$Emerged[D$Emerged=="?"] <- "missing"
D$Emerged[D$Emerged=="No_skewer"] <- "missing"
D$Emerged[D$Emerged=="No_skewer?"] <- "missing"
D$Emerged[D$Emerged=="Unk"] <- "missing"
D$Emerged[D$Emerged=="Y?"] <- "Y"

# fix some bad values in Reproduced column
D$Reproduced[D$Emerged=="missing"] <- "missing"
D$Reproduced[D$Reproduced=="dead"] <- "N"
# fill NAs for "Reproduced" when we know fecundity > 0
tmp <- which(is.na(D$Reproduced) & D$Fecundity > 0)
D$Reproduced[tmp] <- "Y"
D$Reproduced[is.na(D$Reproduced) & D$Emerged=="N"] <- "N"
# assume remaining NAs are missing plants
D$Reproduced[is.na(D$Reproduced)] <- "missing"

# fix bad values in Fecundity column
D$Fecundity[D$Fecundity=="unk"] <- NA
tmp <- which(D$Fecundity=="*seed head broke")
D$Fecundity[tmp] <- NA
D$Fecundity <- as.numeric(D$Fecundity)
D$Fecundity[is.na(D$Fecundity) & D$Reproduced=="N"] <- 0
D$Fecundity <- round(D$Fecundity)  # a couple monster plants were subsampled, have non-integer fecundity

# flag records where Reproduced = Y and Fecundity is NA
D$fecundityflag <- ifelse(D$Reproduced=="Y" & is.na(D$Fecundity),1,0)
# flag two records where Emerged and Reproduced = N and Fecundity is > 0
tmp <- which(D$Emerged=="N" & D$Reproduced=="N" & D$Fecundity > 0)
D$fecundityflag[tmp] <- 1

# when Reproduced = missing, set Fecundity to NA (not zero)
tmp <- which(D$Reproduced=="missing" & D$Fecundity==0)
D$Fecundity[tmp] <- NA

# remove and reorder columns
D2022 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag","Notes")]

sapply(D2022, function(x) sum(is.na(x)))

rm(D)

###
### import 2023 data ---------------------------------------------------
###

D <- read.csv("../rawdata/Satellite_demography_2022-2023.csv",header=T)

# print(unique(D$SiteCode))

# rename some columns
names(D)[which(names(D)=="Treatment..control.OR.removal.")] <- "Treatment"
names(D)[which(names(D)=="Emerged..Yes.or.No.")] <- "Emerged"
names(D)[which(names(D)=="Seeds..Y.N.")] <- "Reproduced"
names(D)[which(names(D)=="Seeds.produced")] <- "Fecundity"
names(D)[which(names(D)=="Transect..N..E..S..or.W.")] <- "Transect"
names(D)[which(names(D)=="Distance.from.center..m.")] <- "Distance"
names(D)[which(names(D)=="Notes..Herbivore.damage..Disease..")] <- "Notes"

D$Year <- 2023

# fix bad values in Treatment column
D$Treatment[D$Treatment=="removal "] <- "removal"
D$Treatment[D$Treatment=="Removal"] <- "removal"
D$Treatment[D$Treatment=="Removal "] <- "removal"
D$Treatment[D$Treatment=="Control"] <- "control"
D$Treatment[D$Treatment=="Control "] <- "control"

# fix bad values in Emerged column
D$Emerged[D$Emerged=="MISSING"] <- "missing"
D$Emerged[D$Emerged=="no"] <- "N"
D$Emerged[D$Emerged=="No"] <- "N"
D$Emerged[D$Emerged=="Yes"] <- "Y"
D$Emerged[D$Emerged=="yes"] <- "Y"

# fix some bad values in Reproduced column
D$Reproduced[D$Reproduced==""] <- NA
D$Reproduced[D$Reproduced==" "] <- NA
D$Reproduced[D$Reproduced=="no"] <- "N"
D$Reproduced[D$Reproduced=="No"] <- "N"
D$Reproduced[D$Reproduced=="not found"] <- "missing"
D$Reproduced[D$Reproduced=="y"] <- "Y"
D$Reproduced[D$Reproduced=="yes"] <- "Y"
D$Reproduced[D$Reproduced=="Yes"] <- "Y"
D$Reproduced[D$Reproduced=="Y?"] <- "Y"

# fill NAs for "Reproduced" when we know fecundity > 0
tmp <- which(is.na(D$Reproduced) & D$Fecundity > 0)
D$Reproduced[tmp] <- "Y"
D$Reproduced[is.na(D$Reproduced) & D$Emerged=="N"] <- "N"
# assume remaining NAs are missing plants
D$Reproduced[is.na(D$Reproduced)] <- "missing"

# fix bad values in Fecundity column
#D$Fecundity <- as.numeric(D$Fecundity)
D$Fecundity <- round(D$Fecundity)  # a couple monster plants were subsampled, have non-integer fecundity
D$Fecundity[is.na(D$Fecundity) & D$Reproduced=="N"] <- 0

# flag records where Reproduced = Y and Fecundity is NA
D$fecundityflag <- ifelse(D$Reproduced=="Y" & is.na(D$Fecundity),1,0)
# flag one record where Emerged and Reproduced = N and Fecundity is > 0
tmp <- which(D$Emerged=="N" & D$Reproduced=="N" & D$Fecundity > 0)
D$fecundityflag[tmp] <- 1

# when Reproduced = missing, set Fecundity to NA (not zero)
tmp <- which(D$Reproduced=="missing" & D$Fecundity==0)
D$Fecundity[tmp] <- NA

# remove and reorder columns
D2023 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag","Notes")]

sapply(D2022, function(x) sum(is.na(x)))


###
### import 2024 data ---------------------------------------------------
###

D <- read.csv("../rawdata/Satellite_demography_2023-2024.csv",header=T)

# print(unique(D$site_code))

# rename some columns
names(D)[which(names(D)=="site_code")] <- "SiteCode"
names(D)[which(names(D)=="treatment")] <- "Treatment"
names(D)[which(names(D)=="emerged")] <- "Emerged"
names(D)[which(names(D)=="seeds")] <- "Reproduced"
names(D)[which(names(D)=="seeds_produced")] <- "Fecundity"
names(D)[which(names(D)=="transect")] <- "Transect"
names(D)[which(names(D)=="distance_from_center")] <- "Distance"
names(D)[which(names(D)=="notes")] <- "Notes"

D$Year <- 2024

# fix bad values in Treatment column
D$Treatment[D$Treatment=="removal "] <- "removal"
D$Treatment[D$Treatment=="Removal"] <- "removal"
D$Treatment[D$Treatment=="Removal "] <- "removal"
D$Treatment[D$Treatment=="Control"] <- "control"
D$Treatment[D$Treatment=="Control "] <- "control"

# fix bad values in Emerged column
D$Emerged[D$Emerged=="MISSING"] <- "missing"
D$Emerged[D$Emerged==""] <- "missing"
D$Emerged[D$Emerged=="no"] <- "N"
D$Emerged[D$Emerged=="No"] <- "N"
D$Emerged[D$Emerged=="Yes"] <- "Y"
D$Emerged[D$Emerged=="yes"] <- "Y"

# assume NAs for Emerged are missing plants
D$Emerged[is.na(D$Emerged)] <- "missing"

# fix some bad values in Reproduced column
D$Reproduced[D$Reproduced==""] <- NA
D$Reproduced[D$Reproduced==" "] <- NA
D$Reproduced[D$Reproduced=="no"] <- "N"
D$Reproduced[D$Reproduced=="No"] <- "N"
D$Reproduced[D$Reproduced=="not found"] <- "missing"
D$Reproduced[D$Reproduced=="y"] <- "Y"
D$Reproduced[D$Reproduced=="yes"] <- "Y"
D$Reproduced[D$Reproduced=="Yes"] <- "Y"
D$Reproduced[D$Reproduced=="Y?"] <- "Y"

# fill NAs for "Reproduced" when we know fecundity > 0
tmp <- which(is.na(D$Reproduced) & D$Fecundity > 0)
# none of these exist for 2024

# fix bad values in Fecundity column
D$Fecundity[is.na(D$Fecundity) & D$Reproduced=="N"] <- 0

# flag records where Reproduced = Y and Fecundity is NA
D$fecundityflag <- ifelse(D$Reproduced=="Y" & is.na(D$Fecundity),1,0)

# when Reproduced = missing, set Fecundity to NA (not zero)
tmp <- which(D$Reproduced=="missing" & D$Fecundity==0)
D$Fecundity[tmp] <- NA

# remove and reorder columns
D2023 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag","Notes")]

sapply(D2022, function(x) sum(is.na(x)))



### COMBINE YEARS

# # make sure siteCodes match
# print(sort(unique(D2021$SiteCode)))
# print(sort(unique(D2022$SiteCode)))
# print(sort(unique(D2023$SiteCode)))

# fix one SiteCode
D2021$SiteCode[D2021$SiteCode=="SymstadS1"] <- "Symstad1"

D <- rbind(D2021,D2022,D2023)

rm(D2021,D2022,D2023)

### take action on issues in Notes column
tmp <- data.frame("demography_notes" = sort(unique(D$Notes)))
write.csv(tmp,"../deriveddata/demography_notes_raw.csv",row.names=F)

# # look up individual notes
# tmp <- which(D$Notes=="TOOTHPICK UPROOTED")
# D[tmp,]

# to do by hand: go through demography_notes_raw.csv
# and record actions for real problems, save to
# demography_notes_actions.csv

# read in and merge notes actions
actions <- read.csv("../deriveddata/demography_notes_actions.csv",header=T)
names(actions)[1] <- "Notes"
D <- merge(D,actions,all.x=T)
D <- D[order(D$SiteCode,D$Year,D$Treatment,D$Transect),] # reorder
D <- D[,-which(names(D)=="Notes")]

# do the obvious cleaning
table(D$notesFlag)
tmp <- which(D$notesFlag=="remove")
D <- D[-tmp,]
tmp <- which(D$notesFlag=="seeddrop" | D$notesFlag=="immature" | D$notesFlag=="fecundityFlag")
D$fecundityflag[tmp] <- 1  # records that shouldn't be used for fecundity analysis (could be used for emergence and reproduction)

rm(actions)

