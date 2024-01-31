
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

# replace NAs with real zeros
# TO DO: use notes column to find and retain "real" NAs (missing plants),
# in this version, those are included as zeros
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
D2021 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag")]

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
# TO DO: use notes column to find and retain "real" NAs (missing plants),
# in this version, these may be a mix of zeros and NAs
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
D2022 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag")]

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
names(D)[which(names(D)=="Seeds..Yes.or.No.")] <- "Reproduced"
names(D)[which(names(D)=="Seeds.produced")] <- "Fecundity"
names(D)[which(names(D)=="Transect..N..E..S..or.W.")] <- "Transect"
names(D)[which(names(D)=="Distance.from.center..m.")] <- "Distance"

D$Year <- 2023

# fix bad values in Treatment column
D$Treatment[D$Treatment=="removal "] <- "removal"
D$Treatment[D$Treatment=="Removal"] <- "removal"
D$Treatment[D$Treatment=="Removal "] <- "removal"
D$Treatment[D$Treatment=="Control"] <- "control"
D$Treatment[D$Treatment=="Control "] <- "control"

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
# TO DO: use notes column to find and retain "real" NAs (missing plants),
# in this version, these may be a mix of zeros and NAs
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
D2022 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag")]

sapply(D2022, function(x) sum(is.na(x)))

### COMBINE YEARS

# # make sure siteCodes match
# print(sort(unique(D2021$SiteCode)))
# print(sort(unique(D2022$SiteCode)))

# fix one SiteCode
D2021$SiteCode[D2021$SiteCode=="SymstadS1"] <- "Symstad1"

D <- rbind(D2021,D2022)

rm(D2021,D2022)
