
###
###  Assemble complete data set
###

# import formatted growth and phenology data
pgD <- read.csv(paste0("../deriveddata/",dosite,doyear,"_growthphenology_by_plantID.csv"),header=T)

# import and join individual plant info
tmp <- read.csv(paste0("../deriveddata/",dosite,doyear,"_plantID.csv"),header=T)
pgD <- merge(pgD,tmp)

# import and join treatment data
tmp <- read.csv(paste0("../rawdata/garden_treatments.csv"),header=T)
names(tmp)[which(names(tmp)=="garden")] <- "site"  # rename 
tmp <- tmp[,-which(names(tmp)=="cum_plot")]  # drop column
pgD <- merge(pgD,tmp)

# import and join flags
tmp <- read.csv(paste0("../deriveddata/",dosite,doyear,"_flags.csv"),header=T)
pgD <- merge(pgD,tmp)

rm(tmp)

# cut missing plants and plants with suspicious positions
sum(!is.na(pgD$missing_plant))  # no missing plants in this data set
sum(!is.na(pgD$bad_position)) 
pgD[which(!is.na(pgD$bad_position)),] # these do look bad!
pgD <- subset(pgD,is.na(pgD$bad_position))

###
### Analyze time to flowering
###

emergD <- group_by(pgD,plantID) %>% summarise(emerged=sum(live=="Y"))
emergD$emerged <- ifelse(emergD$emerged<1,0,1)

# get treatment info and relevant flags for each plant
tmp <- unique(pgD[,c("site","year","plantID","block","plot","density",
              "gravel","genotype","growout","emergence_date","frostheave_date")],
              MARGIN=2)

emergD <- merge(emergD,tmp)

# remove plants affected by frostheave BEFORE germination
drop <- which(emergD$frostheave_date<emergD$emergence_date)
emergD <- emergD[-drop,]

# total emergence probabiliy
sum(emergD$emerged)/nrow(emergD)

# do simple glm, no random effects (plot within block should be included)
m1 <- glm(emerged ~ gravel*density, family="binomial",data=emergD)
summary(m1)

rm(emergD)

###
### Analyze time to flowering
###

# Only work with plants that made it to "FG" (flowering green) 
# Not sure how to handle plants that emerged and survived but never flowered

library(lme4) # should switch to mgcv

# get list of plants that made it to FG
tmp <- pgD[,c("plantID","v")]
tmp <- tmp[tmp$v=="FG",]
tmp <- unique(tmp, MARGIN=2)

# subset full data set to these plants
keep <- which(pgD$plantID %in% tmp$plantID)
flowerD <- pgD[keep,]

# keep earliest flowering data
flowerD <- flowerD %>% group_by(plantID) %>% summarise(flowerday = min(jday[v=="FG"],na.rm=T))

# get treatment info and relevant flags for each plant
tmp <- unique(pgD[,c("site","year","plantID","block","plot","density",
                     "gravel","genotype","growout","resurrection_date", "pheno_regress",    
                     "growth_regress_mm", "herbivory_date", "frostheave_date")],
              MARGIN=2)

flowerD <- merge(flowerD,tmp)

flowerD$genotype <- as.factor(flowerD$genotype)

# drop plants that "resurrect" before flowering
drop <- which(flowerD$flowerday>flowerD$resurrection_date)
flowerD <- flowerD[-drop,]

# do linear model, ignoring the rest of the flags
m1 <- lmer(flowerday ~ gravel*density + (1| block) + (1|genotype),data=flowerD)
summary(m1)
VarCorr(m1)

### repeat, with extremely conservative data set, drop all flags
drop <- which(flowerD$pheno_regress<0 | !is.na(flowerD$herbivory_date) |
                !is.na(flowerD$frostheave_date) )
flowerD2 <- flowerD[drop,]  # lose almost half the data
m2 <- lmer(flowerday ~ gravel*density + (1| block) + (1|genotype),data=flowerD2)
summary(m2)
VarCorr(m2)

### A less extreme approach to filtering

# Let's ignore frostheaving--if it killed the plant, plant wouldn't
# be in the flowering dataset.
# Next, drop records that reverse 2 or more phenophases
drop <- which(flowerD$pheno_regress<1) 
flowerD3 <- flowerD[-drop,]
# Next, drop records where herbivory occurs 
# AND large growth regression occurs (e.g., serious herbivory)
drop <- which(!is.na(flowerD3$herbivory_date) &  flowerD3$growth_regress_mm < -15 )
flowerD3 <- flowerD3[-drop,]
m3 <- lmer(flowerday ~ gravel*density + (1|block) + (1|genotype),data=flowerD3)
summary(m3)
VarCorr(m3)

