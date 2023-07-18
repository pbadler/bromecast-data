
# call from run_everything.R
###
### import site info -------------------------------------------------------------------
###

# year 2021
siteD1 <- read.csv("../rawdata/SiteInfo_2020-2021.csv",header=T)
names(siteD1)[1:3] <- c("SiteCode" ,"Lat","Lon")
siteD1$Year = 2021

# make site SiteCodes match those in the demography file
siteD1$SiteCode[siteD1$SiteCode=="SymstadS1"] <- "Symstad1"

# year 2022
siteD <- read.csv("../rawdata/SiteInfo_2021-2022.csv",header=T)
names(siteD)[1:3] <- c("SiteCode" ,"Lat","Lon")
siteD$Year = 2022

# make site SiteCodes match those in the demography file
siteD$SiteCode[siteD$SiteCode=="EnsingS1_SuRDC"] <- "EnsingS1 SuRDC"
siteD$SiteCode[siteD$SiteCode=="EnsingS2_SumPrinceRd"] <- "EnsingS2 Summerland-Princeton"
siteD$SiteCode[siteD$SiteCode=="EnsingS3_BearCreek"] <- "EnsingS3 Bear Creek"
siteD$SiteCode[siteD$SiteCode=="EnsingS4_LDBM"] <- "EnsingS4 Lundbom"
siteD$SiteCode[siteD$SiteCode=="SymstadS1"] <- "Symstad1"
siteD$SiteCode[siteD$SiteCode=="SymstadS2"] <- "Symstad2"

# combine years into one data frame
siteD <- rbind(siteD1[,c("SiteCode","Lat","Lon","Year")],siteD[,c("SiteCode","Lat","Lon","Year")])

#remove Lehnoff sites
tmp <- grep("LEHN",siteD$SiteCode)
siteD <- siteD[-tmp,]

rm(siteD1,tmp)


###
### pull climate data for each site
###

# Daymet means for fall through spring
climD <- read.csv("../deriveddata/Satellites_daymet_Fall2Spr_means.csv",header=T)

# make site SiteCodes match those in the demography file
climD$SiteCode[climD$SiteCode=="EnsingS1_SuRDC"] <- "EnsingS1 SuRDC"
climD$SiteCode[climD$SiteCode=="EnsingS2_SumPrinceRd"] <- "EnsingS2 Summerland-Princeton"
climD$SiteCode[climD$SiteCode=="EnsingS3_BearCreek"] <- "EnsingS3 Bear Creek"
climD$SiteCode[climD$SiteCode=="EnsingS4_LDBM"] <- "EnsingS4 Lundbom"
climD$SiteCode[climD$SiteCode=="SymstadS1"] <- "Symstad1"
climD$SiteCode[climD$SiteCode=="SymstadS2"] <- "Symstad2"

# merge to site info
names(climD)[which(names(climD)=="climYr")] <- "Year"
siteD <- merge(siteD,climD,all.x=T)

rm(climD)
