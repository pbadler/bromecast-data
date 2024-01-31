
library(daymetr)
library(dplyr)
library(tidyr)

###
### Pull daymet data for satellite sites
###

# import lat lons
siteD <- read.csv("../rawdata/SiteInfo_2021-2022.csv",header=T)
siteD <- siteD[,1:3]
names(siteD) <- c("SiteCode" ,"Lat","Lon")

tmp <- read.csv("../rawdata/SiteInfo_2020-2021.csv",header=T)
tmp <- tmp[,1:3]
names(tmp) <- c("SiteCode" ,"Lat","Lon")

siteD <- rbind(siteD,tmp)

tmp <- read.csv("../rawdata/SiteInfo_2022-2023.csv",header=T)
tmp <- tmp[,1:3]
names(tmp) <- c("SiteCode" ,"Lat","Lon")

siteD <- rbind(siteD,tmp)

# remove duplicates
siteD <- unique(siteD, MARGIN=2)
tmp <- which(siteD$SiteCode=="SymstadS1" & siteD$Lat==43.35620) # remove SymstadS1 w/ bad coords
siteD <- siteD[-tmp,]

# drop Lehnoff sites (no data)
tmp <- grep("LEHN", siteD$SiteCode)
siteD <- siteD[-tmp,]

# this takes a few minutes
for(i in 1:nrow(siteD)){
  tmp <- download_daymet(site = siteD$SiteCode[i],
                          lat = siteD$Lat[i],
                          lon = siteD$Lon[i],
                          start = 1980,
                          end = 2022,
                          internal = TRUE)
  tmp$data$SiteCode <- siteD$SiteCode[i]
  
  if(i==1){
    climD <- tmp$data
  }else{
    climD <- rbind(climD,tmp$data)
  }
  
}

# move SiteCode to first column
climD <- climD[,c(10,1:9)]

# define climate year and climate day
climD$climYr <- ifelse(climD$yday > 273, climD$year+1,climD$year )
climD$climDay <- ifelse(climD$yday > 273, climD$yday-273,climD$yday+(365-273))

# reorder columns
climD <- climD[,c(1:3,11,12,4:10)]

# rename columns
names(climD)[6:12] <- c("daylength","prcp","radiation","swe","tmax","tmin","vp")

# save daily data to file (~ 30 MB)
# write.csv(climD,"../deriveddata/Satellites_daymet_daily.csv",row.names=F)

###
### aggregate to climate year
###

# only consider fall - spring
Fa2SprD <- subset(climD,climD$climDay < 270)

# calculate daily mean temperature
Fa2SprD$tavg <- (Fa2SprD$tmax + Fa2SprD$tmin)/2

annD <- Fa2SprD %>% group_by(SiteCode,climYr) %>%
            summarise(prcp=sum(prcp),
                      tmean=mean(tavg),
                      swe_mean=mean(swe),
                      swe_days=sum(swe>0))

# save annual data to file
write.csv(annD,"../deriveddata/Satellites_daymet_Fall2Spr_means.csv",row.names=F)

rm(climD,Fa2SprD,annD,siteD,tmp)
