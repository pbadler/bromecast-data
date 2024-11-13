
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

tmp <- read.csv("../rawdata/SiteInfo_2023-2024.csv",header=T)
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
                          end = 2023,
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
write.csv(climD,"../deriveddata/Satellites_daymet_daily.csv",row.names=F)

###
### aggregate to climate year
###

# in case daymet daily data already acquired, load here
climD <- read.csv("../deriveddata/Satellites_daymet_daily.csv",header=T)

# set up climate seasons
climD$season <- "Win"
climD$season[climD$climDay < 92] <- "Fall"
climD$season[climD$climDay > 184 & climD$climDay < 276] <- "Spr"
climD$season[climD$climDay >= 276] <- "Sum"

# calculate daily mean temperature
climD$tavg <- (climD$tmax + climD$tmin)/2

annD <- climD %>% group_by(SiteCode,climYr,season) %>%
            summarise(prcp=sum(prcp),
                      tmean=mean(tavg),
                      swe_mean=mean(swe)) #,
                      #swe_days=sum(swe>0))
annD <- as.data.frame(annD)
annD <- reshape(annD, direction="wide",
                idvar=c("SiteCode","climYr"),
                timevar="season" )
            
# since climYr 2024 data is incomplete (only fall 2023 observations available)
# set climYr 2024 Win  Spr and Sum values to NA
tmp <- grep(".Win",names(annD))
annD[annD$climYr==2024,tmp] <- NA
tmp <- grep(".Spr",names(annD))
annD[annD$climYr==2024,tmp] <- NA
tmp <- grep("Sum",names(annD))
annD[annD$climYr==2024,tmp] <- NA

# save annual data to file
write.csv(annD,"../deriveddata/Satellites_daymet_season_means.csv",row.names=F)

rm(climD,Fa2SprD,annD,siteD,tmp)
