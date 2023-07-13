# super minimal cleaning, just trying 
# to get a look at removal effects on 
# fecundity

# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

# load packages
library(dplyr)
library(tidyr)
library(maps)

# confirm log approach works when competition has 
# multiplicative effect on performance 

# constant proportional effect
removal <- c(1,2,4,8,16,32)
control <- removal/2

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="red")
abline(0,1)

# effect of removal increases with removal
control <- removal^(0.5)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1)

# effect of removal decreases with removal
control <- removal*c(0.1,0.2,0.3,0.4,0.5,0.6)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1,lty="dashed")

rm(removal,control)

###
### import 2021 data ---------------------------------------------------
###

D <- read.csv("../rawdata/Satellite_demography_2020-2021.csv",header=T)

print(unique(D$SiteCode))

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
# D$Treatment[D$Treatment=="removal "] <- "removal"
# D$Treatment[D$Treatment=="Removal"] <- "removal"
# D$Treatment[D$Treatment=="Control"] <- "control"

#D$Fecundity[D$Fecundity=="unk"] <- NA
#tmp <- which(D$Fecundity=="*seed head broke")
#D$Fecundity[tmp] <- NA
#D$Fecundity <- as.numeric(D$Fecundity)

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

print(unique(D$SiteCode))

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

# flag records where Reproduced = Y and Fecundity is NA
D$fecundityflag <- ifelse(D$Reproduced=="Y" & is.na(D$Fecundity),1,0)

# remove and reorder columns
D2022 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity","fecundityflag")]

sapply(D2022, function(x) sum(is.na(x)))

### COMBINE YEARS

# make sure siteCodes match
print(sort(unique(D2021$SiteCode)))
print(sort(unique(D2022$SiteCode)))

# fix one SiteCode
D2021$SiteCode[D2021$SiteCode=="SymstadS1"] <- "Symstad1"

D <- rbind(D2021,D2022)

rm(D2021,D2022)


###
### probability of producing seeds -- get site means
###

# Get rid of missing records
D <- subset(D,Reproduced!="missing")

# convert N/Y to 0/1
D$Reproduced <- as.numeric(as.factor(D$Reproduced))-1

prob_reprod <- D %>% group_by(SiteCode,Year,Treatment) %>%
                summarize(mean=mean(Reproduced)) %>% 
                pivot_wider(names_from=Treatment, names_prefix="pR_",values_from=c(mean))
prob_reprod$pR_overall <- (prob_reprod$pR_control + prob_reprod$pR_removal)/2
                               
###
###  calculate site means for fecundity
###

fecD <- subset(D,D$Fecundity>0 & D$fecundityflag==0)

mean_fecundity <- fecD %>% group_by(SiteCode,Year,Treatment) %>%
  summarize(mean=mean(log(Fecundity)),
            q05=quantile(log(Fecundity),0.05),
            q95=quantile(log(Fecundity),0.95) )  %>% 
  pivot_wider(names_from=Treatment, names_prefix="logF_",values_from=c(mean, q05, q95))

# join site means
site_means <- merge(prob_reprod,mean_fecundity,all.x=T)


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

# merge to site means
site_means <- merge(siteD,site_means)

###
### Figures
###

par(tcl=0.2,mgp=c(2,0.5,0))

# treatment effects on prob of reproduction
par(mar=c(3,5,3,1))
plot(site_means$pR_removal,site_means$pR_control,
     xlab="Removal",ylab="Control", xlim=c(0,1), ylim=c(0,1),
     main="Probability of reproduction")
abline(0,1,lty="dashed")

# map mean prob of reproduction
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=site_means$Lon,y=site_means$Lat,pch=".",col="black")
symbols(x=site_means$Lon,y=site_means$Lat,circles=site_means$pR_overall,inches=0.4,add=T)

# mean prob of reproduction and climate
mycol <- ifelse(site_means$Lon < -109, "blue","red")
plot(site_means$prcp,site_means$pR_overall,pch=16, col=mycol,
     xlab="Precipitation (mm)",ylab="Prob. seed production")
legend("topleft",c("west","east"),pch=16,col=c("blue","red"))


# # paired t-test
# x <- prob_reprod$mean_control-prob_reprod$mean_removal
# summary(lm(x~1))










plot(mean_fecundity$mean[mean_fecundity$Treatment=="removal"],
     mean_fecundity$mean[mean_fecundity$Treatment=="control"],
     xlab="Removal",ylab="Control", xlim=c(0,7), ylim=c(0,7),pch=16,main="log Fecundity")
# removals confidence intervals
arrows(x0=mean_fecundity$q05[mean_fecundity$Treatment=="removal"],
       y0=mean_fecundity$mean[mean_fecundity$Treatment=="control"],
       x1=mean_fecundity$q95[mean_fecundity$Treatment=="removal"],
       y1=mean_fecundity$mean[mean_fecundity$Treatment=="control"],angle=90,length=0,code=3)
# controls confidence intervals
arrows(x0=mean_fecundity$mean[mean_fecundity$Treatment=="removal"],
       y0=mean_fecundity$q05[mean_fecundity$Treatment=="control"],
       x1=mean_fecundity$mean[mean_fecundity$Treatment=="removal"],
       y1=mean_fecundity$q95[mean_fecundity$Treatment=="control"],angle=90,length=0,code=3)


abline(0,1,lty="dotted")

# fixed effects approach
m2 <- glm(Fecundity ~ Treatment, family="poisson",data = fecD)
summary(m2)

# play with site means
# go from long to wide

mean_fecundity$logratio <- mean_fecundity$mean_control - mean_fecundity$mean_removal

plot(mean_fecundity$mean_removal,mean_fecundity$logratio, 
     xlab="log Fecundity in Removals",ylab="log(Control/Removal)",pch=16)
abline(h=0,lty="dashed")

# # link site means to aridity data
# mean_fecundity <- merge(mean_fecundity, aridD, all.x=T)
# 
# # try a few plots
# plot(mean_fecundity$gs.AI, mean_fecundity$mean_removal, pch=16, col="blue")
# points(mean_fecundity$gs.AI, mean_fecundity$mean_control, pch=16, col="red")
# 
# plot(mean_fecundity$gs.AI, mean_fecundity$logratio)
# 
# ## look at R & R classes
# mean_fecundity <- merge(mean_fecundity,rrD,all.x=T)
# plot(as.numeric(as.factor(mean_fecundity$RR_Class)),mean_fecundity$logratio)

###
### combine models
###

names(prob_reprod)[3:4] <- paste0("pr_",names(prob_reprod)[3:4])
fitD <- merge(prob_reprod, mean_fecundity)

# combine on log scale
fitD$fit_control <- log(fitD$pr_mean_control) + fitD$mean_control
fitD$fit_removal <- log(fitD$pr_mean_removal) + fitD$mean_removal

plot(fitD$fit_removal,fitD$fit_control,xlab="Removals",ylab="Controls",main="Fitness")
abline(0,1,lty="dashed")

fitD$fit_ratio <- fitD$fit_control - fitD$fit_removal
plot(fitD$fit_removal,fitD$fit_ratio,xlab="log Fitness in Removals",
     pch=16, ylab="Effect of competition")
abline(h=0,lty="dashed")








###
### plot on map
###

# merge Lat lons
fitD <- merge(fitD,siteD,all.x=T)

par(mfrow=c(1,2))

# removal fecundity
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=fitD$Lon,y=fitD$Lat,col="red")
#symbols(x=fitD$Lon,y=fitD$Lat,circles=fitD$mean_removal,inches=0.4,add=T)
symbols(x=fitD$Lon,y=fitD$Lat,circles=sqrt(exp(fitD$fit_removal)),inches=0.4,add=T)

# log ratio control/removal
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("log(Control/Removal)")
tmp <- which(fitD$logratio<0)
symbols(x=fitD$Lon[tmp],y=fitD$Lat[tmp],circles=abs(fitD$logratio[tmp]),inches=0.4,add=T,fg="red")
tmp <- which(fitD$logratio>0)
symbols(x=fitD$Lon[tmp],y=fitD$Lat[tmp],circles=fitD$logratio[tmp],inches=0.4,add=T,fg="blue")




png("abiotic_drivers.png",height=3,width=8,res=400,units="in")
par(mfrow=c(1,3),mar=c(5,2,1,1),oma=c(4,2,0,0),
    cex.lab=1.2)
plot(fitD$prcp,fitD$fit_removal,pch=16,
     xlab="Precipitation",ylab="")
plot(fitD$tmean,fitD$fit_removal,pch=16,
     xlab="Mean temperature",ylab="")
plot(fitD$swe_mean,fitD$fit_removal,pch=16,
     xlab="Mean snow water equivalent",ylab="")
mtext("log Fitness in removals",2,outer=T,line=0.5,cex=0.8)
dev.off()


par(mfrow=c(1,1))
plot(fitD$prcp/fitD$tmean,fitD$fit_removal)

plot(fitD$swe_mean,fitD$fit_removal)
symbols(x=fitD$swe_mean,y=fitD$prcp,circles=fitD$fit_removal+1,inches=0.4,add=T,fg="blue")

## this one is promising
pdf("SWExLongitude.pdf",height=3, width=8.5)

par(mfrow=c(1,3),mar=c(3,5,1,1),mgp=c(2,0.5,0))

plot(fitD$Lon,fitD$swe_mean,xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fitness in removals")
symbols(x=fitD$Lon,y=fitD$swe_mean,circles=sqrt(exp(fitD$fit_removal)),inches=0.4,add=T,fg="blue")

plot(fitD$Lon,fitD$swe_mean,xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fitness in controls")
symbols(x=fitD$Lon,y=fitD$swe_mean,circles=sqrt(exp(fitD$fit_control)),inches=0.4,add=T,fg="blue")

plot(fitD$Lon,fitD$swe_mean,xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Effect of competition")
tmp <- which(fitD$fit_ratio<0)
symbols(x=fitD$Lon[tmp],y=fitD$swe_mean[tmp],circles=abs(fitD$fit_ratio[tmp]),inches=0.4,add=T,fg="red")
tmp <- which(fitD$fit_ratio>0)
symbols(x=fitD$Lon[tmp],y=fitD$swe_mean[tmp],circles=fitD$fit_ratio[tmp],inches=0.4,add=T,fg="blue")

dev.off()



