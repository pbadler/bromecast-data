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

# subset to complete observations
D <- subset(D,!is.na(D$Fecundity))

# remove and reorder columns
D2021 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity")]

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

# fix bad values
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
D$Reproduced[D$Reproduced=="dead"] <- "N"
# fill NAs for "Reproduced" when we know fecundity > 0
tmp <- which(is.na(D$Reproduced) & D$Fecundity > 0)
D$Reproduced[tmp] <- "Y"

# subset to complete observations
D <- subset(D,!is.na(D$Fecundity))

# remove one more record with 0 for fecundity but NA for reproduced
D <- subset(D,!is.na(D$Reproduced))

# remove and reorder columns
D2022 <- D[,c("SiteCode","Year","Treatment","Transect","Distance","Emerged","Reproduced","Fecundity")]

sapply(D2022, function(x) sum(is.na(x)))


###
### import site info
###

siteD <- read.csv("../rawdata/SiteInfo_2021-2022.csv",header=T)
names(siteD)[1:3] <- c("SiteCode" ,"Lat","Lon")

# make site SiteCodes match those in the demography file
siteD$SiteCode[siteD$SiteCode=="EnsingS1_SuRDC"] <- "EnsingS1 SuRDC"
siteD$SiteCode[siteD$SiteCode=="EnsingS2_SumPrinceRd"] <- "EnsingS2 Summerland-Princeton"
siteD$SiteCode[siteD$SiteCode=="EnsingS3_BearCreek"] <- "EnsingS3 Bear Creek"
siteD$SiteCode[siteD$SiteCode=="EnsingS4_LDBM"] <- "EnsingS4 Lundbom"
siteD$SiteCode[siteD$SiteCode=="SymstadS1"] <- "Symstad1"
siteD$SiteCode[siteD$SiteCode=="SymstadS2"] <- "Symstad2"

###
### analyze probability of producing seeds
###

# convert N/Y to 0/1
D$Reproduced <- as.numeric(as.factor(D$Reproduced))-1

prob_reprod <- D %>% group_by(SiteCode,Treatment) %>%
                summarize(mean=mean(Reproduced),sd=sd(Reproduced) )

plot(prob_reprod$mean[prob_reprod$Treatment=="removal"],
     prob_reprod$mean[prob_reprod$Treatment=="control"],
     xlab="Removal",ylab="Control", xlim=c(0,1), ylim=c(0,1),main="Probability of reproduction")
abline(0,1,lty="dotted")

prob_reprod <- prob_reprod %>% pivot_wider(names_from=Treatment, values_from=c(mean, sd))

# paired t-test
x <- prob_reprod$mean_control-prob_reprod$mean_removal
summary(lm(x~1))

# # site level analysis (better to do GLM on individual data)
# # is slope different from 1?
# m1 <- lm(prob_reprod$mean[prob_reprod$Treatment=="control"] ~ prob_reprod$mean[prob_reprod$Treatment=="removal"])
# abline(m1,col="red")
# summary(m1)

###
### model fecundity, conditional on seed production
###

fecD <- subset(D,D$Fecundity>0)

# OttS1 has no observations for removals, just 3 for controls, cut that site for now
fecD <- subset(fecD, fecD$SiteCode!="OttS1")

mean_fecundity <- fecD %>% group_by(SiteCode,Treatment) %>%
  summarize(mean=mean(log(Fecundity)),q05=quantile(log(Fecundity),0.05),q95=quantile(log(Fecundity),0.95) )

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
m2 <- glm(log(Fecundity) ~ Treatment*SiteCode, data = fecD)


# play with site means
# go from long to wide
mean_fecundity <- mean_fecundity %>% pivot_wider(names_from=Treatment, values_from=c(mean, q05, q95))

mean_fecundity$logratio <- mean_fecundity$mean_control - mean_fecundity$mean_removal

plot(mean_fecundity$mean_removal,mean_fecundity$logratio, 
     xlab="log Fecundity in Removals",ylab="log(Control/Removal)")
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

names(prob_reprod)[2:3] <- paste0("pr_",names(prob_reprod)[2:3])
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
fitD <- merge(fitD,siteD[,1:3],all.x=T)

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



###
### pull climate data for each site
###
 
# Daymet means for fall through spring
climD <- read.csv("../deriveddata/Satellites_daymet_Fall2Spr_means.csv",header=T)
climD <- subset(climD, climD$climYr==2022)
# make site SiteCodes match those in the demography file
climD$SiteCode[climD$SiteCode=="EnsingS1_SuRDC"] <- "EnsingS1 SuRDC"
climD$SiteCode[climD$SiteCode=="EnsingS2_SumPrinceRd"] <- "EnsingS2 Summerland-Princeton"
climD$SiteCode[climD$SiteCode=="EnsingS3_BearCreek"] <- "EnsingS3 Bear Creek"
climD$SiteCode[climD$SiteCode=="EnsingS4_LDBM"] <- "EnsingS4 Lundbom"
climD$SiteCode[climD$SiteCode=="SymstadS1"] <- "Symstad1"
climD$SiteCode[climD$SiteCode=="SymstadS2"] <- "Symstad2"

# merge to demography site means
fitD <- merge(fitD,climD,all.x=T)

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


