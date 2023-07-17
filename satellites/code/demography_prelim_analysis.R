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

plot(log(removal),log(control/removal))

# effect of competition increases with fitness
control <- removal^(0.5)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1)

plot(log(removal),log(control/removal))

# effect of competition decreases with fitness
control <- removal*c(0.1,0.2,0.3,0.4,0.5,0.6)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control/removal))


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
# flag two records where Emerged and Reproduced = N and Fecundity is > 0
tmp <- which(D$Emerged=="N" & D$Reproduced=="N" & D$Fecundity > 0)
D$fecundityflag[tmp] <- 1

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

mean_fecundity$fec_logratio <- mean_fecundity$mean_logF_control - mean_fecundity$mean_logF_removal


# join site means
site_means <- merge(prob_reprod,mean_fecundity,all.x=T)

# calculate fitness on log scales
site_means$fit_control <- log(site_means$pR_control) + site_means$mean_logF_control
site_means$fit_removal <- log(site_means$pR_removal) + site_means$mean_logF_removal
site_means$fit_logratio <- site_means$fit_control - site_means$fit_removal

# set fitness NAs (caused by zero prob. reproduction) to -5 (lower than lowest log fitness)
site_means$fit_control[is.na(site_means$fit_control)] <- -5
site_means$fit_removal[is.na(site_means$fit_removal)] <- -5

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

par(tcl=-0.2,mgp=c(2,0.5,0))

# treatment effects on prob of reproduction
colvals <- col2rgb("black")
tmpcol <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
par(mar=c(3,5,3,1))
plot(site_means$pR_removal,site_means$pR_control,col=tmpcol,pch=16,
     xlab="Removal",ylab="Control", xlim=c(0,1), ylim=c(0,1),
     main="Probability of reproduction")
abline(0,1,lty="dashed")

# map mean prob of reproduction
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=site_means$Lon,y=site_means$Lat,pch=".",col="black")
symbols(x=site_means$Lon,y=site_means$Lat,circles=site_means$pR_overall,inches=0.4,add=T)

# mean prob of reproduction and climate
par(mar=c(3,5,3,1))
mycol <- ifelse(site_means$Lon < -109, "blue","red")
plot(site_means$prcp,site_means$pR_overall,pch=16, col=mycol,
     xlab="Precipitation (mm)",ylab="Prob. seed production")
legend("topleft",c("west","east"),pch=16,col=c("blue","red"))


# # paired t-test
# x <- prob_reprod$mean_control-prob_reprod$mean_removal
# summary(lm(x~1))

###
### fecundity figures
###

threshold <- 0.05
tmp <- which(site_means$pR_control >= threshold & site_means$pR_removal >= threshold)

# treatment 1:1
par(mar=c(3,5,3,1))
#mycol <- ifelse(site_means$Lon < -109, "blue","red")
plot(site_means$mean_logF_removal[tmp],site_means$mean_logF_control[tmp], col="black",
     xlab="Removal",ylab="Control", xlim=c(0,7), ylim=c(0,7),pch=16,main="log Fecundity")
abline(0,1,lty="dashed")
# removals confidence intervals
arrows(x0=site_means$q05_logF_removal[tmp],
       y0=site_means$mean_logF_control[tmp],
       x1=site_means$q95_logF_removal[tmp],
       y1=site_means$mean_logF_control[tmp],angle=90,length=0,code=3)
# controls confidence intervals
arrows(x0=site_means$mean_logF_removal[tmp],
       y0=site_means$q05_logF_control[tmp],
       x1=site_means$mean_logF_removal[tmp],
       y1=site_means$q95_logF_control[tmp],angle=90,length=0,code=3)

# map fecundity in removals
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=site_means$Lon[tmp],y=site_means$Lat[tmp],pch=".",col="black")
symbols(x=site_means$Lon[tmp],y=site_means$Lat[tmp],circles=site_means$mean_logF_removal[tmp],inches=0.4,add=T)

# mean removal fecundity and climate
par(mar=c(3,5,3,1))
mycol <- ifelse(site_means$Lon[tmp] < -109, "blue","red")
plot(site_means$prcp[tmp],site_means$mean_logF_removal[tmp],pch=16, col=mycol,
     xlab="Precipitation (mm)",ylab="log Fecundity")
legend("topright",c("west","east"),pch=16,col=c("blue","red"))

# removal fecundity vs effect of competition
mycol <- ifelse(site_means$Lon[tmp] < -109, "blue","red")
plot(site_means$mean_logF_removal[tmp],site_means$fec_logratio[tmp], 
     xlab="log Fecundity in Removals",ylab="log(Control/Removal)",pch=16,col=mycol)
abline(h=0,lty="dashed")
legend("bottomleft",c("west","east"),pch=16,col=c("blue","red"))


# not much here
pdf("SWExLongitude.pdf",height=3, width=8.5)

par(mfrow=c(1,3),mar=c(3,5,1,1),mgp=c(2,0.5,0))

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fecundty in removals")
symbols(x=site_means$Lon[tmp],y=site_means$swe_mean[tmp],circles=sqrt(exp(site_means$mean_logF_removal[tmp])),inches=0.4,add=T,fg="blue")

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fecundity in controls")
symbols(x=site_means$Lon[tmp],y=site_means$swe_mean[tmp],circles=sqrt(exp(site_means$mean_logF_control[tmp])),inches=0.4,add=T,fg="blue")

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Effect of competition")
tmp2 <- which(site_means$fec_logratio[tmp]<0)
symbols(x=site_means$Lon[tmp[tmp2]],y=site_means$swe_mean[tmp[tmp2]],
        circles=abs(site_means$fec_logratio[tmp[tmp2]]),inches=0.4,add=T,fg="red")
tmp2 <- which(site_means$fec_logratio>0)
symbols(x=site_means$Lon[tmp[tmp2]],y=site_means$swe_mean[tmp[tmp2]],
        circles=abs(site_means$fec_logratio[tmp[tmp2]]),inches=0.4,add=T,fg="blue")

dev.off()


# fitness figures


# assign color categories based on fitness outcomes
colvals <- col2rgb("blue")
tmpcol <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
mycol <-  rep(tmpcol,nrow(site_means))  # if fitness control and removal > 0
tmp <- which(site_means$fit_control<0 & site_means$fit_removal<0) 
colvals <- col2rgb("black")
tmpcol <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
mycol[tmp] <- tmpcol
tmp <- which(site_means$fit_control>0 & site_means$fit_removal<0)
colvals <- col2rgb("green4")
tmpcol <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
mycol[tmp] <- tmpcol
tmp <- which(site_means$fit_control<0 & site_means$fit_removal>0)
colvals <- col2rgb("red")
tmpcol <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
mycol[tmp] <- tmpcol

# set threshold for prob of reproduction
threshold <- 0.05
mypch <- ifelse(site_means$pR_control >= threshold & site_means$pR_removal >= threshold,16,1)

# treatment 1:1
pdf("test.pdf",height=3,width=5)
par(mar=c(3,5,3,1))
plot(site_means$fit_removal,site_means$fit_control, col=mycol,
     xlab="Removal",ylab="Control", xlim=c(-5.1,7), ylim=c(-5.1,7),pch=mypch,main="log Fitness")
abline(h=0)
abline(v=0)
abline(0,1,lty="dashed")
dev.off()

# map outcome category
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
points(x=site_means$Lon,y=site_means$Lat,col=mycol,pch=mypch,cex=2)

# mean removal fitness and climate
par(mar=c(3,5,3,1))
plot(site_means$prcp,site_means$fit_removal,pch=mypch, col=mycol,lwd=2,
     xlab="Precipitation (mm)",ylab="log Fitness")
abline(h=0,lty="dashed")

# removal fitness vs effect of competition
plot(site_means$fit_removal,site_means$fit_logratio, 
     xlab="log Fitness in Removals",ylab="log(Control/Removal)",pch=mypch,col=mycol,lwd=2)
abline(h=0,lty="dashed")


# longitude, prcp, and fitness outcome
plot(site_means$Lon,site_means$prcp,pch=mypch,col=mycol,lwd=2,
     xlab="Longitude",ylab="Precipitation (mm)")


# longitude, swe, and fitness outcome
plot(site_means$Lon,site_means$swe_mean,pch=mypch,col=mycol,lwd=2,
     xlab="Longitude",ylab="Snow water equivalent (cm)")

