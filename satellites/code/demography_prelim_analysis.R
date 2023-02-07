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

# import data

D <- read.csv("../rawdata/Satellite_demography_2021-2022.csv",header=T)

print(unique(D$SiteCode))

# rename some columns
names(D)[which(names(D)=="Treatment..control.OR.removal.")] <- "Treatment"
names(D)[which(names(D)=="Emerged..Yes.or.No.")] <- "Emerged"
names(D)[which(names(D)=="Seeds..Yes.or.No.")] <- "Reproduced"
names(D)[which(names(D)=="Seeds.produced")] <- "Fecundity"

# fix bad values
D$Treatment[D$Treatment=="removal "] <- "removal"

# remove Boise sites (no harvest data)
D <- subset(D,D$SiteCode!="Boise High" & D$SiteCode!="Boise Low")

# replace NAs with real zeros
# TO DO: use notes column to find and retain "real" NAs (missing plants),
# in this version, those are included as zeros
D$Reproduced[is.na(D$Reproduced) & D$Emerged=="N"] <- "N"
D$Fecundity[is.na(D$Fecundity) & D$Reproduced=="N"] <- 0

# subset to complete observations
D <- subset(D,!is.na(D$Fecundity))

# remove one more record with 0 for fecundity but NA for reproduced
D <- subset(D,!is.na(D$Reproduced))

###
### analyze probability of producing seeds
###

# convert N/Y to 0/1
D$Reproduced <- as.numeric(as.factor(D$Reproduced))-1

prob_reprod <- D %>% group_by(SiteCode,Treatment) %>%
                summarize(mean=mean(Reproduced),sd=sd(Reproduced) )

plot(prob_reprod$mean[prob_reprod$Treatment=="control"],
     prob_reprod$mean[prob_reprod$Treatment=="removal"],
     ylab="Removal",xlab="Control", xlim=c(0,1), ylim=c(0,1),main="Probability of reproduction")
abline(0,1)

###
### model fecundity, conditional on seed production
###

fecD <- subset(D,D$Fecundity>0)

# OttS1 has no observations for removals, just 3 for controls, cut that site for now
fecD <- subset(fecD, fecD$SiteCode!="OttS1")

mean_fecundity <- fecD %>% group_by(SiteCode,Treatment) %>%
  summarize(mean=mean(log(Fecundity)),q05=quantile(log(Fecundity),0.05),q95=quantile(log(Fecundity),0.95) )

plot(mean_fecundity$mean[mean_fecundity$Treatment=="control"],
     mean_fecundity$mean[mean_fecundity$Treatment=="removal"],
     ylab="Removal",xlab="Control", xlim=c(0,4), ylim=c(0,4),main="log Fecundity")
abline(0,1)

###
### combine models
###

