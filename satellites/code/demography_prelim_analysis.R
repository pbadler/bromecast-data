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

rm(removal,control)

# effect of removal decreases with removal
control <- removal*c(0.1,0.2,0.3,0.4,0.5,0.6)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1,lty="dashed")

rm(removal,control)

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

###
### pull climate data for each site
###
# 
# # Use aridity index from CHELSA
# aridD <- read.csv("../deriveddata/aridity_index_feb2023_btclim.csv",header=T)
# 
# aridD <- aridD[,c("Site.code","Latitude","Longitude","AI","gs.AI","Alt","Ann.Prc","gs.length")]
# names(aridD)[1] <- "SiteCode"
# 
# # get resistance and resilience classifications
# rrD <- read.csv("../deriveddata/R&R_2021-2022.csv",header=T)
# names(rrD)[1] <- "SiteCode"

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

plot(mean_fecundity$mean_removal,mean_fecundity$logratio)


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

plot(fitD$fit_removal,fitD$fit_control)
abline(0,1)

fitD$fit_ratio <- fitD$fit_control - fitD$fit_removal
plot(fitD$fit_removal,fitD$fit_ratio)
