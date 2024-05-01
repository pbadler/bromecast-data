# UNFINISHED, being developed in new repository May 1 2024 
# call from "run_everything.R," otherwise the data won't be loaded

library(lme4)

# choose which data set / functional types to use
myD <- allD_ft1

### analyze probability of emergence

# check data
table(myD$Emerged)
sum(is.na(myD$Emerged))

# remove missing data
myD <- subset(myD, myD$Emerged!="missing")

pairs(siteD[,c("prcp","tmean","swe_mean")])

# make Emerged 1s and 0s
myD$Emerged <- ifelse(myD$Emerged=="N",0,1)

nullE <- glmer(Emerged ~ 1 + (1|SiteCode),data=myD,family="binomial")
climE <- update(nullE, ~ . + prcp*tmean*swe_mean)

## analyze prob of reproduction conditional on emergence

tmp <- which(myD$Emerged==1)
myReprD <- myD[tmp,]

table(myReprD$Reproduced)
sum(is.na(myReprD$Reproduced))

myReprD <- subset(myReprD, myReprD$Reproduced!="missing")

# make Reproduced 1s and 0s
myReprD$Reproduced <- ifelse(myReprD$Reproduced=="N",0,1)

nullR <- glmer(Reproduced ~ 1 + (1|SiteCode),data=myReprD,family="binomial")
climR <- update(nullR, ~ . + prcp*tmean*swe_mean)

