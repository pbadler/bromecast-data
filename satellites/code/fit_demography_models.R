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

# make Emerged 1s and 0s
myD$Emerged <- ifelse(myD$Emerged=="N",0,1)

nullE <- glmer(Emerged ~ 1 + (1|SiteCode/Transect),data=myD,family="binomial")
climE <- update(nullE, ~ . + tmean + swe_mean)
