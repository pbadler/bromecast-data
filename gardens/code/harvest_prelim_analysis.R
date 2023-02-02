
# start clean
rm(list=ls())

# To use relative paths, we need to set working directory to source file location 
# (this method only works on Rstudio)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) # set working directory to location of this file

library(ggplot2)
library(dplyr)
library(ggnewscale)
library(cowplot)

D <- read.csv(file = "../rawdata/Sheep_Station_CG2022_harvest_1-30-23.csv") 
D$treatment <- paste(D$Density, D$Albedo)

# get rid of dead plants (with no seeds)
D <- subset(D,D$Live=="Y")

# fill empty values in V (phenology)
D$V[D$V==""] <- "NOFLOWERS"
D$seed_count_whole[D$V=="NOFLOWERS"] <- 0

# identify subsampled plants
D$subsampled <- ifelse(is.na(D$seed_count_whole),T,F) 

# fix empty string values ("") in seed_mass_sub
D$seed_mass_sub[D$subsampled==F] <- NA
D$seed_mass_sub <- as.numeric(D$seed_mass_sub) # one string left, replaced by NA here

# scale up subsampled seed count
tmp <- which(D$subsampled==T)
D$seed_count_whole[tmp] <- D$seed_count_sub[tmp]*(D$inflor_mass[tmp]/(D$biomass_sub[tmp] + D$seed_mass_sub[tmp]))

# remove plants with no seeds
D <-  subset(D,D$seed_count_whole!=0)

# remove plants with missing seed counts
D <-  subset(D,!is.na(D$seed_count_whole))

# remove plants that dropped seeds
tmp <- grep("DROP",D$notes)
D <- D[-tmp,]
D <- subset(D, D$drop_seed=="N")

# remove plants with smut
D <- subset(D,D$notes != "smut")
D <- subset(D,D$notes != "SMUT")

# lots more cleaning to do, broken tillers, etc. ...

### look for relationships with whole samples

# biomass
plot(D$biomass_whole,D$seed_count_whole,xlab="biomass",ylab="seeds",col=as.numeric(as.factor(D$treatment)))
legend("topleft",unique(D$treatment),pch=1,col=unique(as.numeric(as.factor(D$treatment))))

mBio <- lm(seed_count_whole~biomass_whole*Density*Albedo,data=D,na.action=na.omit)
summary(mBio)
        
plot(sqrt(D$biomass_whole),sqrt(D$seed_count_whole),xlab="sqrt(biomass)",ylab="sqrt(seeds)",col=as.numeric(as.factor(D$treatment)))
legend("topleft",unique(D$treatment),pch=1,col=unique(as.numeric(as.factor(D$treatment))))

mBio_sqrt <- lm(sqrt(seed_count_whole)~I(sqrt(biomass_whole))*Density*Albedo,data=D,na.action=na.omit)
summary(mBio_sqrt)

# inflorescence
plot(D$inflor_mass,D$seed_count_whole,xlab="infl mass",ylab="seeds",col=as.numeric(as.factor(D$treatment)))
legend("topleft",unique(D$treatment),pch=1,col=unique(as.numeric(as.factor(D$treatment))))

mInfl <- lm(seed_count_whole~inflor_mass*Density*Albedo,data=D)
summary(mInfl)

plot(sqrt(D$inflor_mass),sqrt(D$seed_count_whole),xlab="sqrt(infl mass)",ylab="sqrt(seeds)",col=as.numeric(as.factor(D$treatment)))
legend("topleft",unique(D$treatment),pch=1,col=unique(as.numeric(as.factor(D$treatment))))

mInfl_sqrt <- lm(sqrt(seed_count_whole)~I(sqrt(inflor_mass))*Density*Albedo,data=D)
summary(mInfl_sqrt)

### look for relationships within subsamples
