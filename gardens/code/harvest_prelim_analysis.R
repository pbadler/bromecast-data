
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


### look for relationships with whole samples

plot(D$biomass_whole,D$seed_count_whole)
plot(sqrt(D$biomass_whole),sqrt(D$seed_count_whole))

plot(D$inflor_mass,D$seed_count_whole)
plot(sqrt(D$inflor_mass),sqrt(D$seed_count_whole))

# try again with cleaner data set
D_clean <- subset(D, D$drop_seed=="N")

plot(D_clean$biomass_whole,D_clean$seed_count_whole)
plot(sqrt(D_clean$biomass_whole),sqrt(D_clean$seed_count_whole))

plot(D$inflor_mass,D$seed_count_whole)
plot(sqrt(D_clean$inflor_mass),sqrt(D_clean$seed_count_whole))

