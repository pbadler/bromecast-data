# This code calculates the the climate of origin for each genotype from the
# common garden site to use as a covariate for assessing local adaptation

# Load libraries
library(rchelsa); library(raster);library(terra); library(tidyverse)
library(tidyverse); library(here); library(geosphere); library(usmap)
library(patchwork); library(ggnewscale); library(raster)

# Read in garden gps data
garden_gps <- read_csv(here("gardens/rawdata/garden_info.csv"))

# Read in gps data for genotypes
genotype_gps <- read_csv(here("gardens/rawdata/collection_sites.csv"))
genotype_gps %>% 
  dplyr::select(Site.code = `Site code`, 
                longitude = Longitude,
                latitude = Latitude) -> genotype_gps

# Read in genotype codes
genotypes <- read_csv(here("gardens/rawdata/sitecode2genotypenumber.csv"))

# Merge together genotype gps and code data and have just one row per genotype
merge(genotype_gps, genotypes) %>% 
  dplyr::select(site_code = Site.code,
                longitude,
                latitude,
                genotypeID) %>% 
  distinct() %>% 
  mutate(genotype = parse_number(genotypeID)) %>% 
  arrange(genotype) %>% 
  select(-genotypeID) -> genotypes_with_gps

# Add common garden sites
cg_sites <- read_csv(here("gardens/rawdata/garden_info.csv"))
cg_sites %>% 
  select(site_code = garden_code,
         longitude = garden_lon,
         latitude = garden_lat) %>% 
  mutate(genotype = NA) -> sites_with_gps

# Bind together genotype info and common garden site info
all_locations <- rbind(genotypes_with_gps, sites_with_gps)

# Read in climate data
bioclim1 <- raster("~/Downloads/CHELSA_bio1_1981-2010_V.2.1 (2).tif")
bioclim2 <- raster("~/Downloads/CHELSA_bio2_1981-2010_V.2.1.tif")
bioclim3 <- raster("~/Downloads/CHELSA_bio3_1981-2010_V.2.1.tif")
bioclim4 <- raster("~/Downloads/CHELSA_bio4_1981-2010_V.2.1.tif")
bioclim5 <- raster("~/Downloads/CHELSA_bio5_1981-2010_V.2.1.tif")
bioclim6 <- raster("~/Downloads/CHELSA_bio6_1981-2010_V.2.1.tif")
bioclim7 <- raster("~/Downloads/CHELSA_bio7_1981-2010_V.2.1.tif")
bioclim8 <- raster("~/Downloads/CHELSA_bio8_1981-2010_V.2.1.tif")
bioclim9 <- raster("~/Downloads/CHELSA_bio9_1981-2010_V.2.1.tif")
bioclim10 <- raster("~/Downloads/CHELSA_bio10_1981-2010_V.2.1.tif")
bioclim11 <- raster("~/Downloads/CHELSA_bio11_1981-2010_V.2.1.tif")
bioclim12 <- raster("~/Downloads/CHELSA_bio12_1981-2010_V.2.1.tif")
bioclim13 <- raster("~/Downloads/CHELSA_bio13_1981-2010_V.2.1.tif")
bioclim14 <- raster("~/Downloads/CHELSA_bio14_1981-2010_V.2.1.tif")
bioclim15 <- raster("~/Downloads/CHELSA_bio15_1981-2010_V.2.1.tif")
bioclim16 <- raster("~/Downloads/CHELSA_bio16_1981-2010_V.2.1.tif")
bioclim17 <- raster("~/Downloads/CHELSA_bio17_1981-2010_V.2.1.tif")
bioclim18 <- raster("~/Downloads/CHELSA_bio18_1981-2010_V.2.1.tif")
bioclim19 <- raster("~/Downloads/CHELSA_bio19_1981-2010_V.2.1.tif")

# Extract values of each bioclimate variable for each of the site gps points.
# Also scale back to original scale (C).
cbind(all_locations,
      bioclim1 = raster::extract(bioclim1, all_locations[,2:3])*0.1 - 273.15,
      bioclim2 = raster::extract(bioclim2, all_locations[,2:3])*0.1,
      bioclim3 = raster::extract(bioclim3, all_locations[,2:3])*0.1,
      bioclim4 = raster::extract(bioclim4, all_locations[,2:3])*0.1,
      bioclim5 = raster::extract(bioclim5, all_locations[,2:3])*0.1 - 273.15,
      bioclim6 = raster::extract(bioclim6, all_locations[,2:3])*0.1 - 273.15,
      bioclim7 = raster::extract(bioclim7, all_locations[,2:3])*0.1,
      bioclim8 = raster::extract(bioclim6, all_locations[,2:3])*0.1 - 273.15,
      bioclim9 = raster::extract(bioclim9, all_locations[,2:3])*0.1 - 273.15,
      bioclim10 = raster::extract(bioclim10, all_locations[,2:3])*0.1 - 273.15,
      bioclim11 = raster::extract(bioclim11, all_locations[,2:3])*0.1 - 273.15,
      bioclim12 = raster::extract(bioclim12, all_locations[,2:3])*0.1,
      bioclim13 = raster::extract(bioclim13, all_locations[,2:3])*0.1,
      bioclim14 = raster::extract(bioclim14, all_locations[,2:3])*0.1,
      bioclim15 = raster::extract(bioclim15, all_locations[,2:3])*0.1,
      bioclim16 = raster::extract(bioclim16, all_locations[,2:3])*0.1,
      bioclim17 = raster::extract(bioclim17, all_locations[,2:3])*0.1,
      bioclim18 = raster::extract(bioclim18, all_locations[,2:3])*0.1,
      bioclim19 = raster::extract(bioclim19, all_locations[,2:3])*0.1) -> climate_of_origin

# Write derived data csv
write_csv(climate_of_origin, here("gardens/deriveddata/BioclimateOfOrigin_AllGenotypes.csv"))

