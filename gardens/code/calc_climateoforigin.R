# This code calculates the the climate of origin for each genotype from the
# common garden site to use as a covariate for assessing local adaptation

# Load libraries
library(tidyverse); library(here); library(geosphere)

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
  arrange(genotype) -> genotypes_with_gps

# Read in climate data
seed_collect <- read_csv("gardens/rawdata/SeedCollectionData_wClimAndLavinBackground_23Jan2020 - SeedCollectionData_wClimAndLavinBackground_23Jan2020.csv")

seed_collect %>% 
  mutate(long = Longitude,
         lat = Latitude) -> seed_collect

seed_collect %>% 
  filter(Site.code %in% genotypes_with_gps$site_code) %>% 
  distinct() -> cg_seeds

# Also pull observations that are close to common gardens
seed_collect %>% 
  filter(Latitude %in% c(43.286047, 43.459148, 44.26, 41.18)) %>% 
  mutate(Site.code = c("SS", "CH", "BA", "WI")) -> garden_climate

# Bring these data together
rbind(cg_seeds, garden_climate) -> cg_seeds

# Select only columns that we care about
cg_seeds %>% 
  select(lon = Longitude, lat = Latitude,
         site_code = Site.code, elevation = Alt,
         Ann.Mean.Tmp:Prc.Cld.Q) -> cg_seeds

# Make column headings lowercase
names(cg_seeds) <- tolower(names(cg_seeds))

# Add genotype number to the data
genotypes_with_gps %>% 
  select(genotype, site_code) %>% 
  merge(cg_seeds, all.y = T) -> cg_genotypes_climate

# Write derived data csv
write_csv(cg_genotypes_climate, here("gardens/deriveddata/BioclimateOfOrigin_AllGenotypes.csv"))
