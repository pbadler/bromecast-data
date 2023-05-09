# This code calculates the distance of a genotype from the common garden site to
# use as a covariate for assessing local adaptation

## Preliminaries ####
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

# Read in plant ID info
ids <- read_csv(here("gardens/deriveddata/SS2022_plantID.csv"))

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
  mutate(Site.code = c("SS", "WY", "BA", "WI")) -> garden_climate

# Bring these data together
rbind(cg_seeds, garden_climate) -> cg_seeds

library(usmap)
state <- map_data("state")
canada <- map_data("world") %>% filter(region == "Canada")

region <- rbind(state, canada)

ggplot(data=region, aes(x=long, y=lat, group = group)) +
  geom_polygon(color = "white", fill = "gray")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(ylim = c(34, 51), xlim = c(-125, -102)) +
  geom_point(data = seed_collect, aes(x = long, y = lat, group = NA, color = Alt)) +
  theme_classic(base_size = 14) +
  geom_point(data = garden_gps, aes(x = garden_lon, y = garden_lat, group = NA), color = "orange")

# Create matrices of tmin, tmax, and prec
cg_seeds %>% 
  dplyr::select(contains("tmin")) %>% 
  as.matrix() -> tmin_matrix

cg_seeds %>% 
  dplyr::select(contains("tmax")) %>% 
  as.matrix() -> tmax_matrix

cg_seeds %>% 
  dplyr::select(prec_1, prec_2, prec_3, prec_4, prec_5, prec_6,
                prec_7, prec_8, prec_9, prec_10, prec_11, prec_12) %>% 
  as.matrix() -> prec_matrix

biovars(prec = prec_matrix, tmin = tmin_matrix, tmax = tmax_matrix) -> bioclims

# Run PCA on bioclimatic variables

# First center and scale all bioclimatic variables
bioclims_scaled <- apply(bioclims, 2, scale)

pca_out <- prcomp(bioclims_scaled)

# Get percent explained
round(pca_out$sdev^2 / sum(pca_out$sdev^2),3)

arrange(as.data.frame(pca_out$rotation[,1:2]), PC2)

cbind(cg_seeds, pca_out$x) -> all_clim

all_clim %>%
  dplyr::select(Site.code, PC1, PC2) %>%
  arrange(PC2)

all_clim %>% 
  mutate(color_code = ifelse(Site.code %in% c("SS", "WI", "WY", "BA"), "common garden", "collection site")) %>% 
  ggplot(aes(x = PC1, y = PC2, color = color_code, shape = color_code, size = color_code)) +
  geom_point(alpha = 0.8) +
  xlab("PC 1 (37% variation)") +
  ylab("PC 2 (25% variation)") +
  theme_bw(base_size = 14) +
  scale_size_manual(values = c(3,5)) +
  scale_shape_manual(values = c(16, 8)) +
  scale_color_manual(values = c("black", "orange")) +
  labs(color = "", shape = "", size = "") +
  theme(legend.position = "top", legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) -> climate_space

png("~/Desktop/climatespace.png", height = 5, width = 5, units = "in", res = 300)
climate_space
dev.off()

# Get PC values for the four common garden sites
all_clim %>% 
  filter(Site.code %in% c("SS", "WY", "BA", "WI")) %>% 
  dplyr::select(Site.code, PC1, PC2) -> cg_climate

ggplot(data=region, aes(x=long, y=lat, group = group)) +
  geom_polygon(color = "white", fill = "gray")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(ylim = c(34, 51), xlim = c(-125, -100.5)) +
  geom_point(data = merge(all_clim, genotype_gps),
             aes(x = longitude, y = latitude, group = NA, fill = PC1),
             color = "black", shape = 21, size = 4) +
  theme_classic(base_size = 14) +
  scale_fill_distiller() +
  labs(x = "longitude", y = "latitude") -> pc1

ggplot(data=region, aes(x=long, y=lat, group = group)) +
  geom_polygon(color = "white", fill = "gray")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(ylim = c(34, 51), xlim = c(-125, -100.5)) +
  geom_point(data = merge(all_clim, genotype_gps),
             aes(x = longitude, y = latitude, group = NA, fill = PC2),
             color = "black", shape = 21, size = 4) +
  theme_classic(base_size = 14) +
  scale_fill_distiller(palette = "RdPu") +
  labs(x = "longitude", y = "latitude") -> pc2

png("~/Desktop/maps.png", height = 6, width = 11, res = 300, units = "in")
pc1+pc2
dev.off()

# Pull PC data for first 3 axes and line up with genotype data
cg_seeds %>% 
  ungroup() %>% 
  mutate(pc1 = pca_out$x[,1],
         pc2 = pca_out$x[,2],
         pc3 = pca_out$x[,3]) %>% 
  # Merge back with genotype data
  merge(genotypes) %>%  
  mutate(genotype = parse_number(genotypeID)) %>% 
  dplyr::select(genotype, pc1, pc2) %>% 
  arrange(genotype) -> genotype_PCclimate

# Add climate distances
genotype_PCclimate %>% 
  mutate(pc1_SS = abs(cg_climate[1,2] - pc1),
         pc2_SS = abs(cg_climate[1,3] - pc2),
         pc1_WY = abs(cg_climate[2,2] - pc1),
         pc2_WY = abs(cg_climate[2,3] - pc2),
         pc1_BA = abs(cg_climate[3,2] - pc1),
         pc2_BA = abs(cg_climate[3,3] - pc2),
         pc1_WI = abs(cg_climate[4,2] - pc1),
         pc2_WI = abs(cg_climate[4,3] - pc2)) -> genotype_PCclimate

# Remove all objects except climate data
rm(list=setdiff(ls(), "genotype_PCclimate"))
