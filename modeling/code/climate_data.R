# Load libraries
library(tidyverse); library(here); library(stringi)
library(prism); library(raster); library(geosphere); library(sf);
library(dismo)

# Read GPS data for all genotypes from main Bromecast repository
gps <- read_csv("gardens/deriveddata/BioclimateOfOrigin_AllGenotypes.csv") %>% 
  dplyr::select(lon, lat, site_code) 

# Set download folder for PRISM data
prism_set_dl_dir("modeling/data/climate_data/")

## Genotype information ####

# Download climate normals 
# get_prism_normals(type = "ppt",
#                  mon = 1:12,
#                  resolution = "4km",
#                  keepZip = F)

# Get tmin, tmax, and ppt climate normals
to_slice <- prism_archive_ls()
stacked <- pd_stack(to_slice)
proj4string(stacked) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
df <- data.frame(rasterToPoints(stacked))

# Get gps of just genotypes
`%notin%` <- Negate(`%in%`)

# Get closest prism point to each GPS point
store <- matrix(NA, nrow = nrow(gps), ncol = length(to_slice))

for(i in 1:nrow(gps)){
  out <- distm(gps[i,c("lon", "lat")], df[,1:2], fun = distHaversine)
  store[i,] <- as.numeric(df[which.min(out),3:(length(to_slice)+2)])
}

as_tibble(biovars(prec = store[8:nrow(store),1:12],
                  tmax = store[8:nrow(store),13:24],
                  tmin = store[8:nrow(store),25:36])) %>% 
  mutate(genotype = gps$site_code[8:nrow(gps)]) %>% 
  dplyr::select(genotype, everything()) %>% 
  # Remove site level (because we are going to put weather here)
  filter(genotype %notin% c("BA", "CH", "WI", "SS")) -> climate_norms_gen

## Common garden site information ####

# Download daily weather data for entire experimental period

# Set download folder for PRISM data
prism_set_dl_dir("modeling/data/weather_data/")

# get_prism_monthlys(type = "ppt",
#                  year = 2021:2023,
#                  keepZip = F)

# Get tmin, tmax, and ppt monthly data
to_slice <- prism_archive_ls()
stacked <- pd_stack(to_slice)
proj4string(stacked) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
df <- data.frame(rasterToPoints(stacked))

gps_sites <- gps %>% filter(site_code %in% c("BA", "SS", "CH", "WI"))

# Get closest prism point to each GPS point
store <- matrix(NA, nrow = nrow(gps_sites), ncol = length(to_slice))

for(i in 1:nrow(gps_sites)){
  out <- distm(gps_sites[i,c("lon", "lat")], df[,1:2], fun = distHaversine)
  store[i,] <- as.numeric(df[which.min(out),3:(length(to_slice)+2)])
}

colnames(store) <- paste(rep(rep(2021:2023, each = 12),3),
                      rep(1:12, 9),
                      rep(c("ppt", "tmax", "tmin"), each = 36), sep = "_")

month_years_23 <- paste(rep(c(2023,2022), c(7,5)), 1:12, sep = "_")
month_years_22 <- paste(rep(c(2022,2021), c(7,5)), 1:12, sep = "_")

ppt_23 <- store[, paste(month_years_23, "ppt", sep = "_")]
tmax_23 <- store[, paste(month_years_23, "tmax", sep = "_")]
tmin_23 <- store[, paste(month_years_23, "tmin", sep = "_")]

ppt_22 <- store[, paste(month_years_22, "ppt", sep = "_")]
tmax_22 <- store[, paste(month_years_22, "tmax", sep = "_")]
tmin_22 <- store[, paste(month_years_22, "tmin", sep = "_")]

as_tibble(biovars(prec = ppt_22,
                  tmax = tmax_22,
                  tmin = tmin_22)) %>% 
  mutate(genotype = paste(gps_sites$site_code, "22", sep = "_")) %>% 
  dplyr::select(genotype, everything()) -> weather_22

as_tibble(biovars(prec = ppt_23,
                  tmax = tmax_23,
                  tmin = tmin_23)) %>% 
  mutate(genotype = paste(gps_sites$site_code, "23", sep = "_")) %>% 
  dplyr::select(genotype, everything()) -> weather_23

## PCA of climate and weather data ####

# Bind together genotype climate and site × year weather data
all_clim <- rbind(climate_norms_gen, weather_22, weather_23)

# Center and scale bioclimatic covariates
all_clim[,2:ncol(all_clim)] <- apply(all_clim[,2:ncol(all_clim)], 2, scale)

# Run PCA
pca_out <- prcomp(all_clim[,2:ncol(all_clim)])

# Get percent explained by each PC axis
round(pca_out$sdev^2 / sum(pca_out$sdev^2),3) -> perc_explained

# Bind PC axis data with original data
cbind(all_clim, pca_out$x) -> bioclim_pc

# Calculate PC distance between genotypes/site × year combos
#pc1_pc2 <- bioclim_pc %>% dplyr::select(pc1 = PC1, pc2 = PC2)
pc1 <- bioclim_pc %>% dplyr::select(pc1 = PC1) %>% pull(pc1)
# # Make distance matrix based on pc1 and pc2 values
# dist_matrix <- as.matrix(dist(pc1))
# 
# Figure out which columns are for the different sites
which(bioclim_pc$genotype %in% c("BA_22", "CH_22",
                                 "SS_22", "WI_22",
                                 "BA_23", "CH_23",
                                 "SS_23", "WI_23")) -> site_ids
# 
# # Bring together data frame of genotypes and site specific distances
# bioclim_pc %>% 
#   mutate(BA_22 = dist_matrix[,site_ids[1]],
#          CH_22 = dist_matrix[,site_ids[2]],
#          SS_22 = dist_matrix[,site_ids[3]],
#          WI_22 = dist_matrix[,site_ids[4]],
#          BA_23 = dist_matrix[,site_ids[5]],
#          CH_23 = dist_matrix[,site_ids[6]],
#          SS_23 = dist_matrix[,site_ids[7]],
#          WI_23 = dist_matrix[,site_ids[8]]) %>% 
#   dplyr::select(genotype, BA_22, CH_22, SS_22, WI_22,
#                 BA_23, CH_23, SS_23, WI_23) %>% 
#   filter(genotype %notin% c("BA_22", "CH_22", "SS_22", "WI_22",
#                             "BA_23", "CH_23", "SS_23", "WI_23")) %>% 
#   rename(site_code = genotype) -> genotypes_pcs

bioclim_pc %>%
  filter(genotype %notin% c("BA_22", "CH_22", "SS_22", "WI_22",
                            "BA_23", "CH_23", "SS_23", "WI_23")) %>%
  mutate(BA_22 = PC1 - pc1[site_ids[1]],
         CH_22 = PC1 - pc1[site_ids[2]],
         SS_22 = PC1 - pc1[site_ids[3]],
         WI_22 = PC1 - pc1[site_ids[4]],
         BA_23 = PC1 - pc1[site_ids[5]],
         CH_23 = PC1 - pc1[site_ids[6]],
         SS_23 = PC1 - pc1[site_ids[7]],
         WI_23 = PC1 - pc1[site_ids[8]]) %>%
  dplyr::select(genotype, BA_22, CH_22, SS_22, WI_22,
                BA_23, CH_23, SS_23, WI_23) %>%
  rename(site_code = genotype) -> genotypes_pcs


# Read in genotype code info
genotype_codes <- read_csv("~/Git/Bromecast/gardens/deriveddata/BioclimateOfOrigin_AllGenotypes.csv")
genotype_codes %>% 
  dplyr::select(site_code, genotype) -> genotype_codes

merge(genotype_codes, genotypes_pcs) -> merged_genotype_dists

merged_genotype_dists %>% 
  gather(key = site_year, value = clim_dist, `BA_22`:`WI_23`) %>% 
  mutate(year = 2000 + parse_number(site_year),
         site = substr(site_year, 1, 2)) %>% 
  dplyr::select(site, year, genotype, clim_dist) -> clim_dists

# Remove BA 2023 because we don't have data from that year x site
clim_dists %>% 
  filter(site != "BA" | year != 2023) -> clim_dists

# Remove all data except clim_dists
#rm(list=setdiff(ls(), "clim_dists"))
  