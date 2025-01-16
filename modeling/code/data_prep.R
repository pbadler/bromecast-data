## Preliminaries ####

# Load libraries
library(rjags);library(tidyverse);library(cmdstanr);library(posterior);
library(bayesplot); library(janitor); library(patchwork)

# Read in phenology and fitness data
flowerfit_wi <- read_csv("gardens/deriveddata/Boise2023_flower_fit.csv")
flowerfit_ss <- read_csv("gardens/deriveddata/SS2023_flower_fit.csv")
flowerfit_ch <- read_csv("gardens/deriveddata/CH2023_flower_fit.csv")
# Also get 2022 data
flowerfit_22 <- read_csv("gardens/deriveddata/2022_allsites_flower_harvest_cleanest.csv")

# Format Cheyenne data for demographic model
flowerfit_ch %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), jday, NA),
         year = 2023) %>% 
  dplyr::select(year, site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(year, site, block, plot, density, albedo, x, y) -> ch_for_model

# Format Sheep Station data for demographic model
flowerfit_ss %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), first_flower, NA),
         year = 2023) %>% 
  dplyr::select(year, site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(year, site, block, plot, density, albedo, x, y) -> ss_for_model

# Format Wildcat data for demographic model
flowerfit_wi %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), jday, NA),
         year = 2023) %>% 
  dplyr::select(year, site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(year, site, block, plot, density, albedo, x, y) -> wi_for_model

# Format 2022 data for demographic model
flowerfit_22 %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y"),
         plot = paste(site, block, gravel, density, sep = "_"),
         year = 2022,
         density = ifelse(density == "hi", "high", "low")) %>% 
  dplyr::select(year, site, block, plot, density, albedo = gravel, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest = v, note_standard = standard_note, note_standard_harvest = standard_note) %>% 
  arrange(year, site, block, plot, density, albedo, x, y) -> for_model_22

# Bring three site datasets together
for_model <- rbind(ss_for_model, wi_for_model, ch_for_model, for_model_22)

# Estimate seed count total before fitting model (make this within the model
# later)
for_model %>% 
  mutate(seed_count = round(exp(5.933451 + 0.935518*log(inflor_mass)))) -> for_model 

# Fix any missing seed_count values
for_model[is.na(for_model$seed_count), "seed_count"] <- 0

## Calculate neighborhood density for each plant ####

# Set possible number of neighbors for each location in high density
for_model %>%
  mutate(plot_unique = paste(site, block, plot, sep = "_")) -> for_model

for_model$possible_neighbors <- NULL
for_model$neighbors <- NULL
for_model$prop_neighbors <- NULL

for(i in 1:nrow(for_model)){
  
  if(for_model$density[i] == "low"){
    for_model[i,] %>% 
      dplyr::select(x, y) %>% 
      mutate(x_new = x + 1,
             x_new2 = x - 1,
             y_new = y + 1,
             y_new2 = y - 1) -> search_coords
    
    for_model %>% 
      filter(plot_unique == for_model$plot_unique[i]) %>% 
      filter(x == search_coords$x_new & y == search_coords$y |
               x == search_coords$x_new2 & y == search_coords$y |
               x == search_coords$x & y == search_coords$y_new  |
               x == search_coords$x & y == search_coords$y_new2 ) -> possible_neighbors
  }else{
      expand.grid(x = for_model[i,]$x + -5:5, y = for_model[i,]$y + -5:5) -> search_coords
      
    # Filter out search coords that are not within circle using distance matrix
    distances <- as.matrix(dist(cbind(search_coords$x, search_coords$y)))
    focal_coords <- which(search_coords$x == for_model$x[i] & search_coords$y == for_model$y[i])
    search_coords <- search_coords %>% 
      mutate(dist = distances[focal_coords,]) %>% 
      filter(dist <= 5)
    
    for_model %>% 
        filter(plot_unique == for_model$plot_unique[i]) %>% 
        filter(x %in% search_coords$x & y %in% search_coords$y) %>% 
        filter(x != for_model$x[i] | y != for_model$y[i]) -> possible_neighbors
  }
  
  # Calculate inverse distance
  # weights <- 1/as.matrix(dist(data.frame(x = c(for_model$x[i], possible_neighbors$x),
  #                                        y = c(for_model$y[i], possible_neighbors$y))))[2:nrow(possible_neighbors)+1,1]
  
  for_model[i, "possible_neighbors"] <- nrow(possible_neighbors)
  for_model[i, "neighbors"] <- nrow(possible_neighbors %>% filter(survived == "Y"))
  # for_model[i, "weighted_prop"] <- for_model[i, "prop_neighbors"] * mean(weights)
}

## Adjust for edge effects ####

# Get proportion that survived for each plot
for_model %>% 
  mutate(w = ifelse(survived == "Y", 1, 0)) %>% 
  group_by(plot_unique) %>% 
  summarize(prop_survived = sum(w)/n()) %>% 
  ungroup() -> plot_survival

merge(for_model, plot_survival) -> for_model

for_model %>% 
  mutate(new_neighbors = case_when(density == "low" & possible_neighbors == 3 ~ prop_survived + neighbors,
                                   density == "low" & possible_neighbors == 2 ~ prop_survived * 2 + neighbors,
                                   density == "low" & possible_neighbors == 1 ~ prop_survived * 3 + neighbors,
                                   density == "high" & site != "WI" & possible_neighbors < 80 ~ prop_survived * (80-possible_neighbors) + neighbors,
                                   density == "high" & site == "WI" & possible_neighbors < 90 ~ prop_survived * (90-possible_neighbors) + neighbors,
                                   density == "low" & possible_neighbors > 3 ~ neighbors)) -> for_model

## Calculate climate of origin difference for each genotype × site combination ####

source("modeling/code/climate_data.R")

# Merge together climate distances with rest of data
merge(for_model, clim_dists) -> for_model

# Create unique plot ID
for_model %>% 
  mutate(plot_unique = paste(site, block, plot, sep = "_")) -> for_model



## Get weather data for gravel and site for each year ####
vwc <- read_csv("modeling/data/weather_data/dailyVWCdata_allgardens_allyears.csv") %>% clean_names()
temp <- read_csv("modeling/data/weather_data/dailytempdata_allgardens_allyears.csv") %>% clean_names()

## Calculate avg vwc for each gravel × site × year combination ####
vwc %>% 
  mutate(date = mdy(date),
         site = case_when(site == "Sheep Station" ~ "SS",
                          site == "Boise Low" ~ "WI",
                          site == "Boise High" ~ "BA",
                          site == "Cheyenne" ~ "CH"),
         gravel = ifelse(graveltrt == "Black", "black", "white")) %>% 
  dplyr::select(date, site, gravel, vwc_avg) -> vwc

vwc %>% 
  filter( (date > as_date("2022-03-10") & date < as_date("2022-05-15")) |
            date > as_date("2023-03-10") & date < as_date("2023-05-15")) %>% 
  mutate(year = year(date)) %>% 
  group_by(site, gravel, year) %>% 
  summarize(vwc_avg = mean(vwc_avg)) %>% 
  ungroup() -> vwc_sub

## Calculate avg temp for each gravel × site × year combination ####
temp %>% 
  mutate(date = mdy(date),
         site = case_when(site == "Sheep Station" ~ "SS",
                          site == "Boise Low" ~ "WI",
                          site == "Boise High" ~ "BA",
                          site == "Cheyenne" ~ "CH"),
         gravel = ifelse(graveltrt == "Black", "black", "white")) %>% 
  dplyr::select(date, site, gravel, avg_temp_c) -> temp

temp %>% 
  filter(site == "SS") %>% 
  filter(date > as_date("2023-04-24") & date < as_date("2023-08-04")) %>% 
  group_by(gravel) %>% 
  summarize(mean = mean(avg_temp_c))
  

temp %>% 
  filter( (date > as_date("2022-01-01") & date < as_date("2022-05-15")) |
            date > as_date("2023-01-01") & date < as_date("2023-05-15")) %>% 
  mutate(year = year(date),
         avg_temp_c = ifelse(avg_temp_c < 0, 0, avg_temp_c)) %>% 
  group_by(site, gravel, year) %>% 
  summarize(temp_avg_surv = mean(avg_temp_c)) %>% 
  ungroup() -> temp_surv

temp %>% 
  filter( (date > as_date("2022-03-01") & date < as_date("2022-05-15")) |
            date > as_date("2023-03-01") & date < as_date("2023-05-15")) %>% 
  mutate(year = year(date),
         avg_temp_c = ifelse(avg_temp_c < 0, 0, avg_temp_c)) %>% 
  group_by(site, gravel, year) %>% 
  summarize(temp_avg_fecun = mean(avg_temp_c)) %>% 
  ungroup() -> temp_fecun

# Bring all data climate data together
site_info <- cbind(vwc_sub,
                   temp_surv = temp_surv$temp_avg_surv,
                   temp_fecun = temp_fecun$temp_avg_fecun) %>% 
  rename(albedo = gravel)

# Merge together with data
merge(for_model, site_info) -> for_model

## Remove all intermediate data objects ####

rm(list=setdiff(ls(), "for_model"))

## Extra plots ####

# png("~/Documents/Research/USU/Presentations/prop_neighbors.png", height = 8.7, width = 8.7, res = 300, units = "in")
# for_model %>%
#   filter(is.na(note_standard)) %>% 
#   filter(seed_count > 0) %>%
#   ggplot(aes(x = sqrt(new_neighbors), y = log(seed_count),
#              color = albedo)) +
#   facet_grid(site ~ year) +
#   geom_point(alpha = 0.3) +
#   scale_color_manual(values = c("black", "pink")) +
#   theme_bw(base_size = 16) +
#   geom_smooth(method = "lm", aes(fill = albedo), se = F) +
#   labs(y = "ln(number of seeds)",
#        x = "sqrt(number of neighbors)") +
#   scale_fill_manual(values = c("black", "pink"))
# dev.off()
# 
# png("~/Documents/Research/USU/Presentations/pc_dist.png", height = 8.5, width = 10, res = 300, units = "in")
# for_model %>% 
#   filter(seed_count > 0) %>% 
#   ggplot(aes(x = pc_dist_score, y = log(seed_count))) +
#   geom_point() +
#   facet_wrap(~site) +
#   geom_smooth(method = "lm") +
#   theme_bw(base_size = 16) +
#   labs(x = "PC distance from common garden",
#        y = "ln(number of seeds)") -> a
# 
# for_model %>% 
#   ggplot(aes(x = pc_dist_score,
#              y = ifelse(for_model$survived == "Y", 1, 0) )) +
#   geom_point() +
#   facet_wrap(~site) +
#   geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#   theme_bw(base_size = 16) +
#   labs(x = "PC distance from common garden",
#        y = "P(survival)") -> b
# 
# library(patchwork)
# a / b
# dev.off()

# colors <- c("#44AA99", "#AA4499", "#332288", "#6699CC")
# 
# site_info %>% 
#   ggplot(aes(x = year, y = vwc_avg, color = site, linetype = gravel)) +
#   geom_line(size = 2) +
#   geom_point(size = 6) +
#   theme_bw(base_size = 14) +
#   scale_x_continuous(breaks = 2022:2023) +
#   ylab("average vwc") +
#   ggtitle("soil moisture") +
#   scale_color_manual(values = colors)->vwc_plot
# 
# site_info %>% 
#   ggplot(aes(x = year, y = temp_surv, color = site, linetype = gravel)) +
#   geom_line(size = 2) +
#   geom_point(size = 6) +
#   theme_bw(base_size = 14) +
#   scale_x_continuous(breaks = 2022:2023) +
#   ylab("average temp (jan - may)") +
#   ggtitle("survival temp") +
#   ylim(2,15)+
#   scale_color_manual(values = colors)-> temp_surv_plot
# 
# site_info %>% 
#   ggplot(aes(x = year, y = temp_fecun, color = site, linetype = gravel)) +
#   geom_line(size = 2) +
#   geom_point(size = 6) +
#   theme_bw(base_size = 14) +
#   scale_x_continuous(breaks = 2022:2023) +
#   ylab("average temp (jan - may)") +
#   ggtitle("fecundity temp") +
#   scale_color_manual(values = colors)+
#   ylim(2,15)-> temp_fecun_plot
# 
# png("modeling/figs/weather_cov.png", height = 5, width = 14, res = 300, units = "in")
# temp_surv_plot + temp_fecun_plot + vwc_plot + plot_layout(guides = "collect")
# dev.off()
