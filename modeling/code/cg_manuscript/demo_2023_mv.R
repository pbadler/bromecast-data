## Preliminaries ####
library(loo)
# Source data for modeling
source("modeling/code/data_prep.R")

# Subset data for faster convergence as we are trying different models
for_model %>% 
  filter(complete.cases(new_neighbors) & seed_count > 0) -> for_model_sample

# Set contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# Center and scale continuous variables, make factors, make survival response
# binary
for_model_sample$clim_dist_sc <- scale(for_model_sample$clim_dist)[,1]
# Get quadratic of non-absolute climate distance
for_model_sample$clim_dist_sc2 <- (for_model_sample$clim_dist_sc)^2
for_model_sample$clim_dist_sc3 <- (for_model_sample$clim_dist_sc)^3
for_model_sample$sqrt_new_neighbors_sc <- scale(sqrt(for_model_sample$new_neighbors))[,1]
for_model_sample$temp_fecun_sc <- scale(for_model_sample$temp_fecun)[,1]
for_model_sample$vwc_avg_sc <- scale(sqrt(for_model_sample$vwc_avg))[,1]
for_model_sample$temp_surv_sc <- scale(sqrt(for_model_sample$temp_surv))[,1]
for_model_sample$genotype <- as.factor(for_model_sample$genotype)
for_model_sample$survived <- ifelse(for_model_sample$survived == "Y", 1, 0)
for_model_sample$site_year_gravel <- as.factor(paste(for_model_sample$site,
                                                     for_model_sample$year,
                                                     for_model_sample$albedo, sep = "_"))

# Make design matrix for fixed effects
tibble(nb = for_model_sample$sqrt_new_neighbors_sc,
       temp = for_model_sample$temp_fecun_sc,
       vwc = for_model_sample$vwc_avg_sc,
       clim_dist = for_model_sample$clim_dist_sc,
       clim_dist2 = for_model_sample$clim_dist_sc2,
       clim_dist3 = for_model_sample$clim_dist_sc3) %>% 
  mutate(nb_temp = nb * temp,
         nb_vw = nb * vwc,
         temp_vwc = temp * vwc) %>% 
  as.matrix() -> X

# Create numeric identifiers for random effects
site_year_id <- as.numeric(for_model_sample$site_year_gravel)
plot_unique_id <- as.numeric(as.factor(for_model_sample$plot_unique))
genotype_id <- as.numeric(as.factor(for_model_sample$genotype))

data <- list(X = X,
             N = nrow(X),
             P = ncol(X),
             K_site = length(unique(site_year_id)),
             K_plot = length(unique(plot_unique_id)),
             K_genotype = length(unique(genotype_id)),
             site_id = site_year_id,
             plot_id = plot_unique_id,
             genotype_id = genotype_id,
             seed_count = for_model_sample$seed_count)

# Settings for STAN run
file <- file.path("/Users/meganvahsen/Git/Bromecast/cmdstanr_write_stan_file_dir/demo_model_fecun.stan")
mod <- cmdstan_model(file, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
n_cores = parallel::detectCores()
chain = 3
threads_per_chain = ceiling(n_cores/chain)

# Fit model in STAN
fit <- mod$sample(
  data = data,
  chains = chain,
  seed = 4685,
  parallel_chains = 3,
  show_messages = T,
  refresh = 10,
  iter_warmup = 500,
  threads_per_chain = threads_per_chain,
  iter_sampling = 500
)

fit$summary(variables = NULL, ~quantile(., probs = c(0.025, 0.5, 0.975)))
fit_s$summary(variables = NULL, ~quantile(., probs = c(0.025, 0.5, 0.975)))

# Fit the same model without random slopes 
file_noslopes <- file.path("/Users/meganvahsen/Git/Bromecast/cmdstanr_write_stan_file_dir/demo_model_fecun_noslopes.stan")
mod_noslopes <- cmdstan_model(file_noslopes, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
fit_noslopes <- mod_noslopes$sample(
  data = data,
  chains = chain,
  seed = 4685,
  parallel_chains = 3,
  show_messages = T,
  refresh = 10,
  iter_warmup = 1000,
  threads_per_chain = threads_per_chain,
  iter_sampling = 1000
)

# Compute the LOOs for both models to compare
full_model <- fit$loo(variables = "log_likelihood_values")

fit_noslopes <- read_rds("~/Desktop/fecundity_reduced.rds")
reduced_model <- fit_noslopes$loo(variables = "log_likelihood_values")
loo_compare(full_model, reduced_model)

# write_rds(fit, "~/Desktop/fecundity_full.rds")
# write_rds(fit_noslopes, "~/Desktop/fecundity_reduced.rds")

# Get summary of all parameters
summary = fit$summary()

# Get all posterior draws for parameters
posterior <- fit$draws()

mcmc_trace(posterior,  pars = c(paste("site_intercepts[", 1:14, "]", sep = "")))

draws <- fit$draws(format = "df")
draws_s<-fit_s$draws(format = "df")
draws %>% 
  dplyr::select(starts_with("sigma")) %>% 
  gather(key = parameter, value = value, sigma_genotype:sigma_genotype_slope_climdist3) %>% 
  group_by(parameter) %>% 
  summarize(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  arrange(-mean)

draws %>% 
  dplyr::select(contains("seed_count_pred")) %>% 
  apply(MARGIN = 1, FUN = mean) %>% hist(main = "pp check (mean)", xlab = "predicted mean")
abline(v = mean(for_model_sample$seed_count), col = "red", lwd = 2)

draws %>% 
  dplyr::select(contains("seed_count_pred")) %>% 
  apply(MARGIN = 1, FUN = sd) %>% hist(main = "pp check (sd)", xlab = "predicted sd")
abline(v = sd(for_model_sample$seed_count), col = "red", lwd = 2)

draws %>% 
  dplyr::select(contains("seed_count_pred")) %>% 
  apply(MARGIN = 1, FUN = max) %>% hist(main = "pp check (max)", xlab = "predicted mean")
abline(v = max(for_model_sample$seed_count), col = "red", lwd = 2)

## Marginal effect of neighbors ####
seq_sc_nb <- seq(min(for_model_sample$sqrt_new_neighbors_sc),
                 max(for_model_sample$sqrt_new_neighbors_sc),
                 length.out = 10)
seq_nb <- seq(min(for_model_sample$new_neighbors),
              max(for_model_sample$new_neighbors),
              length.out = 10)

store <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store[i,] <- draws$alpha + draws$`beta[1]` * seq_sc_nb[i]
}

apply(store, 1, quantile, c(0.025, 0.5, 0.975)) ->sum_stat

for_model_sample$ln_seed_count <- log(for_model_sample$seed_count)

tibble(lower = sum_stat[1,],
       upper = sum_stat[3,],
       ln_seed_count = sum_stat[2,],
       new_neighbors = seq_nb) %>% 
  ggplot(aes(x = new_neighbors, y = ln_seed_count)) +
  geom_point(data = for_model_sample, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "ln(seed count)",
       x = "number of neighbors") -> neighbors_plot

## Marginal effect of temperature ####
seq_sc_temp <- seq(min(for_model_sample$temp_fecun_sc),
                 max(for_model_sample$temp_fecun_sc),
                 length.out = 10)
seq_temp <- seq(min(for_model_sample$temp_fecun),
              max(for_model_sample$temp_fecun),
              length.out = 10)

store_temp <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_temp[i,] <- draws$alpha + draws$`beta[2]` * seq_sc_temp[i]
}

apply(store_temp, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_temp

tibble(lower = sum_stat_temp[1,],
       upper = sum_stat_temp[3,],
       ln_seed_count = sum_stat_temp[2,],
       temp_fecun = seq_temp) %>% 
  ggplot(aes(x = temp_fecun, y = ln_seed_count)) +
  geom_point(data = for_model_sample, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "ln(seed count)",
       x = "temperature °C (March - May)") -> temp_plot

## Marginal effect of vwc ####
seq_sc_vwc <- seq(min(for_model_sample$vwc_avg_sc),
                   max(for_model_sample$vwc_avg_sc),
                   length.out = 10)
seq_vwc <- seq(min(for_model_sample$vwc_avg),
                max(for_model_sample$vwc_avg),
                length.out = 10)

store_vwc <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_vwc[i,] <- draws$alpha + draws$`beta[3]` * seq_sc_vwc[i]
}

apply(store_vwc, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_vwc

tibble(lower = sum_stat_vwc[1,],
       upper = sum_stat_vwc[3,],
       ln_seed_count = sum_stat_vwc[2,],
       vwc_avg = seq_vwc) %>% 
  ggplot(aes(x = vwc_avg, y = ln_seed_count)) +
  geom_point(data = for_model_sample, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "ln(seed count)",
       x = "vwc") -> vwc_plot

## Marginal effect of clim_dist ####
seq_sc_climdist <- seq(min(for_model_sample$clim_dist_sc),
                   max(for_model_sample$clim_dist_sc),
                   length.out = 10)
seq_climdist <- seq(min(for_model_sample$clim_dist),
                max(for_model_sample$clim_dist),
                length.out = 10)

store_climdist <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_climdist[i,] <- draws$alpha + draws$`beta[4]` * seq_sc_climdist[i]
}

apply(store_climdist, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_climdist

tibble(lower = sum_stat_climdist[1,],
       upper = sum_stat_climdist[3,],
       ln_seed_count = sum_stat_climdist[2,],
       clim_dist = seq_climdist) %>% 
  ggplot(aes(x = clim_dist, y = ln_seed_count)) +
  #geom_point(data = for_model_sample, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "ln(seed count)",
       x = "climate distance") -> climdist_plot

## GxE effects temp ####

genotype_store <- matrix(NA, nrow = 10, ncol = 90)

for (j in 1:90){
  store_temp_g <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",2]", sep = "")
    intercept <- draws %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_temp_g[i,] <- draws$alpha + intercept + (draws$`beta[2]` + slope) * seq_sc_temp[i]
  }
  apply(store_temp_g, 1, mean) -> sum_stat_temp_g
  genotype_store[,j] <- sum_stat_temp_g
}

as_tibble(genotype_store) -> genotype_store_df
names(genotype_store_df) <- paste("g", 1:90, sep = "_")
genotype_store_df %>% 
  mutate(temp_fecun = seq_temp) %>% 
  gather(key = genotype, value = ln_seed_count, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype))) -> temp_g_pred

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model_sample$genotype) %>% 
  distinct() %>% 
  merge(temp_g_pred) %>% 
  dplyr::select(genotype = genotype_id, temp_fecun, ln_seed_count) ->temp_g_pred

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(temp_g_pred) -> temp_g_pred

temp_g_pred %>% 
  ggplot(aes(x = temp_fecun, y = ln_seed_count, group = genotype, color = PC1)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(seed count)",
       x = "temperature (°C)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1)  -> a_temp

temp_g_pred %>% 
  group_by(genotype, PC1) %>% 
  reframe(min = ln_seed_count[which(temp_fecun == min(temp_fecun))],
            max = ln_seed_count[which(temp_fecun == max(temp_fecun))],
          slope = (max - min) / (max(temp_fecun) - min(temp_fecun))) %>% 
  ggplot(aes(x = PC1, y = slope, color = PC1)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "PC 1\n(hot & dry → cool & wet)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> b_temp


temp_g_pred %>% 
  ggplot(aes(x = temp_fecun, y = ln_seed_count, group = genotype, color = genotype)) +
  geom_line() +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none") +
  labs(y = "ln(seed count)",
       x = "temperature °C (March - May)") -> temp_g_plot

## GxE effects vwc ####

genotype_store_vwc <- matrix(NA, nrow = 10, ncol = 90)

for (j in 1:90){
  store_vwc_g <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",3]", sep = "")
    intercept <- draws %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_vwc_g[i,] <- draws$alpha + intercept + (draws$`beta[3]` + slope) * seq_sc_vwc[i]
  }
  apply(store_vwc_g, 1, mean) -> sum_stat_vwc_g
  genotype_store_vwc[,j] <- sum_stat_vwc_g
}

as_tibble(genotype_store_vwc) -> genotype_store_vwc_df
names(genotype_store_vwc_df) <- paste("g", 1:90, sep = "_")
genotype_store_vwc_df %>% 
  mutate(vwc_avg = seq_vwc) %>% 
  gather(key = genotype, value = ln_seed_count, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype))) -> vwc_g_pred

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model_sample$genotype) %>% 
  distinct() %>% 
  merge(vwc_g_pred) %>% 
  dplyr::select(genotype = genotype_id, vwc_avg, ln_seed_count) ->vwc_g_pred
# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(vwc_g_pred) -> vwc_g_pred

vwc_g_pred %>% 
  ggplot(aes(x = vwc_avg, y = ln_seed_count, group = genotype, color = PC1)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(seed count)",
       x = "VWC") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> a

vwc_g_pred %>% 
  group_by(genotype, PC1) %>% 
  reframe(min = ln_seed_count[which(vwc_avg == min(vwc_avg))],
          max = ln_seed_count[which(vwc_avg == max(vwc_avg))],
          slope = (max - min) / (max(vwc_avg) - min(vwc_avg))) %>% 
  ggplot(aes(x = PC1, y = slope, color = PC1)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "PC 1\n(hot & dry → cool & wet)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> b

png("~/Desktop/vwc_ge_fecun.png", height = 4, width = 9.5, res = 300, units = "in")
a + b + plot_layout(guides = "collect", widths = c(2,1))
dev.off()

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model_sample$genotype) -> code_match

draws %>% 
  dplyr::select(starts_with("genotype_slopes") & ends_with(",2]")) %>%
  rename_all(~as.character(1:90)) %>% 
  gather(key = genotype, value = value, `1`:`90`) %>% 
  mutate(genotype = factor(genotype)) %>% 
  merge(code_match %>% distinct()) %>% 
  merge(vwc_g_pred %>% dplyr::select(genotype_id = genotype, PC1) %>% distinct()) %>% 
  dplyr::select(genotype_id, PC1, value) %>% 
  ggplot(aes(x = PC1, y = value, group = genotype_id)) +
  geom_violin()
 

#-> vwc_g_plot

## GxE effects neighbors ####
genotype_store_nb <- matrix(NA, nrow = 10, ncol = 90)


for (j in 1:90){
  store_nb_g <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",1]", sep = "")
    intercept <- draws %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_nb_g[i,] <- draws$alpha + intercept + (draws$`beta[1]` + slope) * seq_sc_nb[i]
  }
  apply(store_nb_g, 1, mean) -> sum_stat_nb_g
  genotype_store_nb[,j] <- sum_stat_nb_g
}

as_tibble(genotype_store_nb) -> genotype_store_nb_df
names(genotype_store_nb_df) <- paste("g", 1:90, sep = "_")
genotype_store_nb_df %>% 
  mutate(new_neighbors = seq_nb) %>% 
  gather(key = genotype, value = ln_seed_count, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype))) -> nb_g_pred

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model_sample$genotype) %>% 
  distinct() %>% 
  merge(nb_g_pred) %>% 
  dplyr::select(genotype = genotype_id, new_neighbors, ln_seed_count) ->nb_g_pred

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(nb_g_pred) -> nb_g_pred

nb_g_pred %>% 
  ggplot(aes(x = new_neighbors, y = ln_seed_count, group = genotype, color = PC1)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(seed count)",
       x = "neighbors") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> a_dens_s


## GxE effects climate distance ####
genotype_store_clim <- matrix(NA, nrow = 10, ncol = 90)
seq_sc_clim <- seq(min(for_model_sample$clim_dist_sc),
                   max(for_model_sample$clim_dist_sc), length.out = 10)
seq_clim <- seq(min(for_model_sample$clim_dist),
                   max(for_model_sample$clim_dist), length.out = 10)
for (j in 1:90){
  store_clim_g <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",4]", sep = "")
    intercept <- draws %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_clim_g[i,] <- draws$alpha + intercept + (draws$`beta[4]` + slope) * seq_sc_clim[i]
  }
  apply(store_clim_g, 1, mean) -> sum_stat_temp_g
  genotype_store_clim[,j] <- sum_stat_temp_g
}

as_tibble(genotype_store_clim) -> genotype_store_clim_df
names(genotype_store_clim_df) <- paste("g", 1:90, sep = "_")
genotype_store_clim_df %>% 
  mutate(clim_dist = seq_clim) %>% 
  gather(key = genotype, value = ln_seed_count, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype))) -> clim_g_pred

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model_sample$genotype) %>% 
  distinct() %>% 
  merge(clim_g_pred) %>% 
  dplyr::select(genotype = genotype_id, clim_dist, ln_seed_count) ->clim_g_pred

# Get in PC1 values by genotype
genotype_codes <- read_csv("~/Desktop/genotype_codes.csv")

bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(clim_g_pred) -> clim_g_pred

clim_g_pred %>% 
  filter(complete.cases(brte_cov)) %>% 
  ggplot(aes(x = clim_dist, y = ln_seed_count, group = genotype, color = brte_cov)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(seed count)",
       x = "BRTE cover",
       color = "BRTE cover") +
  scale_color_distiller(palette = "Oranges", direction = 1) -> clim_dist_plot_f 

clim_g_pred %>% 
  group_by(genotype, brte_cov) %>% 
  reframe(min = ln_seed_count[which(clim_dist == min(clim_dist))],
          max = ln_seed_count[which(clim_dist == max(clim_dist))],
          slope = (max - min) / (max(clim_dist) - min(clim_dist))) %>% 
  ggplot(aes(x = brte_cov, y = slope, color = brte_cov)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "BRTE cover", color = "BRTE cover") +
  scale_color_distiller(palette = "Oranges", direction = 1) -> b_climdist

png("~/Desktop/clim_dist_gxe.png", width = 9.5, height = 4, res = 300, units = "in")
clim_dist_plot_f + b_climdist + plot_layout(widths = c(2,1), guides = "collect")
dev.off()
## Interactive effect of neighbors and temp ####
nb_temp_store <- list()
j_set <- c(1,5,9)
for (j in 1:3){
  store_nb_temp <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_nb_temp[i,] <- draws$alpha +
      draws$`beta[1]` * seq_sc_nb[i] +
      draws$`beta[2]` * seq_sc_temp[j_set[j]] +
      draws$`beta[5]` * seq_sc_nb[i] * seq_sc_temp[j_set[j]]
  }
  apply(store_nb_temp, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_nb_temp
  nb_temp_store[[j]] <- sum_stat_nb_temp
}

quant_nb_temp <- do.call(cbind, lapply(nb_temp_store, head, 3))

tibble(lower = quant_nb_temp[1,],
       median = quant_nb_temp[2,],
       upper = quant_nb_temp[3,],
       new_neighbors = rep(seq_nb,3),
       temp_fecun = factor(round(rep(seq_temp[j_set], each = 10),2))) %>% 
  ggplot(aes(x = new_neighbors, y = median, group = temp_fecun, color = temp_fecun)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = temp_fecun), alpha = 0.2, color = NA) +
  labs(x = "number of neighbors",
       y = "ln(seed count)",
       color = "temperature °C\n(March - May)",
       fill = "temperature °C\n(March - May)") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("dodgerblue", "gray47", "brown1")) +
  scale_fill_manual(values = c("dodgerblue", "gray47", "brown1")) +
  theme(legend.position = "top")-> int_nbtemp_plot
## Interactive effect of neighbors and vwc ####
nb_vwc_store <- list()

for (j in 1:3){
  store_nb_vwc <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_nb_vwc[i,] <- draws$alpha +
      draws$`beta[1]` * seq_sc_nb[i] +
      draws$`beta[3]` * seq_sc_vwc[j_set[j]] +
      draws$`beta[6]` * seq_sc_nb[i] * seq_sc_vwc[j_set[j]]
  }
  apply(store_nb_vwc, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_nb_vwc
  nb_vwc_store[[j]] <- sum_stat_nb_vwc
}

quant_nb_vwc <- do.call(cbind, lapply(nb_vwc_store, head, 3))

tibble(lower = quant_nb_vwc[1,],
       median = quant_nb_vwc[2,],
       upper = quant_nb_vwc[3,],
       new_neighbors = rep(seq_nb,3),
       vwc_avg = factor(round(rep(seq_vwc[j_set], each = 10),2))) %>% 
  ggplot(aes(x = new_neighbors, y = median, group = vwc_avg, color = vwc_avg)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = vwc_avg), alpha = 0.2, color = NA) + 
  labs(x = "number of neighbors",
       y = "ln(seed count)",
       color = "VWC\n(March - May)",
       fill = "VWC\n(March - May)") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("tan", "gray47", "green4")) +
  scale_fill_manual(values = c("tan", "gray47", "green4")) +
  theme(legend.position = "top") -> int_nbvwc_plot


## Interactive effect of temp and vwc ####
temp_vwc_store <- list()

for (j in 1:3){
  store_temp_vwc <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_temp_vwc[i,] <- draws$alpha +
      draws$`beta[2]` * seq_sc_temp[j_set[j]] +
      draws$`beta[3]` * seq_sc_vwc[i] +
      draws$`beta[7]` * seq_sc_temp[j_set[j]] * seq_sc_vwc[i]
  }
  apply(store_temp_vwc, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_temp_vwc
  temp_vwc_store[[j]] <- sum_stat_temp_vwc
}

quant_temp_vwc <- do.call(cbind, lapply(temp_vwc_store, head, 3))

tibble(lower = quant_temp_vwc[1,],
       median = quant_temp_vwc[2,],
       upper = quant_temp_vwc[3,],
       vwc_avg = rep(seq_vwc,3),
       temp_fecun = factor(round(rep(seq_temp[j_set], each = 10), 2))) %>% 
  ggplot(aes(x = vwc_avg, y = median, group = temp_fecun, color = temp_fecun)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = temp_fecun), alpha = 0.2, color = NA)+
  labs(x = "VWC (March - May)",
       y = "ln(seed count)",
       color = "temperature °C\n(March - May)",
       fill = "temperature °C\n(March - May)") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("dodgerblue", "gray47", "brown1")) +
  scale_fill_manual(values = c("dodgerblue", "gray47", "brown1")) +
  theme(legend.position = "top") -> int_tempvwc_plot


## Quick plotting ####

# Main effects
neighbors_plot
temp_plot
vwc_plot
climdist_plot

# Interactive effects
int_nbtemp_plot
int_nbvwc_plot
int_tempvwc_plot

# GxE effects
nb_g_plot
temp_g_plot
vwc_g_plot


## Survival model fit ####
# Make design matrix for fixed effects
tibble(dens = ifelse(for_model$density == "high", -1, 1),
       temp = scale(for_model$temp_surv)[,1],
       vwc = scale(for_model$vwc_avg)[,1],
       clim_dist = scale(for_model$clim_dist)[,1],
       clim_dist2 = scale(for_model$clim_dist)[,1]^2,
       clim_dist3 = scale(for_model$clim_dist)[,1]^3) %>% 
  mutate(nb_temp = dens * temp,
         nb_vw = dens * vwc,
         temp_vwc = temp * vwc) %>% 
  as.matrix() -> Xs

# Create identifiers for random effects
site_year_ids <- as.numeric(factor(paste(for_model$site, for_model$year, for_model$albedo)))
plot_unique_ids <- as.numeric(as.factor(for_model$plot_unique))
genotype_ids <- as.numeric(as.factor(for_model$genotype))

data <- list(X = Xs,
             N = nrow(Xs),
             P = ncol(Xs),
             K_site = length(unique(site_year_ids)),
             K_plot = length(unique(plot_unique_ids)),
             K_genotype = length(unique(genotype_ids)),
             site_id = site_year_ids,
             plot_id = plot_unique_ids,
             genotype_id = genotype_ids,
             survival = ifelse(for_model$survived == "Y", 1, 0))

# Settings for STAN run
file <- file.path("/Users/meganvahsen/Git/Bromecast/cmdstanr_write_stan_file_dir/demo_model_surv.stan")
file_s <- file.path("/Users/meganvahsen/Git/Bromecast/cmdstanr_write_stan_file_dir/demo_model_surv_noslopes.stan")
mod_s <- cmdstan_model(file, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
mod_s_noslopes <- cmdstan_model(file_s, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
n_cores = parallel::detectCores()
chain = 3
threads_per_chain = ceiling(n_cores/chain)

# Fit model in STAN
fit_s <- mod_s$sample(
  data = data,
  chains = chain,
  seed = 4685,
  parallel_chains = 3,
  show_messages = T,
  refresh = 10,
  iter_warmup = 500,
  threads_per_chain = threads_per_chain,
  iter_sampling = 500
)
write_rds(fit_s_noslopes, "~/Desktop/survival_reduced.rds")
fit_s_noslopes <- mod_s_noslopes$sample(
  data = data,
  chains = chain,
  seed = 4685,
  parallel_chains = 3,
  show_messages = T,
  refresh = 10,
  iter_warmup = 1000,
  threads_per_chain = threads_per_chain,
  iter_sampling = 1000
)

# Compute the LOOs for both models to compare
full_model_s <- fit_s$loo(variables = "log_likelihood_values")
fit_s_noslopes <- read_rds("~/Desktop/survival_reduced.rds")

reduced_model_s <- fit_s_noslopes$loo(variables = "log_likelihood_values")
loo_compare(full_model_s, reduced_model_s)

# Get summary of all parameters
summary_s = fit_s$summary()

# Get all posterior draws for parameters
posterior_s <- fit_s$draws()

mcmc_trace(posterior_s,  pars = c(paste("genotype_intercepts[", 1:14, "]", sep = "")))
mcmc_trace(posterior_s, pars = c(paste("beta[", 1:7, "]", sep = "")))
## Marginal effect of density (survival) ####
draws_s <- fit_s$draws(format = "df")

stores_dens <- matrix(NA, nrow = 2, ncol = 3000)

stores_dens[1,] <- draws_s$alpha + draws_s$`beta[1]` * -1
stores_dens[2,] <- draws_s$alpha + draws_s$`beta[1]` * 1

for_model$survival <- ifelse(for_model$survived == "Y", 1, 0)

apply(stores_dens, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stats_dens

tibble(lower = plogis(sum_stats_dens[1,]),
       upper = plogis(sum_stats_dens[3,]),
       survival = plogis(sum_stats_dens[2,]),
       density = c("high", "low")) %>% 
  ggplot(aes(x = density, y = survival)) +
  geom_jitter(data = for_model, alpha = 0.3, height = 0.05, size = 0.2, shape = 16) +
  geom_errorbar(aes(xmin = density, xmax = density, ymin = lower, ymax = upper),
                width = 0.1) +
  geom_point(size = 4) +
  theme_classic(base_size = 16) +
  labs(y = "P(survival)",
       x = "density") +
  ylim(-0.1,1.1) +
  scale_y_continuous(breaks = seq(0,1,length.out = 3)) -> density_plot_s

## Marginal effect of temperature (survival) ####
for_model$temp_surv_sc <- scale(for_model$temp_surv)[,1]

seq_sc_temps <- seq(min(for_model$temp_surv_sc),
                   max(for_model$temp_surv_sc),
                   length.out = 10)
seq_temps <- seq(min(for_model$temp_surv),
                max(for_model$temp_surv),
                length.out = 10)

store_temps <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_temps[i,] <- draws_s$alpha + draws_s$`beta[2]` * seq_sc_temps[i]
}

plogis(apply(store_temps, 1, quantile, c(0.025, 0.5, 0.975))) -> sum_stat_temps

tibble(lower = sum_stat_temps[1,],
       upper = sum_stat_temps[3,],
       survival = sum_stat_temps[2,],
       temp_surv = seq_temps) %>% 
  ggplot(aes(x = temp_surv, y = survival)) +
  geom_jitter(data = for_model, alpha = 0.3, height = 0.05, size = 0.2, shape = 16) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "P(survival)",
       x = "temperature °C (Jan - May)") -> temp_plot_s

## Marginal effect of vwc (survival) ####
for_model$vwc_avg_sc <- scale(for_model$vwc_avg)[,1]

seq_sc_vwcs <- seq(min(for_model$vwc_avg_sc),
                  max(for_model$vwc_avg_sc),
                  length.out = 10)
seq_vwcs <- seq(min(for_model$vwc_avg),
               max(for_model$vwc_avg),
               length.out = 10)

store_vwcs <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_vwcs[i,] <- draws_s$alpha + draws_s$`beta[3]` * seq_sc_vwcs[i]
}

plogis(apply(store_vwcs, 1, quantile, c(0.025, 0.5, 0.975))) -> sum_stat_vwcs

tibble(lower = sum_stat_vwcs[1,],
       upper = sum_stat_vwcs[3,],
       survival = sum_stat_vwcs[2,],
       vwc_avg = seq_vwcs) %>% 
  ggplot(aes(x = vwc_avg, y = survival)) +
  geom_jitter(data = for_model, alpha = 0.3, height = 0.05, size = 0.2, shape = 16, width = 0.0005) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "P(survival)",
       x = "VWC (Jan - May)") -> vwc_plot_s

## Marginal effect of clim_dist (survival) ####
for_model$clim_dist_sc <- scale(for_model$clim_dist)[,1]

seq_sc_climdists <- seq(min(for_model$clim_dist_sc),
                       max(for_model$clim_dist_sc),
                       length.out = 10)
seq_climdists <- seq(min(for_model$clim_dist),
                    max(for_model$clim_dist),
                    length.out = 10)

store_climdists <- matrix(NA, nrow = 10, ncol = 3000)

for (i in 1:10){
  store_climdists[i,] <- draws_s$alpha + draws_s$`beta[4]` * seq_sc_climdists[i]
}

plogis(apply(store_climdists, 1, quantile, c(0.025, 0.5, 0.975))) -> sum_stat_climdists

tibble(lower = sum_stat_climdists[1,],
       upper = sum_stat_climdists[3,],
       survival = sum_stat_climdists[2,],
       clim_dist = seq_climdist) %>% 
  ggplot(aes(x = clim_dist, y = survival)) +
  geom_jitter(data = for_model, alpha = 0.3, height = 0.05, size = 0.2, shape = 16, width = 0.0005) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "purple") +
  geom_line(linewidth = 1, color = "purple") +
  theme_classic(base_size = 16) +
  labs(y = "P(survival)",
       x = "climate distance") -> climdist_plot_s

## GxE effects temp (survival) ####

alpha <- draws_s$alpha
beta_2 <- draws_s$`beta[2]`
seq_sc_temps <- seq_sc_temps

# Precompute the intercepts and slopes for all genotypes (outside of loops)
intercepts <- draws_s %>% as_tibble() %>% dplyr::select(starts_with("genotype_intercepts")) %>% as.matrix()

# We assume that genotype_slopes is a matrix with dimensions: (samples x 3*genotypes)
slopes <- draws_s %>% as_tibble() %>% dplyr::select(starts_with("genotype_slopes") & ends_with(",2]")) %>% as.matrix()

# Initialize the output matrix
genotype_stores <- matrix(NA, nrow = 10, ncol = 90)

# Vectorized computation of genotype stores
for (j in 1:90) {
  # Extract the relevant intercept and slopes for genotype j
  intercept_j <- intercepts[, j]  # Vector of intercepts for genotype j
  
  # Correct indexing to get the slopes for the 2nd predictor for genotype j
  slope_j <- slopes[, j]  # Slopes for the second predictor (2nd column) for genotype j
  
  # Precompute the entire matrix of `store_temp_gs` for each genotype in a vectorized manner
  store_temp_gs <- matrix(NA, nrow = 10, ncol = 3000)
  
  for (i in 1:10) {
    store_temp_gs[i, ] <- alpha + intercept_j + (beta_2 + slope_j) * seq_sc_temps[i]
  }
  
  # Apply colMeans to reduce the result
  genotype_stores[, j] <- rowMeans(store_temp_gs)
}


as_tibble(genotype_stores) -> genotype_store_dfs
names(genotype_store_dfs) <- paste("g", 1:90, sep = "_")
genotype_store_dfs %>% 
  mutate(temp_surv = seq_temps) %>% 
  gather(key = genotype, value = logit_survival, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype)),
         survival = plogis(logit_survival)) -> temp_g_preds


read_csv("~/Git/Bromecast-phenology/data/")

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model$genotype) %>% 
  distinct() %>% 
  merge(temp_g_preds) %>% 
  dplyr::select(genotype = genotype_id, temp_surv, survival, logit_survival) ->temp_g_preds

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(temp_g_preds) -> temp_g_preds

temp_g_preds %>% 
  ggplot(aes(x = temp_surv, y = survival, group = genotype, color = PC1)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "P(survival)",
       x = "temperature (°C)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> a_temp_s

temp_g_preds %>% 
  group_by(genotype, PC1) %>% 
  reframe(min = logit_survival[which(temp_surv == min(temp_surv))],
          max = logit_survival[which(temp_surv == max(temp_surv))],
          slope = (max - min) / (max(temp_surv) - min(temp_surv))) %>% 
  ggplot(aes(x = PC1, y = slope, color = PC1)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "PC 1\n(hot & dry → cool & wet)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> b_temp_s

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(temp_g_preds) -> temp_g_preds

temp_g_preds %>% 
  ggplot(aes(x = temp_surv, y = survival, group = genotype, color = PC1)) +
  geom_line(size = 2) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  labs(y = "P(survival)",
       x = "temperature °C (March - May)") +
  scale_color_distiller(palette = "PiYG")
#-> temp_g_plot_s

## GxE effects vwc (survival) ####
genotype_store_vwcs <- matrix(NA, nrow = 10, ncol = 90)

for (j in 1:90){
  store_vwc_gs <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",3]", sep = "")
    intercept <- draws_s %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws_s %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_vwc_gs[i,] <- draws_s$alpha + intercept + (draws_s$`beta[3]` + slope) * seq_sc_vwcs[i]
  }
  apply(store_vwc_gs, 1, mean) -> sum_stat_vwc_gs
  genotype_store_vwcs[,j] <- sum_stat_vwc_gs
}

as_tibble(genotype_store_vwcs) -> genotype_store_vwc_dfs
names(genotype_store_vwc_dfs) <- paste("g", 1:90, sep = "_")
genotype_store_vwc_dfs %>% 
  mutate(vwc_avg = seq_vwcs) %>% 
  gather(key = genotype, value = logit_survival, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype)),
         survival = plogis(logit_survival)) -> vwc_g_preds

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model$genotype) %>% 
  distinct() %>% 
  merge(vwc_g_preds) %>% 
  dplyr::select(genotype = genotype_id, vwc_avg, survival, logit_survival) ->vwc_g_preds

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(vwc_g_preds) -> vwc_g_preds

vwc_g_preds %>% 
  ggplot(aes(x = vwc_avg, y = survival, group = genotype, color = PC1)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "P(survival)",
       x = "VWC") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> a_s 

vwc_g_preds %>% 
  group_by(genotype, PC1) %>% 
  summarize(min = min(logit_survival),
            max = max(logit_survival),
            slope = (min - max)/(max(vwc_avg) - min(vwc_avg))) %>% 
  ggplot(aes(x = PC1, y = slope, color = PC1)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "PC 1\n(hot & dry → cool & wet)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> b_s

png("~/Desktop/vwc_ge_fecun.png", height = 4, width = 9.5, res = 300, units = "in")
a_s + b_s + plot_layout(guides = "collect", widths = c(2,1))
dev.off()

## GxE effects density (survival) ####
genotype_store_nbs <- matrix(NA, nrow = 2, ncol = 90)
seq_sc_nbs <- c(-1,1)

for (j in 1:90){
  store_nb_gs <- matrix(NA, nrow = 2, ncol = 3000)
  for(i in 1:2){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",1]", sep = "")
    intercept <- draws_s %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws_s %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_nb_gs[i,] <- draws_s$alpha + intercept + (draws_s$`beta[1]` + slope) * seq_sc_nbs[i]
  }
  apply(store_nb_gs, 1, mean) -> sum_stat_nb_gs
  genotype_store_nbs[,j] <- sum_stat_nb_gs
}

as_tibble(genotype_store_nbs) -> genotype_store_nb_dfs
names(genotype_store_nb_dfs) <- paste("g", 1:90, sep = "_")
genotype_store_nb_dfs %>% 
  mutate(density = c("high", "low")) %>% 
  gather(key = genotype, value = logit_survival, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype)),
         survival = plogis(logit_survival)) -> nb_g_preds

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model$genotype) %>% 
  distinct() %>% 
  merge(nb_g_preds) %>% 
  dplyr::select(genotype = genotype_id, density, survival, logit_survival) ->nb_g_preds

# Get in PC1 values by genotype
bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(nb_g_preds) -> nb_g_preds

nb_g_preds %>% 
  ggplot(aes(x = density, y = survival, group = genotype, color = PC1)) +
  geom_line(size = 1) + geom_point(size = 2) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "P(survival)",
       x = "temperature") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> b_dens_s

## GxE effects climate distance ####
genotype_store_clims <- matrix(NA, nrow = 10, ncol = 90)
seq_sc_clims <- seq(min(for_model_sample$clim_dist_sc),
                   max(for_model_sample$clim_dist_sc), length.out = 10)
seq_clims <- seq(min(for_model$clim_dist),
                max(for_model$clim_dist), length.out = 10)
for (j in 1:90){
  store_clim_gs <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    intercept_code <- paste("genotype_intercepts[", j, "]", sep = "")
    slope_code <- paste("genotype_slopes[", j, ",4]", sep = "")
    intercept <- draws_s %>% as_tibble() %>% dplyr::select(any_of(intercept_code)) %>% pull(intercept_code)
    slope <- draws_s %>% as_tibble() %>% dplyr::select(any_of(slope_code)) %>% pull(slope_code)
    store_clim_gs[i,] <- draws_s$alpha + intercept + (draws_s$`beta[4]` + slope) * seq_sc_clims[i]
  }
  apply(store_clim_gs, 1, mean) -> sum_stat_temp_g
  genotype_store_clims[,j] <- sum_stat_temp_g
}

as_tibble(genotype_store_clims) -> genotype_store_clims_df
names(genotype_store_clims_df) <- paste("g", 1:90, sep = "_")
genotype_store_clims_df %>% 
  mutate(clim_dist = seq_clim) %>% 
  gather(key = genotype, value = logit_survival, g_1:g_90) %>% 
  mutate(genotype = as.factor(parse_number(genotype)),
         survival = plogis(logit_survival)) -> clim_g_preds

tibble(genotype = factor(data$genotype_id),
       genotype_id = for_model$genotype) %>% 
  distinct() %>% 
  merge(clim_g_preds) %>% 
  dplyr::select(genotype = genotype_id, clim_dist, survival) ->clim_g_preds

# Get in PC1 values by genotype
genotype_codes <- read_csv("~/Desktop/genotype_codes.csv")

bioclim_pc %>% 
  dplyr::select(site_code = genotype, PC1) %>% 
  merge(genotype_codes) %>% 
  dplyr::select(-site_code) %>% 
  merge(clim_g_preds) -> clim_g_preds

clim_g_preds %>% 
  filter(complete.cases(brte_cov)) %>% 
  ggplot(aes(x = clim_dist, y = survival, group = genotype, color = brte_cov)) +
  geom_line(size = 1) +
  #geom_point(data = for_model_sample) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "P(survival)",
       x = "climate distance",
       color = "BRTE cover") +
  scale_color_distiller(palette = "Oranges", direction = 1) -> clim_dist_plot_s 

clim_g_preds %>% 
  group_by(genotype, brte_cov) %>% 
  reframe(min = survival[which(clim_dist == min(clim_dist))],
          max = survival[which(clim_dist == max(clim_dist))],
          slope = (max - min) / (max(clim_dist) - min(clim_dist))) %>% 
  ggplot(aes(x = brte_cov, y = slope, color = brte_cov)) + geom_point(size = 2) +
  theme_classic(base_size = 16) +
  labs(x = "BRTE cover", color = "BRTE cover") +
  scale_color_distiller(palette = "Oranges", direction = 1) -> b_climdist_s

png("~/Desktop/clim_dist_gxe_s.png", width = 9.5, height = 4, res = 300, units = "in")
clim_dist_plot_s + b_climdist_s + plot_layout(widths = c(2,1), guides = "collect")
dev.off()

## Interactive effect of density and vwc (survival) ####
nb_vwc_stores <- list()

for (j in 1:2){
  store_nb_vwcs <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_nb_vwcs[i,] <- draws_s$alpha +
      draws_s$`beta[1]` * seq_sc_nbs[j] +
      draws_s$`beta[3]` * seq_sc_vwcs[i] +
      draws_s$`beta[6]` * seq_sc_nbs[j] * seq_sc_vwcs[i]
  }
  apply(store_nb_vwcs, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_nb_vwcs
  nb_vwc_stores[[j]] <- sum_stat_nb_vwcs
}

quant_nb_vwcs <- do.call(cbind, lapply(nb_vwc_stores, head, 3))

tibble(lower = plogis(quant_nb_vwcs[1,]),
       median = plogis(quant_nb_vwcs[2,]),
       upper = plogis(quant_nb_vwcs[3,]),
       density = rep(c("high", "low"), each = 10),
       vwc_avg = rep(seq_vwcs, 2)) %>% 
  ggplot(aes(x = vwc_avg, y = median, group = density, color = density)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = density), alpha = 0.2, color = NA) +
  labs(x = "VWC",
       y = "P(survival)",
       color = "density",
       fill = "density") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("#7279ec","#22d1a9")) +
  scale_fill_manual(values = c("#7279ec","#22d1a9")) +
  theme(legend.position = "top") -> dens_vwc_plot_s
## Interactive effect of density and temp (survival) ####
nb_temp_stores <- list()

for (j in 1:2){
  store_nb_temps <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_nb_temps[i,] <- draws_s$alpha +
      draws_s$`beta[1]` * seq_sc_nbs[j] +
      draws_s$`beta[2]` * seq_sc_temps[i] +
      draws_s$`beta[5]` * seq_sc_nbs[j] * seq_sc_temps[i]
  }
  apply(store_nb_temps, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_nb_temps
  nb_temp_stores[[j]] <- sum_stat_nb_temps
}

quant_nb_temps <- do.call(cbind, lapply(nb_temp_stores, head, 3))

tibble(lower = plogis(quant_nb_temps[1,]),
       median = plogis(quant_nb_temps[2,]),
       upper = plogis(quant_nb_temps[3,]),
       density = rep(c("high", "low"), each = 10),
       temp_surv = rep(seq_temps, 2)) %>% 
  ggplot(aes(x = temp_surv, y = median, group = density, color = density)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = density), alpha = 0.2, color = NA) +
  labs(x = "temperature °C\n(Jan - May)",
       y = "P(survival)",
       color = "density",
       fill = "density") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("#7279ec","#22d1a9")) +
  scale_fill_manual(values = c("#7279ec","#22d1a9")) +
  theme(legend.position = "top")-> dens_temp_plot_s

## Interactive effect of temp and vwc (survival) ####
temp_vwc_stores <- list()

for (j in 1:3){
  store_temp_vwcs <- matrix(NA, nrow = 10, ncol = 3000)
  for(i in 1:10){
    store_temp_vwcs[i,] <- draws_s$alpha +
      draws_s$`beta[2]` * seq_sc_temps[j_set[j]] +
      draws_s$`beta[3]` * seq_sc_vwcs[i] +
      draws_s$`beta[7]` * seq_sc_temps[j_set[j]] * seq_sc_vwcs[i]
  }
  apply(store_temp_vwcs, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_temp_vwcs
  temp_vwc_stores[[j]] <- sum_stat_temp_vwcs
}

quant_temp_vwcs <- do.call(cbind, lapply(temp_vwc_stores, head, 3))

tibble(lower = plogis(quant_temp_vwcs[1,]),
       median = plogis(quant_temp_vwcs[2,]),
       upper = plogis(quant_temp_vwcs[3,]),
       vwc_avg = rep(seq_vwcs,3),
       temp_fecun = factor(round(rep(seq_temps[j_set], each = 10), 2))) %>% 
  ggplot(aes(x = vwc_avg, y = median, group = temp_fecun, color = temp_fecun)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = temp_fecun), alpha = 0.2, color = NA)+
  labs(x = "VWC (March - May)",
       y = "survival",
       color = "temperature °C\n(March - May)",
       fill = "temperature °C\n(March - May)") +
  theme_classic(base_size = 16) + 
  scale_color_manual(values = c("dodgerblue", "gray47", "brown1")) +
  scale_fill_manual(values = c("dodgerblue", "gray47", "brown1")) +
  theme(legend.position = "top") -> temp_vwc_plot_s


## Quick plotting ####

# Main effects
density_plot_s
temp_plot_s
vwc_plot_s
climdist_plot_s

# Interactive effects
dens_temp_plot_s
dens_vwc_plot_s
temp_vwc_plot_s

# GxE effects
density_g_plot_s
temp_g_plot_s
vwc_g_plot_s



## VWC effects on FITNESS ####
fitness_vwc <- exp(store_vwc) * plogis(store_vwcs)

apply(fitness_vwc, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_vwc_fit

tibble(lower = log(sum_stat_vwc_fit[1,]),
       upper = log(sum_stat_vwc_fit[3,]),
       fitness = log(sum_stat_vwc_fit[2,]),
       vwc_avg = seq_vwc) %>% 
  ggplot(aes(x = vwc_avg, y = fitness)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#f46b5e") +
  geom_line(linewidth = 1, color = "#f46b5e") +
  theme_classic(base_size = 16) +
  labs(y = "ln(fitness)",
       x = "VWC (March - May)") 

vwc_plot
vwc_plot_s

## Temperature effects on FITNESS ####
fitness_temp <- exp(store_temp) * plogis(store_temps)

apply(fitness_temp, 1, quantile, c(0.025, 0.5, 0.975)) -> sum_stat_temp_fit

tibble(lower = log(sum_stat_temp_fit[1,]),
       upper = log(sum_stat_temp_fit[3,]),
       fitness = log(sum_stat_temp_fit[2,]),
       temp_avg = seq_temp) %>% 
  ggplot(aes(x = temp_avg, y = fitness)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#f46b5e") +
  geom_line(linewidth = 1, color = "#f46b5e") +
  theme_classic(base_size = 16) +
  labs(y = "ln(fitness)",
       x = "temp (March - May)") 

temp_plot
temp_plot_s

## GxE temp effects on fitness ####
genotype_store_df %>% 
  mutate_all(exp) -> genotype_temps

