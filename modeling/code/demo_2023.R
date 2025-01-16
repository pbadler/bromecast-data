# Fit demographic model to 2023 data

## Preliminaries ####

# Load libraries
library(rjags);library(tidyverse);library(cmdstanr);library(posterior);library(bayesplot)

# Read in phenology and fitness data
flowerfit_wi <- read_csv("gardens/deriveddata/Boise2023_flower_fit.csv")
flowerfit_ss <- read_csv("gardens/deriveddata/SS2023_flower_fit.csv")
flowerfit_ch <- read_csv("gardens/deriveddata/CH2023_flower_fit.csv")

# Format Cheyenne data for demographic model
flowerfit_ch %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), jday, NA)) %>% 
  select(site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(site, block, plot, density, albedo, x, y) -> ch_for_model

# Format Sheep Station data for demographic model
flowerfit_ss %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), first_flower, NA)) %>% 
  select(site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(site, block, plot, density, albedo, x, y) -> ss_for_model

# Format Wildcat data for demographic model
flowerfit_wi %>% 
  mutate(survived = ifelse(is.na(biomass_whole) | biomass_whole == 0, "N", "Y")) %>% 
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), jday, NA)) %>% 
  select(site, block, plot, density, albedo, x, y, genotype, survived, first_flower,
         biomass_whole, inflor_mass, v_harvest, note_standard, note_standard_harvest) %>% 
  arrange(site, block, plot, density, albedo, x, y) -> wi_for_model

# Bring three site datasets together
for_model <- rbind(ss_for_model, wi_for_model, ch_for_model)

# Estimate seed count total before fitting model (make this within the model
# later)
for_model %>% 
  mutate(seed_count = round(exp(5.933451 + 0.935518*log(inflor_mass)))) -> for_model 

# Fix any missing seed_count values
for_model[is.na(for_model$seed_count), "seed_count"] <- 0

# Subset data for faster convergence as we are trying different models
for_model %>% 
  sample_n(nrow(for_model)*0.2) -> for_model_sample

# Create unique ID for plot
for_model_sample$plot_unique <- droplevels(as.factor(paste(for_model_sample$site,
                                                           for_model_sample$block,
                                                           for_model_sample$plot,
                                                           sep = "_")))

## Fit STAN model ####

# Create data object for STAN
data <- list(d = as.integer(for_model_sample$seed_count),
             N = nrow(for_model_sample),
             w = ifelse(for_model_sample$survived == "Y", 1, 0),
             genotype = as.numeric(droplevels(as.factor(for_model_sample$genotype))),
             nalpha = length(unique(for_model$genotype)),
             plot = as.numeric(for_model_sample$plot_unique),
             nplot = length(unique(for_model_sample$plot_unique)),
             x1 = as.numeric(as.factor(for_model_sample$density))-1,
             x2 = as.numeric(as.factor(for_model_sample$albedo))-1,
             x3 = ifelse(for_model_sample$site == "CH", 1, 0),
             x4 = ifelse(for_model_sample$site == "WI", 1, 0))

# Settings for STAN run
file <- file.path("/Users/meganvahsen/Git/Bromecast/cmdstanr_write_stan_file_dir/demo_model.stan")
mod <- cmdstan_model(file, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
n_cores = parallel::detectCores()
chain = 3
threads_per_chain = ceiling(n_cores/chain)

# Fit model in STAN
fit <- mod$sample(
  data = data,
  chains = chain,
  seed = 345,
  parallel_chains = 3,
  show_messages = T,
  refresh = 5,
  iter_warmup = 500,
  threads_per_chain = threads_per_chain,
  iter_sampling = 2000
)

# Get summary of all parameters
summary = fit$summary()

summary %>% 
  filter(grepl("beta", variable))

# Get all posterior draws for parameters
posterior <- fit$draws()

# Set color scheme for plots
color_scheme_set("viridisB")

# Trace plots for specific parameters
mcmc_trace(posterior,  pars = c("alpha[1]"), n_warmup = 10)

# Which parameters have rhat greater than 1.10. Most of these are the parameters
# that we are not tracking due to the post sweeping of random effects from Ogle
# et al.

dplyr::filter(summary, rhat >= 1.10) %>% 
  print(n = Inf)

## Post-processing ####

draws <- fit$draws(format = "df")

draws %>% 
  mutate(SS_black_hi = mu_fecund_star,
         SS_black_lo = mu_fecund_star + beta1,
         SS_white_hi = mu_fecund_star + beta2,
         SS_white_lo = mu_fecund_star + beta1 + beta2 + beta5,
         CH_black_hi = mu_fecund_star + beta3,
         CH_black_lo = mu_fecund_star + beta1 + beta3 + beta6,
         CH_white_hi = mu_fecund_star + beta2 + beta3 + beta8,
         CH_white_lo = mu_fecund_star + beta1 + beta2 + beta3 + beta5 + beta6 + beta8 + beta10,
         WI_black_hi = mu_fecund_star + beta4,
         WI_black_lo = mu_fecund_star + beta1 + beta4 + beta7,
         WI_white_hi = mu_fecund_star + beta2 + beta4 + beta9,
         WI_white_lo = mu_fecund_star + beta1 + beta2 + beta4 + beta5 + beta7 + beta9 + beta11) %>% 
  select(SS_black_hi, SS_black_lo, SS_white_hi, SS_white_lo,
         CH_black_hi, CH_black_lo, CH_white_hi, CH_white_lo,
         WI_black_hi, WI_black_lo, WI_white_hi, WI_white_lo) %>% 
  gather(key = treatment, value = ln_fecundity) %>% 
  mutate(site = substr(treatment, 1, 2),
         gravel = substr(treatment, 4, 8),
         density = substr(treatment, 10, 11)) -> pred_means_fecundity
  
pred_means_fecundity %>% 
  ggplot(aes(x = density, y = ln_fecundity, fill = gravel)) +
  geom_violin() +
  facet_wrap(~site, nrow = 3) +
  theme_bw(base_size = 16) +
  ylab("ln(fecundity)") +
  scale_fill_manual(values = c("black", "white")) -> fecundity_plot

draws %>% 
  mutate(SS_black_hi = mu_survive_star,
         SS_black_lo = mu_survive_star + gamma1,
         SS_white_hi = mu_survive_star + gamma2,
         SS_white_lo = mu_survive_star + gamma1 + gamma2 + gamma5,
         CH_black_hi = mu_survive_star + gamma3,
         CH_black_lo = mu_survive_star + gamma1 + gamma3 + gamma6,
         CH_white_hi = mu_survive_star + gamma2 + gamma3 + gamma8,
         CH_white_lo = mu_survive_star + gamma1 + gamma2 + gamma3 + gamma5 + gamma6 + gamma8 + gamma10,
         WI_black_hi = mu_survive_star + gamma4,
         WI_black_lo = mu_survive_star + gamma1 + gamma4 + gamma7,
         WI_white_hi = mu_survive_star + gamma2 + gamma4 + gamma9,
         WI_white_lo = mu_survive_star + gamma1 + gamma2 + gamma4 + gamma5 + gamma7 + gamma9 + gamma11) %>% 
  select(SS_black_hi, SS_black_lo, SS_white_hi, SS_white_lo,
         CH_black_hi, CH_black_lo, CH_white_hi, CH_white_lo,
         WI_black_hi, WI_black_lo, WI_white_hi, WI_white_lo) %>% 
  gather(key = treatment, value = logit_survive) %>% 
  mutate(site = substr(treatment, 1, 2),
         gravel = substr(treatment, 4, 8),
         density = substr(treatment, 10, 11)) -> pred_means_survive

pred_means_survive %>% 
  ggplot(aes(x = density, y = plogis(logit_survive), fill = gravel)) +
  geom_violin() +
  facet_wrap(~site, nrow = 3) +
  theme_bw(base_size = 16) +
  ylab("P(survive)") +
  scale_fill_manual(values = c("black", "white")) -> survival_plot

# Get fitness data
cbind(pred_means_fecundity, logit_survive = pred_means_survive$logit_survive) %>% 
  mutate(fitness = exp(ln_fecundity)*plogis(logit_survive)) %>% 
  ggplot(aes(x = density, y = log(fitness), fill = gravel)) +
  geom_violin() +
  facet_wrap(~site, nrow = 3) +
  theme_bw(base_size = 16) +
  ylab("ln(fitness)") +
  scale_fill_manual(values = c("black", "white")) -> fitness_plot

library(patchwork)

survival_plot + fecundity_plot + fitness_plot + plot_layout(guides = "collect") 

# Genotype main effects

# Genotype-level fecundity
draws %>% 
  select(contains("alpha_star[")) %>% 
  gather(key = genotype, value = value) %>% 
  mutate(genotype = parse_number(genotype)) %>% 
  ggplot(aes(x = reorder(genotype, value), y = value)) +
  geom_violin()+
  ylim(-3,3)

# Plot-level fecundity
draws %>% 
  select(contains("kappa_star[")) %>% 
  gather(key = plot, value = value) %>% 
  mutate(plot = parse_number(plot)) %>%
  ggplot(aes(x = reorder(plot, value), y = value)) +
  geom_violin() +
  ylim(-3,3)

# Genotype-level survival
draws %>% 
  select(contains("psi[")) %>% 
  gather(key = genotype, value = value) %>% 
  mutate(genotype = parse_number(genotype)) %>% 
  ggplot(aes(x = reorder(genotype, value), y = value)) +
  geom_violin() +
  ylim(-4.5,5.)

# Plot-level survival
draws %>% 
  select(contains("omega[")) %>% 
  gather(key = genotype, value = value) %>% 
  mutate(genotype = parse_number(genotype)) %>% 
  ggplot(aes(x = reorder(genotype, value), y = value)) +
  geom_violin() +
  ylim(-4.5,4.5)

draws %>% 
  select(sigma_alpha, sigma_kappa, sigma_psi, sigma_omega) %>% 
  apply(2, quantile)

draws %>% 
  select(sigma_alpha, sigma_kappa, sigma_psi, sigma_omega) %>% 
  gather(key = parameter, value = value) %>% 
  ggplot(aes(x = parameter, y = 1/sqrt(value))) +
  geom_violin()

draws %>% 
  select(contains("omega[")) %>% 
  names()
