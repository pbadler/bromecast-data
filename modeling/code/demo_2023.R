# Fit demographic model to 2023 data

## Preliminaries ####

# Load libraries
library(rjags);library(tidyverse)

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
  sample_n(nrow(for_model)) -> for_model_sample

# Create data object with just what we need for model
for_model_sample %>% 
  mutate(w = ifelse(survived == "Y", 1, 0),
         d = as.integer(seed_count),
         plot_unique = paste(site, block, plot, sep = "_"),
         genotype = (droplevels(as.factor(genotype)))) %>% 
  select(x1 = density,
         x2 = albedo,
         x3 = site,
         w,
         d,
         plot_unique,
         genotype) -> data_jags

# Set sum to zero contrasts
options(contrasts = c("contr.sum", "contr.poly"))
x1 <- as.numeric(model.matrix(d ~ x1 * x2 + x3, data = data_jags)[,2])
x2 <- as.numeric(model.matrix(d ~ x1 * x2 + x3, data = data_jags)[,3])
x3 <- as.numeric(model.matrix(d ~ x1 * x2 + x3, data = data_jags)[,4])
x4 <- as.numeric(model.matrix(d ~ x1 * x2 + x3, data = data_jags)[,5])
x5 <- as.numeric(model.matrix(d ~ x1 * x2 + x3, data = data_jags)[,6]) 

## Fit JAGS model ####

# Create data object for JAGS
data <- list(d = data_jags$d,
             N = nrow(data_jags),
             w = data_jags$w,
             genotype = as.numeric(droplevels(as.factor(for_model$genotype))),
             nalpha = length(unique(for_model$genotype)),
             plot = as.numeric(droplevels(as.factor(data_jags$plot_unique))),
             nplot = length(unique(data_jags$plot_unique)),
             x1 = x1)

# Set parameters for MCMC
n.adapt = 500
n.update = 500
n.iter = 1000

start <- Sys.time()

# Fit model in JAGS
jm = jags.model("modeling/demo_model.R", data = data, n.chains = 3, n.adapt = n.adapt)
#update(jm, n.iter = n.update)
zm = coda.samples(jm, 
                   variable.names = c("mu.fecund.star", "mu.survive.star", 
                                      "sigma.kappa.star", "sigma.alpha.star",
                                      "sigma.psi.star", "sigma.omega.star", "beta1"), 
                   n.iter = n.iter, 
                   n.thin = 1)

end <- Sys.time()

end-start
# Plot trace and density plots
plot(zm)

## Post-processing ####

# # Calculate median and 95% quantiles for parameters
# samples <- as.data.frame(zm[[1]])
# apply(samples, 2, quantile, probs = c(0.025, 0.5, 0.975)) -> params_seed_model
# 
# data_jags %>% 
#   group_by(genotype) %>% 
#   summarize(prop = sum(w)/n()) %>% 
#   ggplot(aes(x = prop)) + 
#   geom_histogram() +
#   xlim(0,1)
# 
# data_jags %>% 
#   group_by(plot_unique) %>% 
#   summarize(prop = sum(w)/n()) %>% 
#   ggplot(aes(x = prop)) + 
#   geom_histogram() +
#   xlim(0,1)
# 
# surv_HB <- plogis(samples[,"gamma0.star"] + samples[,"gamma1"]*1 + samples[,"gamma2"]*1 + samples[,"gamma5"]*1)
# surv_LB <- plogis(samples[,"gamma0.star"] + samples[,"gamma1"]*-1 + samples[,"gamma2"]*1 + samples[,"gamma5"]*-1)
# surv_HW <- plogis(samples[,"gamma0.star"] + samples[,"gamma1"]*1 + samples[,"gamma2"]*-1 + samples[,"gamma5"]*-1)
# surv_LW <- plogis(samples[,"gamma0.star"] + samples[,"gamma1"]*-1 + samples[,"gamma2"]*-1 + samples[,"gamma5"]*1)
# 
# pred_HB <- exp(samples[,"beta0.star"] + samples[,"beta1"]*1 + samples[,"beta2"]*1 + samples[,"beta5"]*1 + samples[,"beta6"]*0.39)
# pred_LB <- exp(samples[,"beta0.star"] + samples[,"beta1"]*-1 + samples[,"beta2"]*1 + samples[,"beta5"]*-1 + samples[,"beta6"]*0.39)
# pred_HW <- exp(samples[,"beta0.star"] + samples[,"beta1"]*1 + samples[,"beta2"]*-1 + samples[,"beta5"]*-1 + samples[,"beta6"]*0.39)
# pred_LW <- exp(samples[,"beta0.star"] + samples[,"beta1"]*-1 + samples[,"beta2"]*-1 + samples[,"beta5"]*1 + samples[,"beta6"]*0.39)
# 
# tibble(treatment = rep(c("high-black", "low-black", "high-white", "low-white"), each = 5000),
#        pred = c(pred_HB*surv_HB, pred_LB*surv_LB,
#                 pred_HW*surv_HW, pred_LW*surv_LW)) %>% 
#   ggplot(aes(x = treatment, y = log(pred), fill = treatment)) +
#   geom_violin() +
#   labs(y = "log(fitness)") +
#   theme_bw(base_size = 16) +
#   scale_fill_manual(values = c("black", "white", "black", "white")) +
#   theme(legend.position = "none")
# 
# samples %>% 
#   select(contains("alpha")) %>% 
#   apply(2, mean) %>% 
#   as.numeric() %>% 
#   mean() -> alpha_mean
# 
# diff <- mean(samples[,"beta0.star"] - alpha_mean)
# 
# samples %>% 
#   select(contains("alpha")) %>% 
#   select(-"alpha[94]") %>% 
#   gather(key = "genotype", value = "value") %>% 
#   mutate("log(fecundity)" = value+diff) %>% 
#   ggplot(aes(x = reorder(genotype, `log(fecundity)`), y = `log(fecundity)`)) +
#   stat_summary(fun.y = mean,fun.min = min, fun.max = max) +
#   geom_hline(aes(yintercept = mean(samples[,"beta0.star"]))) + 
#   xlab("genotype")  +
#   theme_bw(base_size = 16) +
#   theme(axis.text.x = element_blank())
# 
# samples%>% 
#   select(contains("beta")) %>% 
#   gather(key = "param", value = "value") %>% 
#   ggplot(aes(x = value)) +
#   geom_density() +
#   facet_wrap(~param, scales = "free")
# 
# samples%>% 
#   select(contains("gamma")) %>% 
#   gather(key = "param", value = "value") %>% 
#   ggplot(aes(x = value)) +
#   geom_density() +
#   facet_wrap(~param, scales = "free")
#   
# surv_CH <- plogis(samples[,"gamma0.star"] + samples[,"gamma3"]*1 + samples[,"gamma4"]*0)
# surv_WI <- plogis(samples[,"gamma0.star"] + samples[,"gamma3"]*-1 + samples[,"gamma4"]*-1)
# surv_SS <- plogis(samples[,"gamma0.star"] + samples[,"gamma3"]*0 + samples[,"gamma4"]*1)
# 
# pred_CH <- exp(samples[,"beta0.star"] + samples[,"beta3"]*1 + samples[,"beta4"]*0)
# pred_WI <- exp(samples[,"beta0.star"] + samples[,"beta3"]*-1 + samples[,"beta4"]*-1)
# pred_SS <- exp(samples[,"beta0.star"] + samples[,"beta3"]*0 + samples[,"beta4"]*1)
# 
# tibble(site = rep(c("Cheyenne", "Wildcat", "Sheep Station"), each = 5000),
#        pred = c(pred_CH*surv_CH, pred_WI*surv_WI, pred_SS*surv_SS)) %>% 
#   ggplot(aes(x = site, y = log(pred))) +
#   geom_violin() +
#   labs(y = "log(fitness)") +
#   theme_bw(base_size = 16) 
# 
