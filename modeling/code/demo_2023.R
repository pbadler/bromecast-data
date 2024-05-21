# Fit demographic model to 2023 data

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
  mutate(first_flower = ifelse(v %in% c("FG", "FP", "FB"), jday, NA)) %>% 
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

# Create data object with just what we need for model
for_model %>% 
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


# Create data object for JAGS
data <- list(d = data_jags$d,
             N = nrow(data_jags),
             x1 = as.numeric(droplevels(as.factor(data_jags$x1)))-1, # Density covariate (ref = high)
             x2 = as.numeric(droplevels(as.factor(data_jags$x2)))-1, # Gravel covariate (ref = black)
             x3 = ifelse(data_jags$x3 == "SS", 1, 0), # Site covariate (ref = CH)
             x4 = ifelse(data_jags$x3 == "WI", 1, 0), # Site covariate (ref = CH)
             genotype = as.numeric(droplevels(as.factor(for_model$genotype))),
             nalpha = length(unique(for_model$genotype)),
             plot = as.numeric(droplevels(as.factor(data_jags$plot_unique))),
             nplot = length(unique(data_jags$plot_unique)))

sink("modeling/demo_model.R")
cat("
model{ 
  # priors

  # Regression coefficients that do *not* have random intercepts/slopes
  beta3 ~ dnorm(0, 0.001)
  gamma1 ~ dnorm(0, 0.001)
  gamma2 ~ dnorm(0, 0.001)
  gamma3 ~ dnorm(0, 0.001)

  # Hyperparameters for random intercepts for fecundity part of the model
  mu.fecund ~ dnorm(0, 0.001)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)

  #sigma.kappa ~ dunif(0, 100) # SD hyperparameter for random intercepts
  #tau.kappa <- 1/(sigma.kappa^2)

  # Hyperparameters for random intercepts for survival part of the model
  mu.survive ~ dnorm(0, 0.001)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)

  # Random slopes for fecundity part of the model
  mu.slope.density ~ dnorm(0, 0.001)
  sigma.slope.density ~ dunif(0, 100)
  tau.slope.density <- 1/(sigma.slope.density^2)

  mu.slope.gravel ~ dnorm(0, 0.001)
  sigma.slope.gravel ~ dunif(0, 100)
  tau.slope.gravel <- 1/(sigma.slope.gravel^2)

  mu.slope.site1 ~ dnorm(0, 0.001)
  sigma.slope.site1 ~ dunif(0, 100)
  tau.slope.site1 <- 1/(sigma.slope.site1^2)

  mu.slope.site2 ~ dnorm(0, 0.001)
  sigma.slope.site2 ~ dunif(0, 100)
  tau.slope.site2 <- 1/(sigma.slope.site2^2)
  
  # Random slopes for survival part of the model
  mu.slope.site1.surv ~ dnorm(0, 0.001)
  sigma.slope.site1.surv ~ dunif(0, 100)
  tau.slope.site1.surv <- 1/(sigma.slope.site1.surv^2)

  mu.slope.site2.surv ~ dnorm(0, 0.001)
  sigma.slope.site2.surv ~ dunif(0, 100)
  tau.slope.site2.surv <- 1/(sigma.slope.site2.surv^2)
  
  # Loop through random intercepts and slopes for each genotype
  for(j in 1:nalpha){
    beta1[j] ~ dnorm(mu.slope.density, tau.slope.density)
    beta2[j] ~ dnorm(mu.slope.gravel, tau.slope.gravel)
    beta4[j] ~ dnorm(mu.slope.site1, tau.slope.site1)
    beta5[j] ~ dnorm(mu.slope.site2, tau.slope.site2)
    gamma4[j] ~ dnorm(mu.slope.site1.surv, tau.slope.site1.surv)
    gamma5[j] ~ dnorm(mu.slope.site2.surv, tau.slope.site2.surv)
    alpha[j] ~ dnorm(mu.fecund, tau.alpha)
    psi[j] ~ dnorm(mu.survive, tau.psi)
  }

  # Loop through random intercepts for each plot
  #for(k in 1:nplot){
  #  kappa[k] ~ dnorm(0, tau.kappa)
  #}

  # likelihood
  for (i in 1:N){ 

    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- w[i]*mu[i] + 0.00000001 # Hack for inconsistent nodes error
    w[i] ~ dbern(r[i]) 

    # link functions

    # Fecundity model
    log(mu[i]) <- alpha[genotype[i]] +
                  beta1[genotype[i]]*x1[i] + beta2[genotype[i]]*x2[i] + beta3*x1[i]*x2[i] +
                  beta4[genotype[i]]*x3[i] + beta5[genotype[i]]*x4[i]
    
    # Survival model
    logit(r[i]) <- psi[genotype[i]] +
                  gamma1*x1[i] + gamma2*x2[i] + gamma3*x1[i]*x2[i] +
                  gamma4[genotype[i]]*x3[i] + gamma5[genotype[i]]*x4[i]
} 


}
", fill = TRUE)
sink()



# Set initial values for draws
inits = list(
  list(beta3 = runif(1, -2, 2),
       gamma1 = runif(1, -2, 2),
       gamma2 = runif(1, -2, 2),
       gamma3 = runif(1, -2, 2),
       mu.slope.density = runif(1, -2, 2),
       mu.slope.gravel = runif(1, -2, 2),
       mu.slope.site1 = runif(1, -2, 2),
       mu.slope.site2 = runif(1, -2, 2),
       mu.slope.site1.surv = runif(1, -2, 2),
       mu.slope.site2.surv = runif(1, -2, 2),
       mu.fecund = runif(1, -2, 2), mu.survive = runif(1, -2, 2),
       sigma.alpha = runif(1, 1, 2),
       sigma.psi = runif(1, 1, 2),
       sigma.kappa = runif(1, 1, 2),
       sigma.slope.density = runif(1, 1, 2),
       sigma.slope.gravel = runif(1, 1, 2),
       sigma.slope.site1 = runif(1, 1, 2),
       sigma.slope.site2 = runif(1, 1, 2),
       sigma.slope.site1.surv = runif(1, 1, 2),
       sigma.slope.site2.surv = runif(1, 1, 2)),
  list(beta3 = runif(1, -2, 2),
       gamma1 = runif(1, -2, 2),
       gamma2 = runif(1, -2, 2),
       gamma3 = runif(1, -2, 2),
       mu.slope.density = runif(1, -2, 2),
       mu.slope.gravel = runif(1, -2, 2),
       mu.slope.site1 = runif(1, -2, 2),
       mu.slope.site2 = runif(1, -2, 2),
       mu.slope.site1.surv = runif(1, -2, 2),
       mu.slope.site2.surv = runif(1, -2, 2),
       mu.fecund = runif(1, -2, 2), mu.survive = runif(1, -2, 2),
       sigma.alpha = runif(1, 1, 2),
       sigma.psi = runif(1, 1, 2),
       sigma.kappa = runif(1, 1, 2),
       sigma.slope.density = runif(1, 1, 2),
       sigma.slope.gravel = runif(1, 1, 2),
       sigma.slope.site1 = runif(1, 1, 2),
       sigma.slope.site2 = runif(1, 1, 2),
       sigma.slope.site1.surv = runif(1, 1, 2),
       sigma.slope.site2.surv = runif(1, 1, 2)),
  list(beta3 = runif(1, -2, 2),
       gamma1 = runif(1, -2, 2),
       gamma2 = runif(1, -2, 2),
       gamma3 = runif(1, -2, 2),
       mu.slope.density = runif(1, -2, 2),
       mu.slope.gravel = runif(1, -2, 2),
       mu.slope.site1 = runif(1, -2, 2),
       mu.slope.site2 = runif(1, -2, 2),
       mu.slope.site1.surv = runif(1, -2, 2),
       mu.slope.site2.surv = runif(1, -2, 2),
       mu.fecund = runif(1, -2, 2), mu.survive = runif(1, -2, 2),
       sigma.alpha = runif(1, 1, 2),
       sigma.psi = runif(1, 1, 2),
       sigma.kappa = runif(1, 1, 2),
       sigma.slope.density = runif(1, 1, 2),
       sigma.slope.gravel = runif(1, 1, 2),
       sigma.slope.site1 = runif(1, 1, 2),
       sigma.slope.site2 = runif(1, 1, 2),
       sigma.slope.site1.surv = runif(1, 1, 2),
       sigma.slope.site2.surv = runif(1, 1, 2)))

# Set parameters for MCMC
n.adapt = 5000
n.update = 5000
n.iter = 5000

start <- Sys.time()

# Fit model in JAGS
jm = jags.model("modeling/demo_model.R", data = data, inits = inits, n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("beta3",
                                         "sigma.alpha", "mu.survive",
                                         "mu.fecund", "sigma.psi",
                                         "gamma1", "gamma2","gamma3", 
                                         "mu.slope.density", "sigma.slope.density",
                                         "mu.slope.gravel", "sigma.slope.gravel",
                                         "mu.slope.site1", "sigma.slope.site1",
                                         "mu.slope.site2", "sigma.slope.site2",
                                         "mu.slope.site1.surv", "sigma.slope.site1.surv",
                                         "mu.slope.site2.surv", "sigma.slope.site2.surv"), n.iter = n.iter, n.thin = 1)

# Plot trace and density plots
plot(zm)

end <- Sys.time()

end-start

 # Calculate median and 95% quantiles for parameters
samples <- as.data.frame(zm[[1]])
apply(samples, 2, quantile, probs = c(0.025, 0.5, 0.975)) -> params_seed_model




