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
# data <- list(d = data_jags$d,
#              N = nrow(data_jags),
#              x1 = as.numeric(droplevels(as.factor(data_jags$x1)))-1, # Density covariate (ref = high)
#              x2 = as.numeric(droplevels(as.factor(data_jags$x2)))-1, # Gravel covariate (ref = black)
#              x3 = ifelse(data_jags$x3 == "SS", 1, 0), # Site covariate (ref = CH)
#              x4 = ifelse(data_jags$x3 == "WI", 1, 0), # Site covariate (ref = CH)
#              genotype = as.numeric(droplevels(as.factor(for_model$genotype))),
#              nalpha = length(unique(for_model$genotype)),
#              plot = as.numeric(droplevels(as.factor(data_jags$plot_unique))),
#              nplot = length(unique(data_jags$plot_unique)))

# Create data object for JAGS
data <- list(d = data_jags$d,
             N = nrow(data_jags),
             x1 = as.numeric(droplevels(as.factor(data_jags$x1)))-1, # Density covariate (ref = high)
             genotype = as.numeric(droplevels(as.factor(for_model$genotype))),
             nalpha = length(unique(for_model$genotype)),
             plot = as.numeric(droplevels(as.factor(data_jags$plot_unique))),
             nplot = length(unique(data_jags$plot_unique)))

sink("modeling/demo_model.R")
cat("
model{ 
  # priors

  # Regression coefficients that do *not* have random intercepts/slopes
  beta1 ~ dnorm(0, 0.001)
  #beta2 ~ dnorm(0, 0.001)
  #beta3 ~ dnorm(0, 0.001)
  #beta4 ~ dnorm(0, 0.001)
  #beta5 ~ dnorm(0, 0.001)
  gamma1 ~ dnorm(0, 0.001)
  #gamma2 ~ dnorm(0, 0.001)
  #gamma3 ~ dnorm(0, 0.001)
  #gamma4 ~ dnorm(0, 0.001)
  #gamma5 ~ dnorm(0, 0.001)


  #####
  ## Hyperparameters for random intercepts for fecundity part of the model
  #####
  # Non-identifiable intercept (DON'T MONITOR)
  beta0 ~ dnorm(0, 0.001)
  # Identifiable intercept (MONITOR)
  beta0.star <- beta0 + ave.kappa + ave.alpha 
  # Prior for deviations for each GENOTYPE from global intercept (fecundity)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)
  # Prior for deviations for each PLOT from global intercept (fecundity)
  sigma.kappa ~ dunif(0, 100) # SD hyperparameter for random intercepts
  tau.kappa <- 1/(sigma.kappa^2)


  #####
  ## Hyperparameters for random intercepts for survival part of the model
  #####
  # Non-identifiable intercept (DON'T MONITOR)
  gamma0 ~ dnorm(0, 0.001)
  # Identifiable intercept (MONITOR)
  gamma0.star <- gamma0 + ave.psi + ave.omega
  # Prior for deviations for each GENOTYPE from global intercept (survival)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)
  # Prior for deviations for each PLOT from global intercept (survival)
  sigma.omega ~ dunif(0, 100)
  tau.omega <- 1/(sigma.omega^2)


  #####
  # Random slopes for fecundity part of the model
  #####

  #####
  # Random slopes for survival part of the model
  #####
  
  # Loop through random intercepts and slopes for each genotype
  for(j in 1:nalpha){

    # Non-identifiable random intercepts (DON'T MONITOR)
    psi[j] ~ dnorm(0, tau.psi)
    alpha[j] ~ dnorm(0, tau.alpha)
    # Identifiable random intercepts (MONITOR)
    alpha.star[j] <- alpha[j] - ave.alpha
    psi.star[j] <- psi[j] - ave.psi
  } # end genotype loop

  # Loop through random intercepts and slopes for each PLOT
  for(k in 1:nplot){
    # Non-identifiable (DON'T MONITOR)
    kappa[k] ~ dnorm(0, tau.kappa)
    omega[k] ~ dnorm(0, tau.omega)  
    # Identifiable random effect (MONITOR)
    kappa.star[k] <- kappa[k] - ave.kappa
    omega.star[k] <- omega[k] - ave.omega
  } # end plot loop

  # likelihood
  for (i in 1:N){ 

    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- w[i]*mu[i] + 0.00000001 # Hack for inconsistent nodes error
    w[i] ~ dbern(r[i]) 

    # link functions

    # Fecundity model
    #log(mu[i]) <- beta0 + alpha[genotype[i]] + kappa[plot[i]] +
    #              beta1*x1[i] + beta2*x2[i] + beta3*x1[i]*x2[i] +
    #              beta4*x3[i] + beta5*x4[i]

    log(mu[i]) <- beta0 + beta1*x1[i] + alpha[genotype[i]] + kappa[plot[i]] 
    
    # Survival model
    #logit(r[i]) <- gamma0 + psi[genotype[i]] + omega[plot[i]] +
    #               gamma1*x1[i] + gamma2*x2[i] + gamma3*x1[i]*x2[i] +
    #               gamma4*x3[i] + gamma5*x4[i]

    logit(r[i]) <- gamma0 + gamma1*x1[i] + psi[genotype[i]] + omega[plot[i]]

}# end likelihood 
  #####
  ## Derived quantities
  #####
  
  # Mean genotype-level random intercept (DON'T MONITOR)
  ave.alpha <- mean(alpha[])
  ave.psi <- mean(psi[])
  # Identifiable sd of random intercept (MONITOR)
  sigma.alpha.star <- sd(alpha.star[])
  sigma.psi.star <- sd(psi.star[]) 

  # Mean plot-level random effect (DON'T MONITOR)
  ave.kappa <- mean(kappa[])
  ave.omega<- mean(omega[])
  # Identifiable sd of random intercept (MONITOR)
  sigma.kappa.star <- sd(kappa.star[])
  sigma.omega.star <- sd(omega.star[]) 

}
", fill = TRUE)
sink()

# Set parameters for MCMC
n.adapt = 2000
n.update = 2000
n.iter = 5000

start <- Sys.time()

# Fit model in JAGS
jm = jags.model("modeling/demo_model.R", data = data, n.chains = 3, n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("beta0.star", "gamma0.star",
                                         "sigma.kappa.star", "sigma.omega.star",
                                         "sigma.alpha.star", "sigma.psi.star",
                                         "beta1", "gamma1"), n.iter = n.iter, n.thin = 1)


end <- Sys.time()

end-start
# Plot trace and density plots
plot(zm)




 # Calculate median and 95% quantiles for parameters
samples <- as.data.frame(zm[[1]])
apply(samples, 2, quantile, probs = c(0.025, 0.5, 0.975)) -> params_seed_model




