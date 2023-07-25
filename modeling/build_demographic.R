# Create JAGS model of demography for common garden data

# Load libraries
library(tidyverse); library(rjags)

# Create "not in" operator
`%notin%` <- Negate(`%in%`)

# Read in harvest data 
harvest <- read_csv("gardens/rawdata/CG_harvest2022_4May23.csv")

# Make column names all lower case
names(harvest) <- tolower(names(harvest))

# Filter to be just sheep station and make unique ID
harvest %>% 
  filter(site == "SheepStation") %>% 
  mutate(id = paste(plot, density, albedo, x, y, sep = "_")) -> harvestSS

# Create data subset 1: harvested seeds that were subsetted
harvestSS %>% 
  filter(complete.cases(inflor_mass) & live %in% c("Y", "N")) %>% 
  mutate(inflor_mass_sub = biomass_sub + seed_mass_sub,
         ratio = inflor_mass / inflor_mass_sub) %>% 
  mutate(positive = ifelse(ratio < 1, 0, 1)) %>% 
  filter(positive == 1) %>% 
  mutate(seed_count_total = round(seed_count_sub * ratio)) %>%  
  # Take out outlier
  filter(seed_count_total < 4500 & seed_count_total > 0) %>% 
  select(-inflor_mass_sub, -positive, -ratio) -> calib_data_subsetted

# Create data subset 2: harvested seeds where all were counted
harvestSS %>% 
  filter(complete.cases(seed_count_whole) & live %in% c("Y", "N")) %>% 
  mutate(seed_count_total = seed_count_whole) -> calib_data_whole

# Create data subset 3: plants that did not produce seeds
harvestSS %>% 
  filter(id %notin% calib_data_subsetted$id & id %notin% calib_data_whole$id & live %in% c("Y", "N")) %>% 
  mutate(seed_count_total = seed_count_whole)-> calib_data_noflower

# Bind data subsets 1-3 back together
rbind(calib_data_subsetted, calib_data_whole, calib_data_noflower) -> data_all

# Set all NA seed count observations to 0
data_all %>% 
  mutate(seed_count_total = ifelse(is.na(seed_count_total), 0, seed_count_total))-> data_all

# If there is no biomass at the end, plant should not be live (live == "N")
data_all %>% 
  mutate(live = ifelse(is.na(biomass_whole), "N", "Y")) -> data_all

# Create data object with just what we need for model
data_all %>% 
  mutate(w = ifelse(live == "Y", 1, 0),
         d = as.integer(seed_count_total)) %>% 
  select(x1 = density,
         x2 = albedo,
         w,
         d) -> data_jags

sink("modeling/demo_model.R")
cat("
model{ 
  # priors
  for (i in 1:3){
    beta[i] ~ dnorm(0, 0.01)
    gamma[i] ~ dnorm(0, 0.01)
  }

  mu.alpha ~ dnorm(0, 0.01)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)

  mu.psi ~ dnorm(0, 0.01)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)
  
  # random intercepts
  for(j in 1:nalpha){
    alpha[j] ~ dnorm(mu.alpha, tau.alpha)
    psi[j] ~ dnorm(mu.psi, tau.psi)
  }

  # likelihood
  for (i in 1:N){ 
    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- w[i]*mu[i] 
    w[i] ~ dbern(r[i]) 
    # link functions
    log(mu[i]) <- alpha[genotype[i]] + beta[1]*x1[i] + beta[2]*x2[i] + beta[3]*x1[i]*x2[i] 
    logit(r[i]) <- psi[genotype[i]] + gamma[1]*x1[i] + gamma[2]*x2[i] + gamma[3]*x1[i]*x2[i]
} 

  # derived quantities

}
", fill = TRUE)
sink()

# Create vector of survival / mortality
w <- data_jags$w
w[data_jags$w > 0] <- 1

# Set initial values for draws
inits = list(
  list(beta = runif(3, -2, 2), gamma = runif(3, -2, 2),
       mu.alpha = 1, sigma.alpha = 1,
       mu.psi = 1, sigma.psi = 1),
  list(beta = runif(3, -2, 2), gamma = runif(3, -2, 2),
       mu.alpha = 2, sigma.alpha = 2,
       mu.psi = 2, sigma.psi = 2),
  list(beta = runif(3, -2, 2), gamma = runif(3, -2, 2),
       mu.alpha = 3, sigma.alpha = 3,
       mu.psi = 3, sigma.psi = 3))

# Create data object for JAGS
data <- list(d = data_jags$d,
             x1 = as.numeric(as.factor(data_jags$x1))-1, 
             x2 = as.numeric(as.factor(data_jags$x2))-1,
             N = nrow(data_jags),
             w = w,
             genotype = as.numeric(droplevels(as.factor(data_all$genotype))),
             nalpha = length(unique(data_all$genotype)))

# Set parameters for MCMC
n.adapt = 5000
n.update = 10000
n.iter = 20000

# Fit model in JAGS
jm = jags.model("modeling/demo_model.R", data = data, inits = inits, n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("gamma", "beta", "mu.alpha", "sigma.alpha", "mu.psi", "sigma.psi"), n.iter = n.iter, n.thin = 1)

# Plot trace and density plots
plot(zm)

# Calculate median and 95% quantiles for parameters
samples <- as.data.frame(zm[[1]])
apply(samples, 2, quantile, probs = c(0.025, 0.5, 0.975))
