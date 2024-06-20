
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

