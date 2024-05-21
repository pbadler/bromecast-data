
model{ 
  # Priors for global intercept for fecundity part of the model
  mu.fecund ~ dnorm(0, 0.001)

  # Prior for deviations for each genotype from global intercept (fecundity)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)

  # Prior for deviations for each plot from global intercept (fecundity)
  sigma.kappa ~ dunif(0, 100) 
  tau.kappa <- 1/(sigma.kappa^2)

  # Priors for global intercept for survival part of the model
  mu.survive ~ dnorm(0, 0.001)
  
  # Prior for deviations for each genotype from global intercept (survival)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)

  # Loop through random intercept deviations for each genotype
  for(j in 1:nalpha){
    alpha[j] ~ dnorm(0, tau.alpha)
    psi[j] ~ dnorm(0, tau.psi)
  }

  # Loop through random intercept deviations for each plot
  for(k in 1:nplot){
   kappa[k] ~ dnorm(0, tau.kappa)
  }

  # likelihood
  for (i in 1:N){ 

    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- w[i]*mu[i] + 0.00000001 # Hack for inconsistent nodes error
    w[i] ~ dbern(r[i]) 

    # Link functions

    # Fecundity model
    log(mu[i]) <- mu.fecund + alpha[genotype[i]] + kappa[plot[i]]
    
    # Survival model
    logit(r[i]) <- mu.survive + psi[genotype[i]] 
} 


}

