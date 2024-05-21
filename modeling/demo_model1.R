
model{ 
  # Priors for global intercept for fecundity part of the model
  mu.fecund ~ dnorm(0, 0.001)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)

  # Prior for global intercept for survival part of the model
  mu.survive ~ dnorm(0, 0.001)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)

  # Loop through random intercept for each genotype
  for(j in 1:nalpha){
    alpha[j] ~ dnorm(mu.fecund, tau.alpha)
    psi[j] ~ dnorm(mu.survive, tau.psi)
  }

  # likelihood
  for (i in 1:N){ 

    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- w[i]*mu[i] + 0.00000001 # Hack for inconsistent nodes error
    w[i] ~ dbern(r[i]) 

    # Link functions

    # Fecundity model
    log(mu[i]) <- alpha[genotype[i]]
    
    # Survival model
    logit(r[i]) <- psi[genotype[i]] 
} 


}

