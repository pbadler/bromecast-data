
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

