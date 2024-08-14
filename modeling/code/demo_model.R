model{
  # Prior for density effect on fecundity
  beta1 ~ dnorm(0, 0.001)
  
  # Priors for global intercept for fecundity part of the model
  mu.fecund ~ dnorm(0, 0.001)
  # Identifiable intercept (monitor this)
  mu.fecund.star <- mu.fecund + ave.kappa + ave.alpha
  
  # Prior for global intercept for survival part of the model
  mu.survive ~ dnorm(0, 0.001)
  # Identifiable intercept (monitor this)
  mu.survive.star <- mu.survive + ave.psi + ave.omega
  
  # Prior for deviations for each genotype from global intercept (survival)
  sigma.psi ~ dunif(0, 100)
  tau.psi <- 1/(sigma.psi^2)
  
  # Prior for deviations for each plot from global intercept (survival)
  sigma.omega ~ dunif(0, 100)
  tau.omega <- 1/(sigma.omega^2)
  
  # Prior for deviations for each plot from global intercept (fecundity)
  sigma.kappa ~ dunif(0, 100) 
  tau.kappa <- 1/(sigma.kappa^2)
  
  # Prior for deviations for each genotype from global intercept (fecundity)
  sigma.alpha ~ dunif(0, 100)
  tau.alpha <- 1/(sigma.alpha^2)
  
  # Loop through random intercepts for each genotype
  for(j in 1:nalpha){
    # Not identifiable don't monitor! 
    psi[j] ~ dnorm(0, tau.psi)
    # Identifiable random effect (monitor this)
    psi.star[j] <- psi[j] - ave.psi
    # Not identifiable don't monitor! 
    alpha[j] ~ dnorm(0, tau.alpha)
    # Identifiable random effect (monitor this)
    alpha.star[j] <- alpha[j] - ave.alpha
  }
  # Mean genotype-level random effect:
  ave.alpha <- mean(alpha[])
  sigma.alpha.star <- sd(alpha.star[]) 
  ave.psi <- mean(psi[])
  sigma.psi.star <- sd(psi.star[])
  
  # Loop through random intercept deviations for each plot
  for(k in 1:nplot){
    # Not identifiable don't monitor! 
    kappa[k] ~ dnorm(0, tau.kappa)
    # Identifiable random effect (monitor this)
    kappa.star[k] <- kappa[k] - ave.kappa
    # Not identifiable don't monitor! 
    omega[k] ~ dnorm(0, tau.omega)
    # Identifiable random effect (monitor this)
    omega.star[k] <- omega[k] - ave.omega
  }
  # Mean plot-level random effect:
  ave.kappa <- mean(kappa[])
  sigma.kappa.star <- sd(kappa.star[]) 
  ave.omega <- mean(omega[])
  sigma.omega.star <- sd(omega.star[]) 
  
  # likelihood
  for (i in 1:N){ 
    
    # data model
    d[i] ~ dpois(lambda[i]) 
    lambda[i] <- mu[i]*w[i]  + 0.00000001 # Hack for inconsistent nodes error
    w[i] ~ dbern(r[i]) 
    
    # Link functions
    
    # Fecundity model
    log(mu[i]) <- alpha[genotype[i]] + beta1*x1[i] + kappa[plot[i]] + mu.fecund
    
    # Survival model
    logit(r[i]) <- psi[genotype[i]] + omega[plot[i]] + mu.survive
  } 
  
  
}