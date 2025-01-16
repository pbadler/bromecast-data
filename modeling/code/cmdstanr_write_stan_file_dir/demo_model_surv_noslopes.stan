
data {
  int<lower=1> K_site;        // Number of sites
  int<lower=1> K_plot;        // Number of plots
  int<lower=1> K_genotype;    // Number of genotypes
  int<lower=0> N;             // Number of observations
  int<lower=0> P;             // Number of predictors
  array[N] int<lower=1, upper=K_site> site_id;       // Site group identifier
  array[N] int<lower=1, upper=K_plot> plot_id;       // Plot group identifier (nested within site)
  array[N] int<lower=1, upper=K_genotype> genotype_id; // Genotype group identifier
  matrix[N, P] X;             // Design matrix for fixed effects (including interactions)
  array[N] int<lower=0, upper=1> survival;          // Binary survival (0 or 1) response variable
}

parameters {
  real alpha;                          // Intercept
  vector[P] beta;                       // Coefficients for fixed effects (including interactions)
  
  real<lower=0> sigma_site;            // Standard deviation for random intercepts of site
  real<lower=0> sigma_plot;            // Standard deviation for random intercepts of plot
  real<lower=0> sigma_genotype;        // Standard deviation for random intercepts of genotype
  
  // Random intercepts for site, plot, and genotype
  vector[K_site] site_intercepts;      // Random intercepts for site
  vector[K_plot] plot_intercepts;      // Random intercepts for plot
  vector[K_genotype] genotype_intercepts;  // Random intercepts for genotype
}

model {
  // Likelihood: Bernoulli distribution for survival
  for (n in 1:N) {
    // Compute the logit (log-odds) for the probability of success
    real logit_p = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] +  // Random intercept for site
      plot_intercepts[plot_id[n]] +  // Random intercept for plot
      genotype_intercepts[genotype_id[n]];  // Random intercept for genotype

    // Bernoulli likelihood with logit link
    survival[n] ~ bernoulli_logit(logit_p);
  }

  // Priors
  alpha ~ normal(0, 5);  // Prior for the intercept
  beta ~ normal(0, 5);   // Prior for the fixed effects
  
  sigma_site ~ normal(0, 1);           // Prior for the standard deviation of random intercepts for site
  sigma_plot ~ normal(0, 1);           // Prior for the standard deviation of random intercepts for plot
  sigma_genotype ~ normal(0, 1);       // Prior for the standard deviation of random intercepts for genotype

  // Random intercepts: site, plot, genotype
  site_intercepts ~ normal(0, sigma_site);      // Random intercepts for site
  plot_intercepts ~ normal(0, sigma_plot);      // Random intercepts for plot
  genotype_intercepts ~ normal(0, sigma_genotype); // Random intercepts for genotype
}

generated quantities {
  vector[N] survival_pred;
  vector[N] log_likelihood_values;
  
  for (n in 1:N) {
    // Compute the logit (log-odds) for the probability of success
    real logit_p = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] +  // Random intercept for site
      plot_intercepts[plot_id[n]] +  // Random intercept for plot
      genotype_intercepts[genotype_id[n]];  // Random intercept for genotype

    // Predicted values using Bernoulli distribution (logit link)
    survival_pred[n] = bernoulli_logit_rng(logit_p);  // Predicted values using Bernoulli distribution
    
    // Log-likelihood for each observation, which can be used in LOO
    log_likelihood_values[n] = bernoulli_logit_lpmf(survival[n] | logit_p);
  }
}

