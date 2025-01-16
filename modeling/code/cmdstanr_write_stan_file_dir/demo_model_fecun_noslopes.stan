data {
  int<lower=1> K_site;  // Number of sites
  int<lower=1> K_plot;  // Number of plots
  int<lower=1> K_genotype;  // Number of genotypes
  int<lower=0> N;  // Number of observations
  int<lower=0> P;  // Number of predictors
  array[N] int<lower=1, upper=K_site> site_id;  // Site group identifier
  array[N] int<lower=1, upper=K_plot> plot_id;  // Plot group identifier (nested within site)
  array[N] int<lower=1, upper=K_genotype> genotype_id;  // Genotype group identifier
  matrix[N, P] X;  // Design matrix for fixed effects (including interactions)
  array[N] int<lower=0> seed_count;  // Seed count (Poisson-distributed response variable)
}

parameters {
  real alpha;  // Intercept
  vector[P] beta;  // Coefficients for fixed effects (including interactions)

  real<lower=0> sigma_site;  // Standard deviation for random intercepts of site
  real<lower=0> sigma_plot;  // Standard deviation for random intercepts of plot
  real<lower=0> sigma_genotype;  // Standard deviation for random intercepts of genotype
  real<lower=0> phi;  // Dispersion parameter (inverse of overdispersion) for Negative Binomial

  // Random intercepts for site, plot, and genotype
  vector[K_site] site_intercepts;  // Random intercepts for site
  vector[K_plot] plot_intercepts;  // Random intercepts for plot
  vector[K_genotype] genotype_intercepts;  // Random intercepts for genotype
}

model {
  // Likelihood: Negative Binomial distribution for seed_count with log link
  real log_likelihood = 0;  // Initialize the log likelihood
  
  for (n in 1:N) {
    // Compute the log of the mean (mu) for the Negative Binomial model
    real log_mu = alpha + dot_product(X[n], beta) +
                  site_intercepts[site_id[n]] +
                  plot_intercepts[plot_id[n]] +
                  genotype_intercepts[genotype_id[n]];

    // Negative Binomial likelihood with log link: exp(log_mu) gives the mean (mu)
    seed_count[n] ~ neg_binomial_2(exp(log_mu), phi);  // Correct Negative Binomial with log link
    
    // Add the log-likelihood of this observation to the total
    log_likelihood += neg_binomial_2_lpmf(seed_count[n] | exp(log_mu), phi);
  }

  // Priors
  alpha ~ normal(0, 5);  // Prior for the intercept
  beta ~ normal(0, 5);  // Prior for the fixed effects

  sigma_site ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for site
  sigma_plot ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for plot
  sigma_genotype ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for genotype
  phi ~ gamma(1, 1);  // Prior for the dispersion parameter (phi), gamma prior ensures it is positive

  // Random intercepts: site, plot, genotype
  site_intercepts ~ normal(0, sigma_site);  // Random intercepts for site
  plot_intercepts ~ normal(0, sigma_plot);  // Random intercepts for plot
  genotype_intercepts ~ normal(0, sigma_genotype);  // Random intercepts for genotype
}

generated quantities {
  vector[N] seed_count_pred;
  vector[N] log_likelihood_values;

  for (n in 1:N) {
    // Compute the log of the mean (mu) for the Negative Binomial model
    real log_mu = alpha + dot_product(X[n], beta) +
                  site_intercepts[site_id[n]] +
                  plot_intercepts[plot_id[n]] +
                  genotype_intercepts[genotype_id[n]];

    // Predicted seed count from the model, using Negative Binomial distribution
    seed_count_pred[n] = neg_binomial_2_rng(exp(log_mu), phi);  // Predicted values with Negative Binomial distribution
    
    // Log-likelihood for each observation, which can be used in LOO
    log_likelihood_values[n] = neg_binomial_2_lpmf(seed_count[n] | exp(log_mu), phi);
  }
}

