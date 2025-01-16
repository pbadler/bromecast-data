stan_program_surv <- "
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

  // Unique standard deviations for each random slope
  real<lower=0> sigma_genotype_slope_neighbors;     // Standard deviation for random slope of neighbors
  real<lower=0> sigma_genotype_slope_temp;      // Standard deviation for random slope of temp
  real<lower=0> sigma_genotype_slope_vwc; // Standard deviation for random slope of vwc
  real<lower=0> sigma_genotype_slope_climdist; // Standard deviation for random slope of climate distance
  real<lower=0> sigma_genotype_slope_climdist2; // Standard deviation for random slope of climate distance^2
  real<lower=0> sigma_genotype_slope_climdist3; // Standard deviation for random slope of climate distance^3

  // Random intercepts for site, plot, and genotype
  vector[K_site] site_intercepts;      // Random intercepts for site
  vector[K_plot] plot_intercepts;      // Random intercepts for plot
  vector[K_genotype] genotype_intercepts;  // Random intercepts for genotype
  
  // Random slopes for genotype (for temp, vwc, neighbors, and interactions)
  matrix[K_genotype, 6] genotype_slopes;  // Slopes for temp, vwc, and neighbors for each genotype

    
}

model {
  // Likelihood: Bernoulli distribution for survival
  for (n in 1:N) {
    // Compute the logit (log-odds) for the probability of success
    real logit_p = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] + 
      plot_intercepts[plot_id[n]] + 
      genotype_intercepts[genotype_id[n]] + 
      genotype_slopes[genotype_id[n], 1] * X[n, 1] +  // Random slope for temp
      genotype_slopes[genotype_id[n], 2] * X[n, 2] +  // Random slope for vwc
      genotype_slopes[genotype_id[n], 3] * X[n, 3] + // Random slope for neighbors
      genotype_slopes[genotype_id[n], 4] * X[n, 4] + // Random slope for climate distance
      genotype_slopes[genotype_id[n], 5] * X[n, 5] + // Random slope for climate distance^2
      genotype_slopes[genotype_id[n], 6] * X[n, 6]; // Random slope for climate distance^3
      
    // Bernoulli likelihood with logit link
    survival[n] ~ bernoulli_logit(logit_p);
  }

  // Priors
  alpha ~ normal(0, 5);  // Prior for the intercept
  beta ~ normal(0, 5);   // Prior for the fixed effects
  
  sigma_site ~ normal(0, 1);           // Prior for the standard deviation of random intercepts for site
  sigma_plot ~ normal(0, 1);           // Prior for the standard deviation of random intercepts for plot
  sigma_genotype ~ normal(0, 1);       // Prior for the standard deviation of random intercepts for genotype

  // Priors for unique standard deviations of random slopes
  sigma_genotype_slope_neighbors ~ normal(0, 1);     
  sigma_genotype_slope_temp ~ normal(0, 1);      
  sigma_genotype_slope_vwc ~ normal(0, 1); 
  sigma_genotype_slope_climdist ~ normal(0, 1);  
  sigma_genotype_slope_climdist2 ~ normal(0, 1);  
  sigma_genotype_slope_climdist3 ~ normal(0, 1);  

  // Random intercepts: site, plot, genotype
  site_intercepts ~ normal(0, sigma_site);      // Random intercepts for site
  plot_intercepts ~ normal(0, sigma_plot);      // Random intercepts for plot
  genotype_intercepts ~ normal(0, sigma_genotype); // Random intercepts for genotype

  // Random slopes for genotype (independent normal distributions with unique standard deviations)
  for (g in 1:K_genotype) {
    genotype_slopes[g, 1] ~ normal(0, sigma_genotype_slope_neighbors);     // Random slope for neighbors
    genotype_slopes[g, 2] ~ normal(0, sigma_genotype_slope_temp);      // Random slope for temp
    genotype_slopes[g, 3] ~ normal(0, sigma_genotype_slope_vwc); // Random slope for vwc
    genotype_slopes[g, 4] ~ normal(0, sigma_genotype_slope_climdist); // Random slope for climdist
    genotype_slopes[g, 5] ~ normal(0, sigma_genotype_slope_climdist2); // Random slope for climdist2
    genotype_slopes[g, 6] ~ normal(0, sigma_genotype_slope_climdist3); // Random slope for climdist3
  }
}

generated quantities {
  vector[N] survival_pred;
  vector[N] log_likelihood_values;
  
  for (n in 1:N) {
    // Compute the logit (log-odds) for the probability of success
    real logit_p = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] + 
      plot_intercepts[plot_id[n]] + 
      genotype_intercepts[genotype_id[n]] + 
      genotype_slopes[genotype_id[n], 1] * X[n, 1] +  // Random slope for neighbors
      genotype_slopes[genotype_id[n], 2] * X[n, 2] +  // Random slope for temp
      genotype_slopes[genotype_id[n], 3] * X[n, 3] + // Random slope for vwc
      genotype_slopes[genotype_id[n], 4] * X[n, 4] + // Random slope for climate distance
      genotype_slopes[genotype_id[n], 5] * X[n, 5] + // Random slope for climate distance^2
      genotype_slopes[genotype_id[n], 6] * X[n, 6]; // Random slope for climate distance^3
    
    // Predicted values using Bernoulli distribution (logit link)
    survival_pred[n] = bernoulli_logit_rng(logit_p);  // Predicted values using Bernoulli distribution
    
    // Log-likelihood for each observation, which can be used in LOO
    log_likelihood_values[n] = bernoulli_logit_lpmf(survival[n] | logit_p);
  }
}
"

f <- write_stan_file(stan_program_surv,
                     basename = "demo_model_surv",
                     dir = "modeling/code/cg_manuscript/cmdstanr_write_stan_file_dir")