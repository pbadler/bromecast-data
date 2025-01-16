stan_program <- "
data {
  int<lower=1> N;            // number of data points
  int<lower=1> nalpha;       // number of genotypes
  int<lower=1> nplot;        // number of plots
  array[N] int d;          // observed data (seed count)
  array[N] int w;          // observed data (emergence)
  array[N] int genotype; // genotype index
  array[N] int plot;      // plot index
  vector[N] x1;              // covariate for density effect
  vector[N] x2;              // covariate for gravel effect
  vector[N] x3;              // covariate for location effect 1
  vector[N] x4;              // covariate for location effect 2
}
parameters {
  real beta1;                // density effect on fecundity
  real gamma1;                // density effect on survival
  real beta2;                // gravel effect on fecundity
  real gamma2;                // gravel effect on survival
  real beta3;                // location effect 1 on fecundity
  real gamma3;                // location effect 1 on survival
  real beta4;                // location effect 2 on fecundity
  real gamma4;                // location effect 2 on survival
  real beta5;                // density and gravel interaction effect on fecundity
  real gamma5;                // density and gravel interaction effect on survival
  real beta6;                // density and location 1 interaction effect on fecundity
  real gamma6;                // density and location 1 interaction effect on survival
  real beta7;                // density and location 2 interaction effect on fecundity
  real gamma7;                // density and location 2 interaction effect on survival
  real beta8;                // gravel and location 1 interaction effect on fecundity
  real gamma8;                // gravel and location 1 interaction effect on survival
  real beta9;                // gravel and location 2 interaction effect on fecundity
  real gamma9;                // gravel and location 2 interaction effect on survival
  real beta10;                // density, gravel, location 1 interaction effect on fecundity
  real gamma10;                // density, gravel, location 1 interaction effect on survival
  real beta11;                // density, gravel, location 2 interaction effect on fecundity
  real gamma11;                // density, gravel, location 2 interaction effect on survival
  real mu_fecund;            // global intercept for fecundity
  real mu_survive;           // global intercept for survival
  real<lower=0> sigma_psi;   // standard deviation for genotype deviations (survival)
  real<lower=0> sigma_omega; // standard deviation for plot deviations (survival)
  real<lower=0> sigma_kappa; // standard deviation for plot deviations (fecundity)
  real<lower=0> sigma_alpha; // standard deviation for genotype deviations (fecundity)
  
  vector[nalpha] alpha;      // random intercepts for each genotype (fecundity)
  vector[nplot] kappa;       // random intercepts for each plot (fecundity)
  vector[nalpha] psi;        // random intercepts for each genotype (survival)
  vector[nplot] omega;       // random intercepts for each plot (survival)
}
transformed parameters {
  real mu_fecund_star;       // identifiable intercept for fecundity
  real mu_survive_star;      // identifiable intercept for survival
  vector[nalpha] alpha_star; // identifiable random effect (genotype, fecundity)
  vector[nplot] kappa_star;  // identifiable random effect (plot, fecundity)
  vector[nalpha] psi_star;   // identifiable random effect (genotype, survival)
  vector[nplot] omega_star;  // identifiable random effect (plot, survival)
  
  real ave_alpha = mean(alpha);
  real ave_psi = mean(psi);
  real ave_kappa = mean(kappa);
  real ave_omega = mean(omega);
  
  mu_fecund_star = mu_fecund + ave_kappa + ave_alpha;
  mu_survive_star = mu_survive + ave_psi + ave_omega;
  
  alpha_star = alpha - ave_alpha;
  kappa_star = kappa - ave_kappa;
  psi_star = psi - ave_psi;
  omega_star = omega - ave_omega;
}
model {
  // Priors
  beta1 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma1 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta2 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma2 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta3 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma3 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta4 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma4 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta5 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma5 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta6 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma6 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta7 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma7 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta8 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma8 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta9 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma9 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta10 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma10 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  beta11 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)
  gamma11 ~ normal(0, 31.62);  // N(0, 0.001^-0.5)

  mu_fecund ~ normal(0, 31.62);
  mu_survive ~ normal(0, 31.62);
  
  sigma_psi ~ gamma(1, 1); // inverse-gamma priors (more stable)
  psi ~ normal(0, 1/sqrt(sigma_psi)); 
  
  sigma_alpha ~ gamma(1, 1); // inverse-gamma priors (more stable)
  alpha ~ normal(0, 1/sqrt(sigma_alpha));
  
  sigma_omega ~ gamma(1, 1); // inverse-gamma priors (more stable)
  omega ~ normal(0, 1/sqrt(sigma_omega));
  
  sigma_kappa ~ gamma(1, 1); // inverse-gamma priors (more stable)
  kappa ~ normal(0, 1/sqrt(sigma_kappa));
  
  // Likelihood
  vector[N] mu; // mean of seed counts 
  vector[N] r; // mean of seed counts 
  for (i in 1:N) {
    r[i] = inv_logit(mu_survive + gamma1 * x1[i] + gamma2 * x2[i] + gamma3 * x3[i] + gamma4 * x4[i] +
    gamma5 * x1[i] * x2[i] + gamma6 * x1[i] * x3[i] + gamma7 * x1[i] * x4[i] + gamma8 * x2[i] * x3[i] + gamma9 * x2[i] * x4[i] +
    gamma10 * x1[i] * x2[i] * x3[i] + gamma11 * x1[i] * x2[i] * x4[i] +
    omega[plot[i]] + psi[genotype[i]]);
    
    mu[i] = exp(mu_fecund + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + beta4 * x4[i] +
    beta5 * x1[i] * x2[i] + beta6 * x1[i] * x3[i] + beta7 * x1[i] * x4[i] + beta8 * x2[i] * x3[i] + beta9 * x2[i] * x4[i] +
    beta10 * x1[i] * x2[i] * x3[i] + beta11 * x1[i] * x2[i] * x4[i] +
    kappa[plot[i]] + alpha[genotype[i]]);
    d[i] ~ poisson(mu[i] * w[i] + 1e-8); // small value added for stability
    w[i] ~ bernoulli(r[i]);
  }
}
generated quantities {
  real sigma_alpha_star = 1/sd(alpha_star)^2;
  real sigma_psi_star = 1/sd(psi_star)^2;
  real sigma_kappa_star = 1/sd(kappa_star)^2;
  real sigma_omega_star = 1/sd(omega_star)^2;
}
"

f <- write_stan_file(stan_program,
                     basename = "demo_model",
                     dir = "cmdstanr_write_stan_file_dir")


