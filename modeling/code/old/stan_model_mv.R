# Demographic model for common garden experiments

# Code for QR decomposition: https://medewitt.github.io/resources/stan_mult_linear_regression.html#fit_on_actual_data
# This makes multiple regression faster by putting things in matrix form and transforming

# Code for random slopes: https://nicholasrjenkins.science/tutorials/bayesian-inference-with-stan/mm_stan/

# Additional code for random slopes (haven't implemented this yet): https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/
stan_program_mv <- "
data {
  int<lower=0> N;   // number of data items
  int<lower=0> Ks;   // number of predictors for survival model
  int<lower=0> Kf;   // number of predictors for fecundity model
  int<lower=1> ngen;       // number of genotypes
  int<lower=1> nplot;        // number of plots
  matrix[N, Ks] x_surv;   // predictor matrix for survival
  matrix[N, Kf] x_fecun;   // predictor matrix for fecundity
  array[N] int y;      // seed count
  array[N] int w;      // binary survival outcome
  array[N] int genotype; // genotype index
  array[N] int plot;      // plot index
  vector[N] neighbors; // density covariate for fecundity model
  vector[N] gravel; // gravel covariate for fecundity model
  vector[N] location1; // location1 covariate for fecundity model
  vector[N] location2; // location2 covariate for fecundity model
  vector[N] year; // year covariate for fecundity model
}
// QR decomposition to optimize efficiency for survival
transformed data {
  matrix[N, Ks] Q_ast_s;
  matrix[Ks, Ks] R_ast_s;
  matrix[Ks, Ks] R_ast_inverse_s;
  // thin and scale the QR decomposition
  Q_ast_s = qr_Q(x_surv)[, 1:Ks] * sqrt(N - 1);
  R_ast_s = qr_R(x_surv)[1:Ks, ] / sqrt(N - 1);
  R_ast_inverse_s = inverse(R_ast_s);
  
// QR decomposition to optimize efficiency for fecundity  
  matrix[N, Kf] Q_ast_f;
  matrix[Kf, Kf] R_ast_f;
  matrix[Kf, Kf] R_ast_inverse_f;
  // thin and scale the QR decomposition
  Q_ast_f = qr_Q(x_fecun)[, 1:Kf] * sqrt(N - 1);
  R_ast_f = qr_R(x_fecun)[1:Kf, ] / sqrt(N - 1);
  R_ast_inverse_f = inverse(R_ast_f);
  
  // for posterior predictive checks
  real<lower=0> mean_y = mean(to_vector(y));
  real<lower=0> sd_y = sd(to_vector(y));
  
}
parameters {
  real mu_survive;  // intercept for survival
  real mu_fecund;   // intercept for fecundity
  vector[Kf] theta;  // coefficients on Q_ast for fecundity
  vector[Ks] phi;    // coefficients on Q_ast for survival
  
  vector<lower=0>[6] sigma_alpha; // standard deviations for random intercept and slopes for genotype (fecundity)
  real<lower=0> tau_psi;   // standard deviations for genotype deviations (survival)
  real<lower=0> tau_omega; // standard deviation for plot deviations (survival)
  real<lower=0> tau_kappa; // standard deviation for plot deviations (fecundity)
  
  vector[ngen] alpha;      // random intercepts for each genotype (fecundity)
  vector[nplot] kappa;       // random intercepts for each plot (fecundity)
  vector[ngen] psi;        // random intercepts for each genotype (survival)
  vector[nplot] omega;       // random intercepts for each plot (survival)
  
  vector[ngen] density_slope;
  vector[ngen] gravel_slope;
  vector[ngen] location1_slope;
  vector[ngen] location2_slope;
  vector[ngen] year_slope;
  real a;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  corr_matrix[6] Rho;
}

transformed parameters {
  real mu_fecund_star;       // identifiable intercept for fecundity
  real mu_survive_star;      // identifiable intercept for survival
  vector[ngen] alpha_star; // identifiable random effect (genotype, fecundity)
  vector[nplot] kappa_star;  // identifiable random effect (plot, fecundity)
  vector[ngen] psi_star;   // identifiable random effect (genotype, survival)
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
  vector[N] mu; // mean of seed counts 
  vector[N] r; // mean survival probability 
  vector[N] fixef_surv; 
  vector[N] fixef_fecun; 
  
  // varying slopes and varying intercepts component
  {
  // vector for fecundity random effects: intercept mean and slope mean
  array[ngen] vector[6] YY;
  vector[6] MU;
  MU = [a , b1, b2, b3, b4, b5]';
  for (j in 1:ngen) YY[j] = [alpha[j], density_slope[j], gravel_slope[j], location1_slope[j], location2_slope[j], year_slope[j]]';
    YY ~ multi_normal(MU, quad_form_diag(Rho, sigma_alpha));
  }
  // hyper-priors
  Rho ~ lkj_corr(1);
  sigma_alpha ~ exponential(1);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);
  b3 ~ normal(0, 10);
  b4 ~ normal(0, 10);
  b5 ~ normal(0, 10);
  a ~ normal(0, 10);

  // Bring together fixed effects for both parts of the model
  fixef_surv = Q_ast_s * phi + mu_survive;
  fixef_fecun = Q_ast_f * theta + mu_fecund;
  
  for (i in 1:N) {
    
    // Process model for survival 
    r[i] = inv_logit(fixef_surv[i] + psi[genotype[i]] + omega[plot[i]]);
    
    // Process model for fecundity
    mu[i] = exp(fixef_fecun[i] + 
          density_slope[genotype[i]] * neighbors[i] +
          gravel_slope[genotype[i]] * gravel[i] +
          location1_slope[genotype[i]] * location1[i] + location2_slope[genotype[i]] * location2[i] +
          year_slope[genotype[i]] * year[i] +
          alpha[genotype[i]] + kappa[plot[i]]);
    
    y[i] ~ poisson(mu[i] * w[i] + 1e-8); // small value added for stability
    w[i] ~ bernoulli(r[i]);
  }
  
  mu_fecund ~ normal(0, 10);
  mu_survive ~ normal(0, 10);
  
  tau_psi ~ gamma(1, 1); // inverse-gamma priors (more stable)
  psi ~ normal(0, 1/sqrt(tau_psi));
  
  tau_omega ~ gamma(1, 1); // inverse-gamma priors (more stable)
  omega ~ normal(0, 1/sqrt(tau_omega));
  
  tau_kappa ~ gamma(1, 1); // inverse-gamma priors (more stable)
  kappa ~ normal(0, 1/sqrt(tau_kappa));
  
}
// converts the quantities back to the original scale
generated quantities {

  vector[Kf] beta;
  beta = R_ast_inverse_f * theta; // coefficients on x for fecundity;
  vector[Ks] gamma;
  gamma = R_ast_inverse_s * phi; // coefficients on x for survival
  
}
"

f <- write_stan_file(stan_program_mv,
                     basename = "demo_model_mv",
                     dir = "cmdstanr_write_stan_file_dir")