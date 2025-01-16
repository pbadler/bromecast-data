install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

check_cmdstan_toolchain()
install_cmdstan(cores = 2)

file <- file.path("demo_model.stan")
mod <- cmdstan_model(file, stanc_options = list("O1"), cpp_options = list(stan_threads = TRUE))
n_cores = parallel::detectCores()
chain = 3
threads_per_chain = ceiling(n_cores/chain)
# Takes around 20 minutes for 200 warm up samples followed by 1000 regular samples
fit <- mod$sample(
  data = data,
  chains = chain,
  seed = 345,
  parallel_chains = 3,
  show_messages = T,
  refresh = 5,
  iter_warmup = 200,
  threads_per_chain = threads_per_chain,
  iter_sampling = 1000
)
summary = fit$summary()
posterior <- fit$draws()
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("beta1"), n_warmup = 10)
p + facet_text(size = 15)
dplyr::filter(summary, rhat >= 1.10)