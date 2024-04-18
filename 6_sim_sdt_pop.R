# Notes ------------------------------------------------------------------------

# This script is for running the analyses on a really large dataset to calculate the "population" reliability

# brms seems to hard code constant priors to the stan model - so this approach here requires recompiling the model quite a few times, whcih isn't super efficient, but workable with a limited number of runs. 


# Load Stuff -------------------------------------------------------------------
rm(list=ls(all.names = T))
gc()
library(brms)
library(tidyverse)
library(future)
library(future.apply)
library(cmdstanr)

# Source all simulation functions 
# List all .R files in the folder
list.files(file.path("helper_functions"), pattern = "\\.R$", full.names = TRUE) %>%
  lapply(function(x) source(x))

# Compile stan model -----------------------------------------------------------

example_data = sim_sdt_binomial(
  sens_mean  = 1/2,
  sens_sigma = 1/4,
  k_sigma    = 0,
  # k_mean     = params_list$k_mean[i],
  # k_sigma    = params_list$k_sigma[i],
  n_pps        = 20, 
  n_trials_old = 10,
  n_trials_new = 20)

sim_model = brms::brm(
  y | trials(n_trials) ~ 1 + cond + (1 + cond || pps),  # I've set the random effects to uncorrelated here on purpose (in the fitted models we allow for a correlation between them)
  family = binomial(link = "probit"),
  # data =  data_frame(value = 0, cond=0, pps=1),
  data = example_data$data,
  cores   = 1,
  chains  = 2,
  warmup  = 1000, 
  iter    = 2000,
  init    = .01,
  prior = c(
    prior(constant(.05), class = "Intercept"),
    prior(constant(.5), class = "sd", coef = "Intercept", group = "pps"),
    prior(constant(0.25), class = "b",  coef = "cond"),
    prior(constant(0.125), class = "sd", coef = "cond", group = "pps")),
  backend = "rstan",
  # threads = threading(4),
  algorithm = "meanfield"   # meanfield does not seem to work with cmdstanr currently!
)

# Create Parameter Table ---------------------------------------------------

params_list <- expand.grid(
  sens_mean      = c(.5),
  sens_sigma     = c(0,.2,.4),
  k_mean         = c(0),
  k_sigma        = c(0, .20),
  sample_sizes = c( 100000),
  n_items = c(10, 20, 40),
  run_rep = 1:1  # 1 rep takes about 5 minutes (100 took 8.3 hours)
) # 8100 obs in 

saveRDS(params_list, file = file.path("results","6_params_list_pop_c.rds"))

# Run code in parallel using future --------------------------------------------

# future::plan(future::sequential())
future::plan(future::multisession(workers = 8))

# nrow(params_list)

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = 10, 1:nrow(params_list), function(i) {
# results <- lapply(1:2, function(i) {
  print(i)
  set.seed(10)
  suppressWarnings({
  run_sdt_sim(
    i = i,
    sens_mean  = params_list$sens_mean[i],
    sens_sigma = params_list$sens_sigma[i],
    k_mean     = params_list$k_mean[i],
    k_sigma    = params_list$k_sigma[i],
    n_pps      = params_list$sample_sizes[i], 
    n_items    = params_list$n_items[i],
    b_prior    = params_list$sens_mean[i],
    sd_prior   = params_list$sens_sigma[i],
    save_results = FALSE
  )})
  
}
)

time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

# save(results, file = file.path("results","4_results_tauinequiv_d.Rdata"))
saveRDS(results, file = file.path("results","6_results_pop_c.rds"))



