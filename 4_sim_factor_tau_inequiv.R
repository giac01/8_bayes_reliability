# Load Stuff -------------------------------------------------------------------
#rm(list=ls(all.names = T))
gc()

#Sys.setenv(CMDSTAN_PATH = '/home/gb424/.cmdstan/cmdstan-2.34.1')
#library(cmdstanr)
#cmdstanr::set_cmdstan_path(Sys.getenv("CMDSTAN_PATH"))
cmdstanr::set_cmdstan_path(path = "/home/gb424/.cmdstan/cmdstan-2.34.1")

library(brms)
library(tidyverse)
library(future)
library(future.apply)
#library(cmdstanr)
#source("0_set_cmdstan_path_cluster.R")
#cmdstanr::set_cmdstan_path(path = "/home/gb424/cmdstan/cmdstan-2.34.1")
#Sys.setenv(CMDSTAN_PATH = '/home/gb424/cmdstan/cmdstan-2.34.1')
#library(cmdstanr)
#≈cmdstanr::set_cmdstan_path(Sys.getenv("CMDSTAN_PATH"))
library(cmdstanr)

# Source all simulation functions 
# List all .R files in the folder
list.files(file.path("helper_functions"), pattern = "\\.R$", full.names = TRUE) %>%
  lapply(., function(x) {source(x)})

# Compile stan model -----------------------------------------------------------

mod <- cmdstan_model(file.path("stan_models","stan_inequiv_factor_model_v5.stan"))

# Create Parameter Table ---------------------------------------------------

count_so_far = function(x){
  out = sapply(1:length(x), function(i) length(which(x[1:i]==x[i])))
  return(out)
}

rel_function = function(l){
  e = 1 - l
  return(sqrt(sum(l^2/e)/(1+sum(l^2/e))))
}

loadings_list = list(
  c( 0, 0, 0, 0, 0, 0),
  c(.3,.2,.1),
  c(.3,.2,.1,.1,.1,.1),
  c(.4,.3,.3,.2,.1,.0),
  c(.6,.5,.3,.1,.1,.1),
  c(.7,.6,.6,.5,.4),
  c(.7,.6,.5,.5,.4,.4,.4,.3,.3)
)

# Reliabilities 

lapply(loadings_list, rel_function)

# Example of creating a list of all combinations
params_list <- expand.grid(
  loading_set  = 1:length(loadings_list),
  sample_sizes = c(200, 500, 2000),
  run_rep = 1:400  
) 

# 100 reps completed in 3 hours and 25 minutes 

saveRDS(params_list, file = file.path("results","4_params_list_aa.rds"))

# Run code in parallel using future --------------------------------------------
print(availableCores())

future::plan(future::multisession(workers = availableCores()))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = FALSE, 1:nrow(params_list), function(i) {
  run_factor_sim_2(
    i = i,
    n = params_list$sample_sizes[i], 
    n_items  = length(loadings_list[[params_list$loading_set[i]]]), 
    loadings = loadings_list[[params_list$loading_set[i]]],
    additional_tests = TRUE
  )
}
)
time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

#saveRDS(results, file = file.path("results","4_results_tauinequiv_aa.rds"))

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- sprintf("4_results_tauinequiv_%s.rds", timestamp)
saveRDS(results, file = file.path("results", filename))


# Time difference of 2.264748 days
