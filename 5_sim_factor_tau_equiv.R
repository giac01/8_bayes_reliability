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
  lapply(., function(x) {source(x)})

# Compile stan model -----------------------------------------------------------

mod <- cmdstan_model(file.path("stan_models","stan_equiv_factor_model_v3.stan"))

# Create Parameter Table ---------------------------------------------------

count_so_far = function(x){
  # x = params_list$n_items
  out = sapply(1:length(x), function(i) length(which(x[1:i]==x[i])))
  return(out)
}

# Example of creating a list of all combinations
params_list <- expand.grid(
  tau_equivalence = c(TRUE),
  sample_sizes = c(50, 100, 500, 2000),
  n_items = c(3, 6, 12),
  run_rep = 1:400
) 

# 250 reps took 3 hours and 25 minuntes 

params_list$loadings_set = count_so_far(params_list$sample_sizes)

# Create set of loadings to use ------------------------------------------------

loadings_list = list()

for(i in 1:max(params_list$loadings_set)){
  x = params_list %>%
    filter(loadings_set == i) 
  
  if (length(unique(x$n_items)) !=1 ) {stop("ERROR")}
  if (length(unique(x$tau_equivalence)) !=1 ) {stop("ERROR")}
  
  if (unique(x$tau_equivalence)==TRUE)   { loadings_list[[i]]  = rep(rbeta(1, 1, 1.9), unique(x$n_items)) }
  if (unique(x$tau_equivalence)==FALSE)  { loadings_list[[i]]  = rbeta(unique(x$n_items), 1, 1.9) }
}

params_list = params_list %>%
  arrange(loadings_set)

saveRDS(params_list, file = file.path("results","5_params_list_a.rds"))

# Run code in parallel using future --------------------------------------------

print(availableCores(logical = TRUE))

future::plan(future::multisession(workers = availableCores()))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = 10, 1:nrow(params_list), function(i) {
  run_factor_sim_2(
    i = i,
    n = params_list$sample_sizes[i], 
    n_items = params_list$n_items[i], 
    loadings = loadings_list[[params_list$loadings_set[i]]],
    h_ci_calc = FALSE,
    use_init = FALSE,
    additional_tests = TRUE
  )
}
)

time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

# Save results 

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- sprintf("5_results_tauequiv_%s.rds", timestamp)
saveRDS(results, file = file.path("results", filename))




