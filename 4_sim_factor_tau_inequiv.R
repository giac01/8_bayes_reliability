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

mod <- cmdstan_model(file.path("helper_functions","stan_factor_model_v6.stan"))

# Create Parameter Table ---------------------------------------------------

count_so_far = function(x){
  # x = params_list$n_items
  out = sapply(1:length(x), function(i) length(which(x[1:i]==x[i])))
  return(out)
}

# Example of creating a list of all combinations
params_list <- expand.grid(
  tau_equivalence = c(FALSE),
  constained_loadings = c(FALSE, TRUE),
  sample_sizes = c(200, 500, 2000),
  n_items = c(4, 6, 12),
  run_rep = 1:80  # 1 rep takes about 5 minutes (100 took 8.3 hours)
) 

# Example of creating a list of all combinations
# params_list <- expand.grid(
#   tau_equivalence = c(FALSE),
#   sample_sizes = c(4000),
#   n_items = c(8),
#   run_rep = 1:8*4  # 1 rep takes about 5 minutes (100 took 8.3 hours)
# ) 

params_list$loadings_set = count_so_far(params_list$sample_sizes)

# Create set of loadings to use ------------------------------------------------

loadings_list = list()

for(i in 1:max(params_list$loadings_set)){
  x = params_list %>%
    filter(loadings_set == i) 
  
  if (length(unique(x$n_items)) !=1 ) {stop("ERROR")}
  if (length(unique(x$tau_equivalence)) !=1 ) {stop("ERROR")}
  
  # if (unique(x$tau_equivalence)==TRUE)   { loadings_list[[i]] = rep(runif(1,.01,.99), unique(x$n_items)) }
  # if (unique(x$tau_equivalence)==FALSE)  { loadings_list[[i]]  = runif(unique(x$n_items), .01, .99) }
  
  # if (unique(x$constained_loadings)==FALSE)   { loadings_list[[i]] = rep(unique(x$n_items), min = .01, max = .99 ) }
  # if (unique(x$constained_loadings)==FALSE)   { loadings_list[[i]] = rep(unique(x$n_items), min = .3, max = .7 ) }
  # 
  # 
  
  if (unique(x$constained_loadings)==TRUE)  {
    # loadings_list[[i]]    = runif(unique(x$n_items), min = 0, max = 1);
    loadings_list[[i]]    = runif(unique(x$n_items), min = 0, max = 1);
    
    loadings_list[[i]]    = sort( loadings_list[[i]], decreasing = TRUE);
    loadings_list[[i]][1] = runif(1, min = .3, max = .8);
    loadings_list[[i]][2] = runif(1, min = .3, max = .5);
  }
  
  
  if (unique(x$constained_loadings)==FALSE)  {
    loadings_list[[i]]    = runif(unique(x$n_items), min = 0, max = 1);
  }
}

params_list = params_list %>%
  arrange(loadings_set)

saveRDS(params_list, file = file.path("results","4_params_list_g.rds"))


# Run code in parallel using future --------------------------------------------

future::plan(future::multisession(workers = 8))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = 10, 1:nrow(params_list), function(i) {
  run_factor_sim_2(
    i = i,
    n = params_list$sample_sizes[i], 
    n_items = params_list$n_items[i], 
    loadings = loadings_list[[params_list$loadings_set[i]]],
    additional_tests = TRUE
  )
}
)
time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

saveRDS(results, file = file.path("results","4_results_tauinequiv_g.rds"))

# Time difference of 2.264748 days
