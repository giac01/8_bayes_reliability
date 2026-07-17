# Load Stuff -------------------------------------------------------------------
source("1_setup.R")

# Sys.setenv(RUN_REP = 2, SEED_ENV = 1)                                         # NB: this code is designed to be run in a HPC environment, where the values of these environment variables are set in the slurm job scripts. For testing locally, it can be set here
run_rep_env = as.numeric(Sys.getenv("RUN_REP", unset = NA))                     # Number of times to repeat the simulation
seed_env    = as.numeric(Sys.getenv("SEED_ENV", unset = NA))                    # Random Number Seed

# Compile stan model -----------------------------------------------------------
cmdstanr::set_cmdstan_path(path = "/home/giaco/.cmdstan/cmdstan-2.39.0") # THIS NEEDS UPDATNG TO LOCATION OF CMDSTAN INSTALL ON HPC CLUSTER

mod <- cmdstan_model(file.path("stan_models","stan_inequiv_factor_model_v16.stan"))

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
  c(.1,.1,.1,.1,.1),
  c(.3,.2,.1),
  c(.4,.3,.3,.2,.1,.0),
  c(.4,.4,.4,.4),
  c(.6,.5,.4,.3,.2),
  c(.7,.6,.5,.5,.5,.4,.4,.3,.3)
)

# Reliabilities 
# Note that because the loadings aren't known exactly, any sampling error in estimating the loadings will influence the reliability of the IRT scores

lapply(loadings_list, rel_function)

# Example of creating a list of all combinations
params_list <- expand.grid(
  loading_set  = 1:length(loadings_list),
  sample_sizes = c(50, 250, 1000),
  run_rep = 1:run_rep_env  
) 

# 100 reps completed in 3 hours and 25 minutes 

# Run code in parallel using future --------------------------------------------
print(availableCores())
 
future::plan(future::multicore(workers = availableCores()))
# future::plan(future::multisession(workers = 8))

time_a = Sys.time()

results <- future.apply::future_lapply(future.seed = seed_env, 1:nrow(params_list), function(i) {
  run_study1_simulation(
    i = i,
    n = params_list$sample_sizes[i], 
    loadings = loadings_list[[params_list$loading_set[i]]],
    additional_tests = TRUE
  )
}
)

time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0("study1_", seed_env ,"_",timestamp,".rds")
saveRDS(results, file = file.path("results", "study1_results", filename))


# Time difference of 2.264748 days
