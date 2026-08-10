# Load Stuff -------------------------------------------------------------------
source("1_setup.R")

# Sys.setenv(RUN_REP = 2, SEED_ENV = 1)                                         # NB: this code is designed to be run in a HPC environment, where the values of these environment variables are set in the slurm job scripts. For testing locally, it can be set here
run_rep_env = as.numeric(Sys.getenv("RUN_REP", unset = "NA"))                   # Number of times to repeat the simulation
seed_env    = as.numeric(Sys.getenv("SEED_ENV", unset = "NA"))                  # Random Number Seed

print(Sys.getenv())
print(run_rep_env)
print(seed_env)

# cmdstanr::set_cmdstan_path(path = "/home/giaco/.cmdstan/cmdstan-2.39.0")      # Desktop/Docker path - uncomment (and comment out the line below) to run locally
cmdstanr::set_cmdstan_path(path = "/users/k2583181/.cmdstan/cmdstan-2.39.0")     # KCL CREATE HPC path

# Compile stan model -----------------------------------------------------------

mod <- cmdstan_model(file.path("stan_models","stan_two_arm_bandit_v6.stan"))

# Create Parameter Table ---------------------------------------------------

# g_normaluniform(100000, .2, .4) %>% hist()


# Example of creating a list of all combinations
params_list <- expand.grid(
  n_pps               = c(60,120),
  n_trials            = c(90,180,320), 
  # n_trials          = c(200),
  learning_rate_mean  = 0.2,
  learning_rate_sd    = c(0, .25, .5),
  decision_noise_mean = .75,
  decision_noise_sd   = .25,
  prob_real           = .75,    # probability of outcome 2 
  run_rep = 1:run_rep_env  
) 

# Note that above aren't the learning rate sd, to work it out use:
# sd(g_normaluniform(400000000, .5, learning_rate_sd)

print(params_list)
print(run_rep_env)
print(seed_env)

# Run code in parallel using future --------------------------------------------
print(availableCores())

future::plan(future::multicore(workers = availableCores()))
# future::plan(future::multisession(workers =  8))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = seed_env, 1:nrow(params_list), function(i) {
  run_study3_simulation(
    i                  = i,
    n_pps              = params_list$n_pps[i], 
    n_trials           = params_list$n_trials[i], 
    learning_rate_mean = params_list$learning_rate_mean[i],
    learning_rate_sd   = params_list$learning_rate_sd[i],
    decision_noise_mean= params_list$decision_noise_mean[i],
    decision_noise_sd  = params_list$decision_noise_sd[i],
    prob_real          = params_list$prob_real[i],
    reward_outcome     = c(-1, 2),
    init_beliefs       = c(0,0),
    additional_tests = TRUE,
    save_results = FALSE
  )
}
)

time_b = Sys.time()

future::plan(future::sequential())

warnings()


timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0("study3_results_seed_", seed_env ,"_",timestamp,".rds")

print(filename)
print(time_b - time_a)

saveRDS(results, file = file.path("results","study3_results", filename))
