# Load Stuff -------------------------------------------------------------------
rm(list=ls(all.names = T))
gc()


run_rep_env = as.numeric(Sys.getenv("RUN_REP", unset = "NA"))
seed_env    = as.numeric(Sys.getenv("SEED_ENV", unset = "NA"))

print(Sys.getenv())
print(run_rep_env)
print(seed_env)

cmdstanr::set_cmdstan_path(path = "/home/gb424/.cmdstan/cmdstan-2.34.1")

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

mod <- cmdstan_model(file.path("stan_models","stan_two_arm_bandit_v6.stan"))

# Create Parameter Table ---------------------------------------------------

g_normaluniform(100000, .2, .4) %>% hist()


# Example of creating a list of all combinations
params_list <- expand.grid(
  n_pps               = c(70,140),
  n_trials            = c(100,200,400), 
  # n_trials          = c(200),
  learning_rate_mean  = 0.2,
  learning_rate_sd    = c(0, .20, .4),
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

 future::plan(future::multisession(workers = availableCores()))
# future::plan(future::multisession(workers =  8))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = seed_env, 1:nrow(params_list), function(i) {
  run_ri_sim(
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
print(time_b - time_a)

print("Here3321")

future::plan(future::sequential())

warnings()

print("Here11242")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0("study3_results_seed_", seed_env ,"_",timestamp,".rds")
print("Here11212322")
print(filename)
saveRDS(results, file = file.path("results",filename))
