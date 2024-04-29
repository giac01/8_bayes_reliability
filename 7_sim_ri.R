# Load Stuff -------------------------------------------------------------------
rm(list=ls(all.names = T))
gc()

cmdstanr::set_cmdstan_path(path = "/home/gb424/.cmdstan/cmdstan-2.34.1")

library(brms)
library(tidyverse)
library(future)
library(future.apply)
library(cmdstanr)

seedval = 1

# Source all simulation functions 
# List all .R files in the folder
list.files(file.path("helper_functions"), pattern = "\\.R$", full.names = TRUE) %>%
  lapply(., function(x) {source(x)})

# Compile stan model -----------------------------------------------------------

mod <- cmdstan_model(file.path("stan_models","stan_two_arm_bandit_v6.stan"))

# Create Parameter Table ---------------------------------------------------

# Example of creating a list of all combinations
params_list <- expand.grid(
  #n_pps               = c(200, 500, 1000),
  n_pps               = c(200),
  n_trials            = c(100, 200, 400),
  # n_trials            = c(200),
  learning_rate_mean  = 0.5,
  learning_rate_sd    = c(0, .2, 1),
  # learning_rate_sd    = c(.2),
  decision_noise_mean = .75,
  decision_noise_sd   = .25,
  prob_real           = c(.75),    # probability of outcome 2 
  run_rep = 1:1000 # 50 takes 11 hours  (FULLRANK) Takes 3.2 hours with meanfield (.0001) 
                   # 
                   # 100 takes 1.25 days (.00001)
) 

# Run code in parallel using future --------------------------------------------
print(availableCores())

 future::plan(future::multisession(workers = availableCores()))
#future::plan(future::multisession(workers =  8))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = seedval, 1:nrow(params_list), function(i) {
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
time_b - time_a

future::plan(future::sequential())

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0("7_results_ri_seed", seedval ,"_",timestamp,".rds")
saveRDS(results, file = file.path("results", filename))



# Time difference of 2.264748 days

# results[[5]]$stan_results$summary() %>%
#   slice(which(!grepl("^A\\[",.$variable))) %>%
#   slice(which(!grepl("^tau_unscaled\\[",.$variable))) %>%
#   slice(which(!grepl("^tau\\[",.$variable))) %>%
#   slice(which(!grepl("_z",.$variable))) %>%
#   slice(which(!grepl("learning_rate\\[",.$variable))) %>%
#   slice(which(!grepl("^decision_noise\\[",.$variable))) 
