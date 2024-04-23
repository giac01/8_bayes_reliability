# Load Stuff -------------------------------------------------------------------
rm(list=ls(all.names = T))
gc()

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

# Example of creating a list of all combinations
params_list <- expand.grid(
  n_pps               = c(1000),
  n_trials            = c(100, 200, 400),
  learning_rate_mean  = 0.5,
  learning_rate_sd    = c(0, 0.15, .29),
  decision_noise_mean = .75,
  decision_noise_sd   = .25,
  prob_real           = c(.75),    # probability of outcome 2 
  run_rep = 1:50 # 100 took 2.5 hours 260 took 1.9 hours when not sourcing, weird!!!
) 

# Run code in parallel using future --------------------------------------------
print(availableCores())

# future::plan(future::multisession(workers = availableCores()))
future::plan(future::multisession(workers =  3))
# future::plan(future::sequential())

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = 10, 1:nrow(params_list), function(i) {
  tryCatch({
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
      n_draws            = 250,
      additional_tests = FALSE,
      save_results = FALSE
    )
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    return(list(error = paste("Error in iteration", i, ":", e$message)))  # Return an error message or other placeholder
  })
}
)

time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

#saveRDS(results, file = file.path("results","4_results_tauinequiv_aa.rds"))

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- sprintf("7_results_ri_pop_%s.rds", timestamp)
saveRDS(results, file = file.path("results", filename))


# Time difference of 2.264748 days



# Quick checker

sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
rmp_est = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
rmp_lb = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
rmp_ub = sapply(results, function(x) x$rmp[3]) %>% as.numeric()

data.frame(rmp_est, rmp_lb, rmp_ub) %>%
  arrange(rmp_est) %>%
  mutate(i = 1:nrow(.)) %>%
  ggplot(aes(x = i, y = rmp_est, ymin = rmp_lb, ymax = rmp_ub)) + 
  geom_errorbar() + 
  geom_hline(yintercept = .510, col = "red")

lapply(results, function(x) x$stan_results_summary)

plot(sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric(),
     sapply(results, function(x) x$rmp[1]) %>% as.numeric())
