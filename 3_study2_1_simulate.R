# Load Stuff -------------------------------------------------------------------
source("1_setup.R")

# Sys.setenv(RUN_REP = 2, SEED_ENV = 1)                                         # NB: this code is designed to be run in a HPC environment, where the values of these environment variables are set in the slurm job scripts. For testing locally, it can be set here
run_rep_env = as.numeric(Sys.getenv("RUN_REP", unset = NA))                     # Number of times to repeat the simulation
seed_env    = as.numeric(Sys.getenv("SEED_ENV", unset = NA))                    # Random Number Seed

# cmdstanr::set_cmdstan_path(path = "/home/giaco/.cmdstan/cmdstan-2.39.0")      # Desktop/Docker path - uncomment (and comment out the line below) to run locally
cmdstanr::set_cmdstan_path(path = "/users/k2583181/.cmdstan/cmdstan-2.39.0")     # KCL CREATE HPC path

# Compile stan model -----------------------------------------------------------

example_data = sim_sdt_binomial(
  sens_mean  = 1/2,
  sens_sigma = 1/4,
  k_sigma    = 0,
  # k_mean     = params_list$k_mean[i],
  # k_sigma    = params_list$k_sigma[i],
  n_pps        = 1, 
  n_trials_old = 10,
  n_trials_new = 20)

sim_model = brms::brm(
  y | trials(n_trials) ~  1 + cond + (1 + cond | pps),
  family = binomial(link = "probit"),
  # data =  data_frame(value = 0, cond=0, pps=1),
  data = example_data$data,
  cores   = 1,
  chains  = 2,
  warmup  = 1000, 
  iter    = 2000,
  init    = .01,
  # prior = c(prior(constant(0.25), class = "b",  coef = "cond"),
  #           prior(constant(0.125), class = "sd", coef = "cond", group = "pps")),
  adapt_delta = .95,
  backend = "cmdstanr",
  # threads = threading(4)
)

# Create Parameter Table ---------------------------------------------------

params_list <- expand.grid(
  sens_mean      = c(.5),
  sens_sigma     = c(0,.2,.4),
  k_mean         = c(0),
  k_sigma        = c(.20), # removed the 0 condition
  sample_sizes = c( 50, 250, 1000),
 # sample_sizes = c( 100),
  n_items = c(10, 20, 40),
  run_rep = 1:run_rep_env  # 1 rep takes about 5 minutes (100 took 8.3 hours)
) # 8100 obs in

saveRDS(params_list, file = file.path("results","study2_results","study2_params_list.rds"))
# 
# Run code in parallel using future --------------------------------------------

future::plan(future::multicore(workers = availableCores()))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = seed_env, 1:nrow(params_list), function(i) {
  run_study2_simulation(
    i = i,
    sens_mean  = params_list$sens_mean[i],
    sens_sigma = params_list$sens_sigma[i],
    k_mean     = params_list$k_mean[i],
    k_sigma    = params_list$k_sigma[i],
    n_pps      = params_list$sample_sizes[i],
    n_items    = params_list$n_items[i],
    save_results = F
  )
}
)
time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0("study2_", seed_env ,"_",timestamp,".rds")
saveRDS(results, file = file.path("results", "study2_results", filename))




