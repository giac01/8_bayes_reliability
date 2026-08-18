# Load Stuff -------------------------------------------------------------------
source("1_setup.R")

# Sys.setenv(COMBO_ENV = 1, SEED_ENV = 1001)                                    # NB: this code is designed to be run in a HPC environment, where the values of these environment variables are set in the slurm job script (6_study3_0_slurm). For testing locally, it can be set here.
combo_env = as.numeric(Sys.getenv("COMBO_ENV", unset = "NA"))                   # Which of the 9 n_trials x learning_rate_sd combos to run this task (1-9) - see combo_grid below
seed_env  = as.numeric(Sys.getenv("SEED_ENV", unset = "NA"))                    # Random number seed for this single simulation

print(Sys.getenv())
print(combo_env)
print(seed_env)

# cmdstanr::set_cmdstan_path(path = "/home/giaco/.cmdstan/cmdstan-2.39.0")      # Desktop/Docker path - uncomment (and comment out the line below) to run locally
cmdstanr::set_cmdstan_path(path = "/users/k2583181/.cmdstan/cmdstan-2.39.0")     # KCL CREATE HPC path

# Compile stan model -----------------------------------------------------------

mod <- cmdstan_model(file.path("stan_models","stan_two_arm_bandit_v6.stan"))

# Select this task's single param combo -----------------------------------------

# Unlike 4_study3_1_simulate.R/5_study3_1_simulate_320trials.R, each array task
# here runs exactly ONE simulation instead of looping over several rows/reps.
# n_pps=240 is double the largest n_pps tried so far in study3, and even a
# single n_trials=320 simulation at n_pps<=120 was already observed to
# sometimes take >12h to fit (see 5_study3_1_simulate_320trials.R) - looping
# multiple rows per task risks losing the whole task's results to a timeout,
# so 6_study3_0_slurm gives every (n_trials, learning_rate_sd, rep) triple its
# own array task. combo_env (1-9) selects a row below; SEED_ENV both seeds the
# RNG and identifies the rep. The slurm script derives both from
# SLURM_ARRAY_TASK_ID.
combo_grid <- data.frame(
  n_trials         = c(90, 90, 90,   180, 180, 180,   320, 320, 320),
  learning_rate_sd = c(0, .25, .5,   0, .25, .5,       0, .25, .5)
)

if (is.na(combo_env) || combo_env < 1 || combo_env > nrow(combo_grid)) {
  stop("COMBO_ENV must be an integer between 1 and ", nrow(combo_grid))
}
if (is.na(seed_env)) stop("SEED_ENV is not set")

this_row <- combo_grid[combo_env, ]

print(this_row)

# Run simulation -----------------------------------------------------------

set.seed(seed_env)

time_a = Sys.time()

result <- run_study3_simulation(
  i                   = seed_env,
  n_pps               = 240,
  n_trials            = this_row$n_trials,
  learning_rate_mean  = 0.2,
  learning_rate_sd    = this_row$learning_rate_sd,
  decision_noise_mean = .75,
  decision_noise_sd   = .25,
  prob_real           = .75,    # probability of outcome 2
  reward_outcome      = c(-1, 2),
  init_beliefs        = c(0,0),
  additional_tests    = TRUE,
  save_results        = FALSE
)

time_b = Sys.time()

warnings()

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # This will create a timestamp in the format "YYYYMMDD_HHMMSS"
filename <- paste0(
  "study3_results_240pps_ntrials", this_row$n_trials,
  "_lrsd", this_row$learning_rate_sd,
  "_seed_", seed_env, "_", timestamp, ".rds"
)

print(filename)
print(time_b - time_a)

# Wrapped in a list so each file's shape matches the per-file "list of
# simulation results" that 4_study3_2_analysis.R expects (it reads every file,
# then flattens one level with do.call("c", ...)), even though this file holds
# only a single simulation.
saveRDS(list(result), file = file.path("results","study3_results", filename))
