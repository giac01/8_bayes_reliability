# Split-half reliability for the bandit task: instead of correlating parameter estimates across
# the two test-retest SESSIONS (02_01_pike_2026_fit_bandit_models.R), this splits each subject's
# trials into first-half/second-half within EACH session and fits model_3_noncentered.stan (the
# same, unmodified model) to each half separately. Just the data prep + fitting -- the actual
# split-half reliability estimate (correlating lrR/lrP/ivT posterior means across each session's
# two saved fits) is computed in 02_01_pike_2026_bandit.qmd, alongside the test-retest and rmu
# numbers, not here.
#
# Deliberately first/second-half, not odd/even, and deliberately no change to
# model_3_noncentered.stan. Unlike the explore task's model_4_2.stan (a probit regression on
# Kalman-filter covariates precomputed once, outside Stan, over each subject's full trial
# history -- see 04_02_pike_2026_fit_explore_splithalf_models.R), this model updates its value
# table v INSIDE the Stan model block, trial by trial:
#   v[choice] += lrR/lrP * (reward/penalty - v[choice])
# Interleaving odd/even trials would re-index the retained trials as if they occurred
# back-to-back, silently skipping the real reward/penalty the subject received on every excluded
# trial before their next real choice -- a materially different (counterfactual) learning
# trajectory, not a fair split of the same one. A first/second-half split keeps each half's
# trials CONTIGUOUS in their true order, so within a half the value update from one retained
# trial to the next always uses the real, adjacent outcome. The one thing it doesn't preserve is
# carrying the first half's accumulated value table into the second half -- each half is fit as
# its own complete sequence starting from Vinits, same as the model already assumes for a full
# session. That's a real approximation (second-half "learning" restarts from scratch rather than
# continuing from wherever the subject's beliefs actually were at trial 101), but it's the
# standard practical compromise for split-half reliability of a sequential RL model without
# editing the Stan code to add a likelihood mask (which is what a fully faithful version would
# need: keep updating v on every trial using the complete history, but only accumulate the
# likelihood for the trials in a given half).
#
# bandit.csv/bandit2.csv have trial_nr 0-199 (200 trials/subject), contiguous and uniform across
# all 115 subjects in both sessions (verified). bandit_datalist() (utils.R) computes T/Tsubj as
# max(trial_nr) + 1, which only matches the actual trial count when trial_nr is 0-indexed and
# gap-free -- true for the first half (trial_nr 0-99) as filtered, but the second half (trial_nr
# 100-199) needs trial_nr re-based to 0-99 first, or T/Tsubj would come out as 200 instead of 100.
#
# adapt_delta/inits/iter counts mirror 02_01_pike_2026_fit_bandit_models.R's own established fix
# for this exact model (see that script's header comment for the funnel/multimodality diagnostics
# that motivated it) -- reused as-is for each (shorter) half.
#
# Run this once separately (e.g. `Rscript 02_04_pike_2026_fit_bandit_splithalf_models.R`, or
# inside the bignardig/tidyverse461:v4 container). All four fits (t1 first/second, t2
# first/second) run via parallel::mcparallel (fork-based -- Linux/macOS only), each still using
# parallel_chains = 4 internally, for 16 concurrent chain processes total. Adjust chains_per_fit
# below if you don't have that many cores available.

library(tidyverse)
library(cmdstanr)
library(parallel)
setwd("..")

data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

source(file.path(data_dir, "utils.R")) # provides bandit_datalist()

bandit_raw_t1 = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
bandit_raw_t2 = read_csv(file.path(data_dir, "bandit2.csv"), show_col_types = FALSE)

# same N = 115 test-retest subsample as 02_01_pike_2026_fit_bandit_models.R
ids_t2 = bandit_raw_t2 %>% distinct(id) %>% arrange(id) %>% pull(id)
ids_t1 = bandit_raw_t1 %>% filter(id %in% ids_t2) %>% distinct(id) %>% arrange(id) %>% pull(id)
stopifnot(identical(ids_t1, ids_t2))

saveRDS(ids_t1, file.path(fits_dir, "bandit_splithalf_ids.RDS"))

# trial_nr is 0-199 (200 trials/subject) for every one of these 115 subjects, in both sessions --
# first half = trial_nr < 100 (already 0-indexed, feeds bandit_datalist() unmodified), second
# half = trial_nr >= 100, re-based to 0-indexed so bandit_datalist()'s T = max(trial_nr) + 1 comes
# out as 100, not 200 (see header comment). bandit_datalist() does its own de-duplication, so no
# need to dedup here first.
split_half = function(raw, ids) {
  d = raw %>% filter(id %in% ids) %>% arrange(id)
  list(
    first  = d %>% filter(trial_nr < 100),
    second = d %>% filter(trial_nr >= 100) %>% mutate(trial_nr = trial_nr - 100)
  )
}

t1_halves = split_half(bandit_raw_t1, ids_t2)
t2_halves = split_half(bandit_raw_t2, ids_t2)

# bandit_datalist() returns rwd/plt/choice as tibbles (from pivot_wider); cmdstanr doesn't coerce
# data.frame elements passed directly as an R list to $sample(), so convert to plain matrices
# first (same helper used throughout this project's fit scripts)
to_stan_data = function(data_list) {
  map(data_list, function(x) if (is.data.frame(x)) as.matrix(x) else x)
}

t1_first_data  = t1_halves$first  %>% bandit_datalist() %>% to_stan_data()
t1_second_data = t1_halves$second %>% bandit_datalist() %>% to_stan_data()
t2_first_data  = t2_halves$first  %>% bandit_datalist() %>% to_stan_data()
t2_second_data = t2_halves$second %>% bandit_datalist() %>% to_stan_data()

# compile the model once, up front, so all forked fits below reuse the same binary instead of
# racing to compile it independently
bandit_model = cmdstan_model(file.path(data_dir, "model", "bandit_models", "model_3_noncentered_v2.stan"))

chains_per_fit = 4 # 4 fits x 4 chains = 16 concurrent processes

# same "seed near the prior" approach as 02_01_pike_2026_fit_bandit_models.R (see its header
# comment) -- reused unmodified for half-length data
make_init = function(n_subj) {
  function() {
    list(
      mu      = rnorm(3, 0, 0.1),
      sigma   = runif(3, 0.1, 0.3),
      lrR_raw = rnorm(n_subj, 0, 0.2),
      lrP_raw = rnorm(n_subj, 0, 0.2),
      ivT_raw = rnorm(n_subj, 0, 0.2)
    )
  }
}

fit_half = function(data, out_file, seed, n_subj) {
  fit = bandit_model$sample(
    data = data,
    chains = chains_per_fit,
    parallel_chains = chains_per_fit,
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    init = make_init(n_subj),
    seed = seed
  )
  fit$save_object(out_file)
  out_file
}

n_subj = length(ids_t1)

job_t1_first  = mcparallel(fit_half(t1_first_data,  file.path(fits_dir, "fit_model3nc_bandit_t1_first.RDS"),  seed = 1, n_subj = n_subj))
job_t1_second = mcparallel(fit_half(t1_second_data, file.path(fits_dir, "fit_model3nc_bandit_t1_second.RDS"), seed = 1, n_subj = n_subj))
job_t2_first  = mcparallel(fit_half(t2_first_data,  file.path(fits_dir, "fit_model3nc_bandit_t2_first.RDS"),  seed = 1, n_subj = n_subj))
job_t2_second = mcparallel(fit_half(t2_second_data, file.path(fits_dir, "fit_model3nc_bandit_t2_second.RDS"), seed = 1, n_subj = n_subj))

results = mccollect(list(job_t1_first, job_t1_second, job_t2_first, job_t2_second), wait = TRUE)

if (any(sapply(results, is.null))) {
  stop("One or more half-fits failed -- check the console output above for the underlying error.")
}

cat("Done. Saved ids + all four half-fits to", fits_dir, "\n")
