# Split-half reliability for the bandit task: instead of correlating parameter estimates across
# the two test-retest SESSIONS (02_01_pike_2026_fit_bandit_models.R), this splits each subject's
# trials into first-half/second-half within EACH session and fits hBayesDM::bandit4arm_4par() (the
# same model, unmodified) to each half separately. Just the data prep + fitting -- the actual
# split-half reliability estimate (correlating Arew/Apun/R/P posterior means across each session's
# two saved fits) is computed in 02_02_pike_2026_bandit.qmd, alongside the test-retest and rmu
# numbers, not here.
#
# Deliberately first/second-half, not odd/even. bandit4arm_4par() updates its Q-value tables
# (Qr/Qp) trial by trial inside its Stan model, and interleaving odd/even trials would re-index the
# retained trials as if they occurred back-to-back, silently skipping the real reward/punishment
# the subject received on every excluded trial before their next real choice -- a materially
# different (counterfactual) learning trajectory, not a fair split of the same one. A
# first/second-half split keeps each half's trials CONTIGUOUS in their true order, so within a half
# the value update from one retained trial to the next always uses the real, adjacent outcome. The
# one thing it doesn't preserve is carrying the first half's accumulated Q-values into the second
# half -- each half is fit as its own complete sequence starting from initV = [0,0,0,0], same as
# the model already assumes for a full session. That's a real approximation (second-half
# "learning" restarts from scratch rather than continuing from wherever the subject's beliefs
# actually were at trial 101), but it's the standard practical compromise for split-half
# reliability of a sequential RL model.
#
# bandit.csv/bandit2.csv have trial_nr 0-199 (200 trials/subject), contiguous and uniform across
# all 115 subjects in both sessions (verified) -- first half = trial_nr < 100, second half =
# trial_nr >= 100. bandit4arm_df() (data/pike_2026/utils.R) has no notion of trial_nr itself (it
# infers each subject's trial sequence from row order), so unlike the old bandit_datalist()-based
# pipeline there's no need to re-base the second half's trial_nr back to 0 -- just filter the raw
# rows before converting.
#
# Same niter/nwarmup/adapt_delta as 02_01_pike_2026_fit_bandit_models.R's full-length T1/T2 fits
# (niter = 7000, nwarmup = 3000, adapt_delta = 0.95), reused as-is for each (shorter) half.
#
# Run this once separately (e.g. `Rscript 02_01_pike_2026_fit_bandit_splithalf_models.R`, or
# inside the bignardig/tidyverse461:v5 container). All four fits (t1 first/second, t2
# first/second) run via parallel::mcparallel (fork-based -- Linux/macOS only), each still using
# ncore = 4 internally, for 16 concurrent chain processes total. Adjust chains_per_fit below if you
# don't have that many cores available.

library(tidyverse)
library(hBayesDM)
library(cmdstanr)
library(parallel)
setwd("..")

data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

source(file.path(data_dir, "utils.R")) # provides bandit4arm_df()

bandit_raw_t1 = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
bandit_raw_t2 = read_csv(file.path(data_dir, "bandit2.csv"), show_col_types = FALSE)

# same N = 115 test-retest subsample as 02_01_pike_2026_fit_bandit_models.R
ids_t2 = bandit_raw_t2 %>% distinct(id) %>% arrange(id) %>% pull(id)
ids_t1 = bandit_raw_t1 %>% filter(id %in% ids_t2) %>% distinct(id) %>% arrange(id) %>% pull(id)
stopifnot(identical(ids_t1, ids_t2))

saveRDS(ids_t1, file.path(fits_dir, "bandit_splithalf_ids.RDS"))

# trial_nr is 0-199 (200 trials/subject) for every one of these 115 subjects, in both sessions
split_half = function(raw, ids) {
  d = raw %>% filter(id %in% ids)
  list(
    first  = d %>% filter(trial_nr < 100),
    second = d %>% filter(trial_nr >= 100)
  )
}

t1_halves = split_half(bandit_raw_t1, ids_t2)
t2_halves = split_half(bandit_raw_t2, ids_t2)

t1_first_data  = bandit4arm_df(t1_halves$first,  ids_t1)
t1_second_data = bandit4arm_df(t1_halves$second, ids_t1)
t2_first_data  = bandit4arm_df(t2_halves$first,  ids_t2)
t2_second_data = bandit4arm_df(t2_halves$second, ids_t2)

# compile the model once, up front, so all forked fits below reuse the same cached binary instead
# of racing to compile it independently
bandit4arm_stan_file = system.file("stan_files", "bandit4arm_4par.stan", package = "hBayesDM")
invisible(cmdstan_model(stan_file = bandit4arm_stan_file, include_paths = dirname(bandit4arm_stan_file)))

chains_per_fit = 4 # 4 fits x 4 chains = 16 concurrent processes

fit_half = function(data, out_file, seed) {
  model_output = bandit4arm_4par(
    data        = data,
    niter       = 7000,
    nwarmup     = 3000,
    nchain      = chains_per_fit,
    ncore       = chains_per_fit,
    adapt_delta = 0.95,
    seed        = seed
  )
  model_output$fit$save_object(out_file)
  out_file
}

job_t1_first  = mcparallel(fit_half(t1_first_data,  file.path(fits_dir, "fit_bandit4arm4par_t1_first.RDS"),  seed = 1))
job_t1_second = mcparallel(fit_half(t1_second_data, file.path(fits_dir, "fit_bandit4arm4par_t1_second.RDS"), seed = 1))
job_t2_first  = mcparallel(fit_half(t2_first_data,  file.path(fits_dir, "fit_bandit4arm4par_t2_first.RDS"),  seed = 1))
job_t2_second = mcparallel(fit_half(t2_second_data, file.path(fits_dir, "fit_bandit4arm4par_t2_second.RDS"), seed = 1))

results = mccollect(list(job_t1_first, job_t1_second, job_t2_first, job_t2_second), wait = TRUE)

if (any(sapply(results, is.null))) {
  stop("One or more half-fits failed -- check the console output above for the underlying error.")
}

cat("Done. Saved ids + all four half-fits to", fits_dir, "\n")
