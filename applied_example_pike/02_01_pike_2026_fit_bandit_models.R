# Fits model_3_noncentered.stan -- a non-centered reparameterization of the authors'
# model_3.stan bandit RL model (Vincent Valton) -- on the N = 115 subjects who completed BOTH
# sessions of the bandit task (bandit_t1.json / bandit_t2.json).
#
# The original model_3.stan draws lrR/lrP/tau directly from Beta/Gamma hyperpriors (a centered
# parameterization), which produced a funnel-geometry convergence problem on this data: rhat up
# to 1.74, ESS as low as ~6, low E-BFMI, concentrated on lrP/tau/inv_temp/log_lik plus every
# group-level hyperparameter. model_3_noncentered.stan reparameterizes non-centered (mu/sigma +
# Phi_approx), matching the style already used in model/rbias/model_3.stan, which doesn't show
# these problems. See the diagnostics section of 02_01_pike_2026_bandit.qmd for details.
#
# That reparameterization alone left genuine multimodality (zero divergences, but rhat up to 2.64
# / ESS as low as ~5) in a handful of weakly-identified subjects' lrP/tau. sigma's prior was
# tightened cauchy(0, 2.5) -> normal(0, 0.2) in the .stan file to shrink those subjects harder
# toward the population mean, and adapt_delta/iter_warmup/iter_sampling are bumped a bit below as
# cheap insurance -- though since there were no divergences to begin with, the prior change is
# expected to be what actually fixes convergence. Chain inits are also seeded near the prior
# (make_init() below) so no chain starts from a wildly implausible corner.
#
# This is split out from 02_01_pike_2026_bandit.qmd because sampling is slow -- run this once
# separately (e.g. `Rscript 02_01_pike_2026_fit_bandit_models.R`, or inside the
# bignardig/tidyverse461:v4 container), then 02_01_pike_2026_bandit.qmd just reads the saved fits.
#
# The two sessions are fit in parallel (parallel::mcparallel, fork-based -- Linux/macOS only),
# each still using parallel_chains = 4 internally, for 8 concurrent chain processes total.
# Adjust chains_per_fit below if you don't have 8 cores available. Mirrors
# 03_01_pike_2026_fit_rbias_models.R.
rm(list = ls())
getwd()
library(tidyverse)
library(cmdstanr)
library(parallel)
setwd("..")
data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

# subject IDs, in the sorted order used to build bandit_t1.json / bandit_t2.json
# (see Datalists.Rmd: both were built via arrange(id) %>% bandit_datalist())
bandit_raw_t1 = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
bandit_raw_t2 = read_csv(file.path(data_dir, "bandit2.csv"), show_col_types = FALSE)

ids_t2 = bandit_raw_t2 %>% distinct(id) %>% arrange(id) %>% pull(id)
ids_t1 = bandit_raw_t1 %>% filter(id %in% ids_t2) %>% distinct(id) %>% arrange(id) %>% pull(id)
stopifnot(identical(ids_t1, ids_t2))

saveRDS(ids_t1, file.path(fits_dir, "bandit_ids_t1t2.RDS"))

# compile the non-centered model once, up front, so both forked fits below reuse the same
# binary instead of racing to compile it independently
bandit_model = cmdstan_model(file.path(data_dir, "model", "bandit_models", "model_3_noncentered_v2.stan"))

chains_per_fit = 4 # 2 fits x 4 chains = 8 concurrent processes

# cmdstanr's default init draws each unconstrained parameter ~ uniform(-2, 2), which for
# sigma (lower=0, exp-transformed) reaches as high as ~7.4 -- wildly inconsistent with the new
# normal(0, 0.2) prior, and a bad enough draw can start a chain in the "wrong" basin of the
# lrP/tau multimodality before warmup even gets going. Initializing every chain near the prior
# (mu/subject-level raw scores close to 0, sigma close to its prior scale), with only small jitter
# so the chains aren't literally identical, keeps all chains starting from the same
# prior-consistent region instead of scattering across it.
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

fit_session = function(data, out_file, seed, n_subj) {
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

# fit both sessions at once (each internally parallel over its own chains); note console
# output from the two fits' sampling progress will interleave
job_t1 = mcparallel(fit_session(file.path(data_dir, "bandit_t1.json"), file.path(fits_dir, "fit_model3nc_bandit_t1.RDS"), seed = 1, n_subj = length(ids_t1)))
job_t2 = mcparallel(fit_session(file.path(data_dir, "bandit_t2.json"), file.path(fits_dir, "fit_model3nc_bandit_t2.RDS"), seed = 1, n_subj = length(ids_t1)))

results = mccollect(list(job_t1, job_t2), wait = TRUE)

if (any(sapply(results, is.null))) {
  stop("One or both model fits failed -- check the console output above for the underlying error.")
}

cat("Done. Saved ids + both fits to", fits_dir, "\n")
