# Fits hBayesDM::bandit4arm_4par() -- the 4-parameter model (Arew: reward learning rate, Apun:
# punishment learning rate, R: reward sensitivity, P: punishment sensitivity) from Seymour, Daw,
# Roiser, Dayan, & Dolan (2012) -- on the N = 115 subjects who completed BOTH sessions of the
# bandit task (bandit.csv / bandit2.csv).
#
# This replaces an earlier version of this script that fit a custom non-centered Stan model
# (model_3_noncentered_v2.stan, 3 parameters: lrR, lrP, ivT, no separate reward/punishment
# sensitivity -- see git history for that model and the convergence diagnostics that motivated its
# non-centered reparameterization). bandit4arm_4par() ships its own non-centered parameterization
# (Phi_approx, sigma ~ normal(0, 0.2)) and handles compilation, initial values (a variational-
# inference warm start by default) and posterior extraction internally -- see
# 02_02_pike_2026_bandit.qmd for the full model description and diagnostics.
#
# hBayesDM's model wants one row per trial with columns subjID/choice/gain/loss (not the authors'
# T x N matrices used by bandit_datalist()), built here directly from the raw csvs via
# bandit4arm_df() (data/pike_2026/utils.R).
#
# Run this once separately (e.g. `Rscript 02_01_pike_2026_fit_bandit_models.R`, or inside the
# bignardig/tidyverse461:v5 container), then 02_02_pike_2026_bandit.qmd just reads the saved fits.
#
# The two sessions are fit in parallel (parallel::mcparallel, fork-based -- Linux/macOS only),
# each still using ncore = 4 internally, for 8 concurrent chain processes total. Adjust
# chains_per_fit below if you don't have 8 cores available. Mirrors
# 02_01_pike_2026_fit_bandit_fullsample.R / 02_01_pike_2026_fit_bandit_splithalf_models.R.
rm(list = ls())
getwd()
library(tidyverse)
library(hBayesDM)
library(cmdstanr)
library(parallel)
setwd("..")
data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

source(file.path(data_dir, "utils.R")) # provides bandit4arm_df()

# subject IDs, in the sorted order bandit4arm_df() arranges each session's data.frame by -- since
# hBayesDM groups subjects by first appearance in the data, sorting the input up front keeps its
# internal subject order identical to this one
bandit_raw_t1 = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
bandit_raw_t2 = read_csv(file.path(data_dir, "bandit2.csv"), show_col_types = FALSE)

ids_t2 = bandit_raw_t2 %>% distinct(id) %>% arrange(id) %>% pull(id)
ids_t1 = bandit_raw_t1 %>% filter(id %in% ids_t2) %>% distinct(id) %>% arrange(id) %>% pull(id)
stopifnot(identical(ids_t1, ids_t2))

saveRDS(ids_t1, file.path(fits_dir, "bandit_ids_t1t2.RDS"))

t1_df = bandit4arm_df(bandit_raw_t1, ids_t1)
t2_df = bandit4arm_df(bandit_raw_t2, ids_t2)

# compile bandit4arm_4par's Stan model once, up front, via the same file cmdstanr keys its
# compilation cache on, so the two forked fits below reuse the cached binary instead of racing to
# compile it independently
bandit4arm_stan_file = system.file("stan_files", "bandit4arm_4par.stan", package = "hBayesDM")
invisible(cmdstan_model(stan_file = bandit4arm_stan_file, include_paths = dirname(bandit4arm_stan_file)))

chains_per_fit = 4 # 2 fits x 4 chains = 8 concurrent processes

fit_session = function(data, out_file, seed) {
  model_output = bandit4arm_4par(
    data        = data,
    niter       = 8000,
    nwarmup     = 3000,
    nchain      = chains_per_fit,
    ncore       = chains_per_fit,
    adapt_delta = 0.975,
    seed        = seed
  )
  model_output$fit$save_object(out_file)
  out_file
}

# fit both sessions at once (each internally parallel over its own chains); note console output
# from the two fits' sampling progress will interleave
job_t1 = mcparallel(fit_session(t1_df, file.path(fits_dir, "fit_bandit4arm4par_t1.RDS"), seed = 1))
job_t2 = mcparallel(fit_session(t2_df, file.path(fits_dir, "fit_bandit4arm4par_t2.RDS"), seed = 1))

results = mccollect(list(job_t1, job_t2), wait = TRUE)

if (any(sapply(results, is.null))) {
  stop("One or both model fits failed -- check the console output above for the underlying error.")
}

cat("Done. Saved ids + both fits to", fits_dir, "\n")
