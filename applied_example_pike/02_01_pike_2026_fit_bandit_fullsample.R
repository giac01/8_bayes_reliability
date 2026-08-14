# Fits model_3_noncentered_v2.stan (same model as 02_01_pike_2026_fit_bandit_models.R) to the
# FULL T1 sample -- all N = 547 subjects in bandit_all.json -- rather than just the N = 115
# subjects who also completed session 2 (bandit_t1.json).
#
# Motivation: gbtoolbox::reliability()'s RMU is a within-session reliability estimate computed
# purely from one fitted model's posterior draws -- it never uses session-2 data, so it doesn't
# need the test-retest-matched subsample at all. Fitting the full T1 sample instead lets
# 02_01_pike_2026_bandit.qmd report an RMU estimate at the largest N the raw data actually
# supports, for comparison against the matched-subsample RMU/test-retest/split-half numbers.
# bandit_all.json was built in the authors' Datalists.Rmd as
# `bandit_datalist(bandit %>% arrange(id))` -- i.e. every subject in bandit.csv, unfiltered --
# whereas bandit_t1.json additionally filters to `id %in% bandit2$id`.
#
# In addition to the full-length fit, this also fits the full N = 547 sample split into
# first-half/second-half trials (trial_nr 0-99 / 100-199), the same within-session split-half
# approach as 02_01_pike_2026_fit_bandit_splithalf_models.R, but here applied to the full sample
# rather than the N = 115 test-retest subsample (there's no session 2 for the full sample, so
# there's nothing to build a matched-subsample split-half fit from other than this one). See that
# script's header comment for why first/second-half (not odd/even) and why no change to the Stan
# model. The split-half fits are run first since they're the ones 02_02_pike_2026_bandit.qmd is
# actually waiting on; the full-length fit re-uses the already-compiled model afterwards.
#
# adapt_delta/iter counts/inits mirror 02_01_pike_2026_fit_bandit_models.R's own established fix
# for this exact model (see that script's header comment for the funnel/multimodality diagnostics
# that motivated it), reused as-is here. Runtime will be longer than the N = 115 fits purely
# because ~5x as many subjects means ~5x the per-iteration likelihood computation.
#
# Run this once separately (e.g. `Rscript 02_01_pike_2026_fit_bandit_fullsample.R`, or inside the
# bignardig/tidyverse461:v5 container), then 02_02_pike_2026_bandit.qmd just reads the saved fits.
rm(list = ls())
getwd()
library(tidyverse)
library(cmdstanr)
library(parallel)
setwd("..")
data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

source(file.path(data_dir, "utils.R")) # provides bandit_datalist()

# subject IDs, in the sorted order used to build bandit_all.json (see Datalists.Rmd:
# bandit_datalist(bandit %>% arrange(id)) -- pivot_wider's id_cols then places subjects in
# ascending id order)
bandit_raw_all = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
ids_all = bandit_raw_all %>% distinct(id) %>% arrange(id) %>% pull(id)

saveRDS(ids_all, file.path(fits_dir, "bandit_ids_all.RDS"))

# trial_nr is 0-199 (200 trials/subject) -- first half = trial_nr < 100 (already 0-indexed, feeds
# bandit_datalist() unmodified), second half = trial_nr >= 100, re-based to 0-indexed so
# bandit_datalist()'s T = max(trial_nr) + 1 comes out as 100, not 200 (see
# 02_01_pike_2026_fit_bandit_splithalf_models.R's header comment for the full explanation).
# bandit_datalist() does its own de-duplication, so no need to dedup here first.
split_half = function(raw) {
  d = raw %>% arrange(id)
  list(
    first  = d %>% filter(trial_nr < 100),
    second = d %>% filter(trial_nr >= 100) %>% mutate(trial_nr = trial_nr - 100)
  )
}

all_halves = split_half(bandit_raw_all)

# bandit_datalist() returns rwd/plt/choice as tibbles (from pivot_wider); cmdstanr doesn't coerce
# data.frame elements passed directly as an R list to $sample(), so convert to plain matrices
# first (same helper used throughout this project's fit scripts)
to_stan_data = function(data_list) {
  map(data_list, function(x) if (is.data.frame(x)) as.matrix(x) else x)
}

all_first_data  = all_halves$first  %>% bandit_datalist() %>% to_stan_data()
all_second_data = all_halves$second %>% bandit_datalist() %>% to_stan_data()

# compile the model once, up front, so both the split-half fits and the full-length fit below
# reuse the same binary instead of recompiling
bandit_model = cmdstan_model(file.path(data_dir, "model", "bandit_models", "model_3_noncentered_v2.stan"))

# same "seed near the prior" approach as 02_01_pike_2026_fit_bandit_models.R (see its header
# comment) -- reused unmodified
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

n_subj = length(ids_all)

# --- split-half fits (first) -----------------------------------------------------------------
# same higher adapt_delta as 02_01_pike_2026_fit_bandit_splithalf_models.R's half-length fits
chains_per_fit = 4 # 2 fits x 4 chains = 8 concurrent processes

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

job_all_first  = mcparallel(fit_half(all_first_data,  file.path(fits_dir, "fit_model3nc_bandit_fullsample_first.RDS"),  seed = 1, n_subj = n_subj))
job_all_second = mcparallel(fit_half(all_second_data, file.path(fits_dir, "fit_model3nc_bandit_fullsample_second.RDS"), seed = 1, n_subj = n_subj))

half_results = mccollect(list(job_all_first, job_all_second), wait = TRUE)

if (any(sapply(half_results, is.null))) {
  stop("One or more full-sample split-half fits failed -- check the console output above for the underlying error.")
}

# --- full-length fit (after split-half) -------------------------------------------------------
fit_bandit_all = bandit_model$sample(
  data = file.path(data_dir, "bandit_all.json"),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  adapt_delta = 0.98,
  init = make_init(n_subj),
  seed = 1
)

fit_bandit_all$save_object(file.path(fits_dir, "fit_model3nc_bandit_fullsample.RDS"))

cat("Done. Saved ids + split-half fits + full-length fit to", fits_dir, "\n")
