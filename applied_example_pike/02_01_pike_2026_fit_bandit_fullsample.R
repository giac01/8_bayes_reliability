# Fits hBayesDM::bandit4arm_4par() (same model as 02_01_pike_2026_fit_bandit_models.R) to the FULL
# T1 sample -- all N = 547 subjects in bandit.csv -- rather than just the N = 115 subjects who also
# completed session 2.
#
# Motivation: gbtoolbox::reliability()'s RMU is a within-session reliability estimate computed
# purely from one fitted model's posterior draws -- it never uses session-2 data, so it doesn't
# need the test-retest-matched subsample at all. Fitting the full T1 sample instead lets
# 02_02_pike_2026_bandit.qmd report an RMU estimate at the largest N the raw data actually
# supports, for comparison against the matched-subsample RMU/test-retest/split-half numbers.
#
# In addition to the full-length fit, this also fits the full N = 547 sample split into
# first-half/second-half trials (trial_nr 0-99 / 100-199), the same within-session split-half
# approach as 02_01_pike_2026_fit_bandit_splithalf_models.R, but here applied to the full sample
# rather than the N = 115 test-retest subsample (there's no session 2 for the full sample, so
# there's nothing to build a matched-subsample split-half fit from other than this one). See that
# script's header comment for why first/second-half (not odd/even). The split-half fits are run
# first since they're the ones 02_02_pike_2026_bandit.qmd is actually waiting on; the full-length
# fit re-uses the already-compiled model afterwards.
#
# niter/nwarmup/adapt_delta mirror 02_01_pike_2026_fit_bandit_models.R's own T1/T2 fits
# (niter = 7000, nwarmup = 3000, adapt_delta = 0.95), reused as-is here. Runtime will be longer
# than the N = 115 fits purely because ~5x as many subjects means ~5x the per-iteration
# likelihood computation.
#
# The three fits run SEQUENTIALLY, each saved to disk as soon as it finishes, and any fit whose
# output file already exists is skipped -- so if the script (or the machine) dies partway, a
# rerun resumes from the first missing fit instead of redoing everything. To force a refit,
# delete the corresponding .RDS first.
#
# Run this once separately (e.g. `Rscript 02_01_pike_2026_fit_bandit_fullsample.R`, or inside the
# bignardig/tidyverse461:v5 container), then 02_02_pike_2026_bandit.qmd just reads the saved fits.
rm(list = ls())
getwd()
library(tidyverse)
library(hBayesDM)
library(cmdstanr)
setwd("..")
data_dir = file.path("data", "pike_2026")
fits_dir = file.path(data_dir, "model_fits")
dir.create(fits_dir, showWarnings = FALSE)

source(file.path(data_dir, "utils.R")) # provides bandit4arm_df()

bandit_raw_all = read_csv(file.path(data_dir, "bandit.csv"), show_col_types = FALSE)
ids_all = bandit_raw_all %>% distinct(id) %>% arrange(id) %>% pull(id)

saveRDS(ids_all, file.path(fits_dir, "bandit_ids_all.RDS"))

# trial_nr is 0-199 (200 trials/subject) -- first half = trial_nr < 100, second half =
# trial_nr >= 100 (bandit4arm_df() infers trial order from row order, not from trial_nr itself, so
# unlike the old bandit_datalist()-based pipeline there's no need to re-base trial_nr)
split_half = function(raw) {
  list(
    first  = raw %>% filter(trial_nr < 100),
    second = raw %>% filter(trial_nr >= 100)
  )
}

all_halves = split_half(bandit_raw_all)

all_first_data  = bandit4arm_df(all_halves$first,  ids_all)
all_second_data = bandit4arm_df(all_halves$second, ids_all)
all_data        = bandit4arm_df(bandit_raw_all,    ids_all)

# compile the model once, up front, so both the split-half fits and the full-length fit below
# reuse the same cached binary instead of recompiling
bandit4arm_stan_file = system.file("stan_files", "bandit4arm_4par.stan", package = "hBayesDM")
invisible(cmdstan_model(stan_file = bandit4arm_stan_file, include_paths = dirname(bandit4arm_stan_file)))

# --- memory fix: patch hBayesDM's draws extraction --------------------------------------------
# hBayesDM's internal .hbayesdm_extract() calls fit$draws() with NO variable filter, which
# force-reads every variable in the CmdStan CSVs -- including the N x T y_pred posterior
# predictions in generated quantities -- and converts the lot to rvars. At N = 547 that
# transiently needs more RAM than this machine has: every run between 2026-08-18 and 2026-08-25
# died to the kernel OOM killer either here or in the equally unfiltered $save_object().
# cmdstanr's CSV reader honours `variables` with a column-select (data.table::fread select=),
# so restricting the read to the parameters hBayesDM actually extracts keeps peak memory at a
# few hundred MB. Nothing downstream ever uses y_pred.
assignInNamespace(".hbayesdm_extract", function(fit, pars) {
  pars_found = intersect(pars, fit$metadata()$stan_variables)
  draws = posterior::as_draws_rvars(fit$draws(variables = pars_found))
  out = list()
  for (p in pars) {
    if (!is.null(draws[[p]])) {
      out[[p]] = posterior::draws_of(draws[[p]], with_chains = FALSE)
    }
  }
  out
}, ns = "hBayesDM")

# --- saving helper ----------------------------------------------------------------------------
# 02_02_pike_2026_bandit.qmd only ever reads these variables (plus sampler diagnostics) from the
# saved fits, so cache exactly these on the fit object and serialize it directly instead of using
# $save_object() (which would force-read every variable first, y_pred included). The saved object
# supports the same $summary()/$draws(variables = ...)/$diagnostic_summary() calls the notebook
# makes; only variables never cached (y_pred, the *_pr raws) are unavailable from it.
keep_vars = c("mu_pr", "sigma", "Arew", "Apun", "R", "P", "lp__")

save_fit_trimmed = function(fit, out_file) {
  fit$draws(variables = keep_vars) # incremental: only reads whatever the extract patch didn't already cache
  try(fit$sampler_diagnostics(), silent = TRUE)
  try(fit$init(), silent = TRUE)
  # write to a temp name and rename, so out_file only ever exists complete -- a crash mid-save
  # can't leave behind a truncated RDS for the file.exists() resume check below to trust
  tmp_file = paste0(out_file, ".tmp")
  saveRDS(fit, tmp_file)
  file.rename(tmp_file, out_file)
}

fit_and_save = function(data, out_file, seed) {
  if (file.exists(out_file)) {
    cat("Skipping", basename(out_file), "-- already exists\n")
    return(invisible(NULL))
  }
  model_output = bandit4arm_4par(
    data        = data,
    # niter       = 7000,
    niter       = 9000, # For all_data model
    nwarmup     = 3000,
    nchain      = 4,
    ncore       = 4,
    # adapt_delta = 0.95,
    adapt_delta = 0.975, # For all_data model
    seed        = seed
  )
  save_fit_trimmed(model_output$fit, out_file)
  rm(model_output)
  invisible(gc())
}

# --- run the three fits sequentially ----------------------------------------------------------
fit_and_save(all_first_data,  file.path(fits_dir, "fit_bandit4arm4par_fullsample_first.RDS"),  seed = 1)
fit_and_save(all_second_data, file.path(fits_dir, "fit_bandit4arm4par_fullsample_second.RDS"), seed = 1)
fit_and_save(all_data,        file.path(fits_dir, "fit_bandit4arm4par_fullsample.RDS"),        seed = 1)

cat("Done. Saved ids + split-half fits + full-length fit to", fits_dir, "\n")
