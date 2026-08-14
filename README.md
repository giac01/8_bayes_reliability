# Code and results for Relative Measurement Uncertainty Paper

[![Preprint](https://img.shields.io/badge/preprint-PsyArXiv-1E90FF)](https://osf.io/preprints/psyarxiv/h54k8_v1)
![License](https://img.shields.io/github/license/giac01/8_bayes_reliability)
![Docker](https://img.shields.io/badge/docker-bignardig%2Ftidyverse461%3Av1-2496ED?logo=docker&logoColor=white)
![R](https://img.shields.io/badge/R-4.6.1-276DC3?logo=r&logoColor=white)
![Stan](https://img.shields.io/badge/Stan-brms%20%2F%20cmdstanr-B2001D?logo=stan&logoColor=white)
![Last Commit](https://img.shields.io/github/last-commit/giac01/8_bayes_reliability)
![Repo Size](https://img.shields.io/github/repo-size/giac01/8_bayes_reliability)

Code for simulation studies and analyses accompanying the preprint:

> **A general method for estimating reliability using Bayesian Measurement Uncertainty**
> Giacomo Bignardi, Rogier Kievit, Paul-Christian Bürkner
> PsyArXiv, September 30, 2025. [osf.io/preprints/psyarxiv/h54k8_v1](https://osf.io/preprints/psyarxiv/h54k8_v1)

<details>
<summary>Abstract</summary>

Unreliable measurement can lead to lower statistical power, attenuated effect sizes and residual confounding. However, estimating reliability can be challenging for complex cognitive and behavioural assessments without test-retest data. Most statistical methods for assessing reliability with a single test administration are designed for fixed-item questionnaires. We introduce a novel Bayesian procedure called relative measurement uncertainty (RMU) for estimating reliability that can be broadly applied, including to many widely used computational models. Our approach draws pairs of samples from each subject's posterior and calculates the correlation between draws. We demonstrate analytically and via simulation that this method provides accurate reliability estimates and well-calibrated credible intervals across linear factor, signal detection and reinforcement learning models. Simulations found that RMU was more accurate (lower root mean square error) than existing measures (coefficient alpha, coefficient H, split-half), but more importantly, it can be applied across a wide range of computational models. RMU offers a general method for estimating reliability, leveraging the modelling flexibility of Bayesian statistical methods.

</details>

## Environment

This project runs in the Docker container **`bignardig/tidyverse461:v1`**, based on **R version 4.6.1**.

> [!NOTE]
> Study 3 (`4_study3_*`) requires **`bignardig/tidyverse461:v3`** instead, which adds the `libtbb-dev` system library needed to run its code.

> [!WARNING]
> This repository is actively being updated. Scripts, file names, and structure are being reorganized and may change without notice — expect breaking changes until this warning is removed.

## Structure

- `1_setup.R` — loads packages and sources all helper functions in `helper_functions/`
- `2_*`, `3_*`, `4_*`, `5_*` — simulation and analysis scripts for each study (see below)
- `helper_functions/` — shared R functions (simulation, model-fitting, and reliability-estimation helpers)
- `stan_models/` — Stan model files
- `data/` — study data (not tracked in git)
- `results/`, `results_tables/` — simulation outputs

There are three studies, each simulating data from a different measurement model, fitting it with Stan/brms, and computing RMU reliability alongside classical benchmarks (coefficient alpha, coefficient H, split-half). Each study has a `_0_slurm` job script (submits the simulation to an HPC cluster), a `_1_simulate.R` script (runs the simulation for one seed/job), and a `_2_analysis.R` script (collates and summarises results across jobs).

### Study 1 — Linear factor model (`2_study1_*`)

Simulates single-factor data across a range of sample sizes and loading patterns, fits the Bayesian factor model in Stan, and compares RMU reliability against coefficient alpha, coefficient H, and split-half reliability.

- `2_study1_0_slurm`, `2_study1_0_slurm_himem` — HPC job submission scripts
- `2_study1_1_simulate.R` — for each combination of sample size and loading set, simulates data and fits `stan_models/stan_inequiv_factor_model_v14.stan`
- `2_study1_2_analysis.R` — reads in and summarises the simulation output (bias, RMSE, coverage) across conditions

Simulation functions used: **`sim_factor_stnd`** (generates standardised-loading factor data), **`run_study1_simulation`** (orchestrates simulation + Stan fit + reliability estimation for one condition, internally calling `coef_h` for coefficient H and `calc_r_stan_m3` for the RMU correlation from posterior draws)

### Study 2 — Signal detection (SDT) model (`3_study2_*`)

Simulates binomial hit/false-alarm data from a signal-detection model across sample sizes, number of items/trials, and between-subject variability in sensitivity, then fits the model with `brms`.

- `3_study2_0_slurm` — HPC job submission script
- `3_study2_1_simulate.R` — for each parameter combination, simulates SDT data and fits the binomial probit model via `brms`/`cmdstanr`
- `3_study2_2_analysis.R` — collates results and compares them against population reliability estimates

Simulation functions used: **`sim_sdt_binomial`** (generates per-subject hit/false-alarm counts from sensitivity and criterion parameters), **`run_sdt_sim`** (orchestrates simulation + brms fit + reliability estimation, internally calling `calc_r_brms_sdt` for the RMU correlation from posterior draws)

### Study 3 — Reinforcement-learning (two-armed bandit) model (`4_study3_*`, `5_study3b_*`)

Simulates choice/outcome sequences from a Rescorla-Wagner-style reinforcement-learning model across sample sizes, trial counts, and between-subject variability in learning rate, then fits `stan_models/stan_two_arm_bandit_v6.stan`.

- `4_study3_0_slurm`, `4_study3_0_slurm_himem`, `4_study3_0_slurm_largesamplesize_himem2` — HPC job submission scripts (including a large-sample-size variant)
- `4_study3_1_simulate.R` — main simulation across sample sizes/trial counts/learning-rate variability, using full MCMC
- `4_study3_1_largesamplesize_simulate.R` — same design at a larger sample size (n = 2000), for supplementary/robustness checks
- `4_study3_2_analysis.R` — collates and summarises results across jobs
- `5_study3b_1_simulate.R` and its `_0_slurm_himem` variants — a variational-inference version of the same design (for larger sample sizes where full MCMC is too slow)

Simulation functions used: **`sim_ri`** (simulates trial-by-trial choices/outcomes from learning rate, decision noise, and reward probabilities, using **`g_normaluniform`** to draw individual differences in learning rate/decision noise and **`g_softmax`** to convert beliefs into choice probabilities), **`run_ri_sim`** (orchestrates simulation + full-MCMC Stan fit + reliability estimation for Study 3), **`run_ri_sim_variational`** (same, but fits via Stan's variational inference, used in Study 3b)


## Helpful SLURM commands

See all completed jobs last 30 days 

sacct -S now-30days --name=study3_array_320trials --format=JobID,JobName,State,ExitCode,Elapsed,AllocCPUS

sacct -S now-30days --name=study3_array --format=JobID,JobName%25,State,Elapsed,ReqMem,MaxRSS%12 --units=G | grep "COMPLETED" | grep "batch"
Command to sync data from hpc:  

rsync -avzP k2583181@create:/users/k2583181/8_bayes_reliability/results/ /home/giaco/Downloads/hpc_results/

rsync -av --ignore-existing ~/Downloads/hpc_results/ ~/GitHub/8_bayes_reliability/results/
