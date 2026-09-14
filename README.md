# Code and results for Relative Measurement Uncertainty Paper

[![Preprint](https://img.shields.io/badge/preprint-PsyArXiv-1E90FF)](https://osf.io/preprints/psyarxiv/h54k8_v1)
![License](https://img.shields.io/github/license/giac01/8_bayes_reliability)
![Docker](https://img.shields.io/badge/docker-bignardig%2Ftidyverse461%3Av5-2496ED?logo=docker&logoColor=white)
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

All code runs in the **`bignardig/tidyverse461`** Docker container (**R 4.6.1**; on the HPC the same container is used as a Singularity image, `tidyverse461.sif`). Different tags were used at different stages of the project:

| Stage | Scripts | Container |
|---|---|---|
| Study 1 and Study 2 simulations | `2_study1_1_simulate.R`, `3_study2_1_simulate.R` | `bignardig/tidyverse461:v2` |
| Study 3 simulations | `4_study3_1_simulate.R`, `5_study3_1_simulate_320trials.R`, `6_study3_1_simulate_240pps.R` | `bignardig/tidyverse461:v3` (adds the `libtbb-dev` system library needed by the Stan model) |
| Analysis scripts and applied example | `*_2_analysis.R`, `applied_example_pike/` | `bignardig/tidyverse461:v5` |

The simulation results in `results/` were produced with the v2/v3 containers; the summary tables and figures (`results_tables/`, `plots/`) are produced from them with v5. Each `_2_analysis.R` script states this in its header. The container does not include cmdstan (v2.39.0), which is installed separately and pointed to with `cmdstanr::set_cmdstan_path()` in the simulation scripts (`0_set_cmdstan_path_cluster.R` on the HPC).

## Structure

- `install_packages.R` — checks a list of required R packages and installs any that are missing
- `0_set_cmdstan_path_cluster.R` — sets the `cmdstan` install path when running on the HPC cluster
- `1_setup.R` — loads packages and sources all helper functions in `helper_functions/`
- `2_*`, `3_*`, `4_*`, `5_*`, `6_*` — simulation and analysis scripts for each study (see below)
- `9_manuscript_normal_posterior_plot_withpoints.R` — generates the normal-posterior illustrative figure used in the manuscript
- `helper_functions/` — shared R functions (simulation, model-fitting, and reliability-estimation helpers); `helper_functions/depreciated/` holds superseded versions kept for reference
- `stan_models/` — Stan model files
- `data/` — study data (not tracked in git)
- `results/`, `results_tables/` — simulation outputs
- `applied_example_pike/` — worked applied example fitting the RMU method to real reinforcement-learning data (see [Applied example](#applied-example) below)
- `tutorial_calculating_rmu_gonogo.qmd`, `tutorial_rmu_sum_score_reliability.qmd` — worked tutorials (see [Tutorials](#tutorials) below)

There are three studies, each simulating data from a different measurement model, fitting it with Stan/brms, and computing RMU reliability alongside classical benchmarks (coefficient alpha, coefficient H, split-half). Each study has a `_0_slurm` job script (submits the simulation to an HPC cluster), a `_1_simulate.R` script (runs the simulation for one seed/job), and a `_2_analysis.R` script (collates and summarises results across jobs).

### Study 1 — Linear factor model (`2_study1_*`)

Simulates single-factor data across a range of sample sizes and loading patterns, fits the Bayesian factor model in Stan, and compares RMU reliability against coefficient alpha, coefficient H, and split-half reliability.

- `2_study1_0_slurm` — HPC job submission script
- `2_study1_0_run_in_command_line` — runs the simulation locally in the project's Docker container
- `2_study1_1_simulate.R` — for each combination of sample size and loading set, simulates data and fits `stan_models/stan_inequiv_factor_model_v14.stan`
- `2_study1_2_analysis.R` — reads in and summarises the simulation output (bias, RMSE, coverage) across conditions

Simulation functions used: **`sim_factor_stnd`** (generates standardised-loading factor data), **`run_study1_simulation`** (orchestrates simulation + Stan fit + reliability estimation for one condition, internally calling `coef_h` for coefficient H and `gbtoolbox::reliability()` for the RMU correlation from posterior draws)

### Study 2 — Signal detection (SDT) model (`3_study2_*`)

Simulates binomial hit/false-alarm data from a signal-detection model across sample sizes, number of items/trials, and between-subject variability in sensitivity, then fits the model with `brms`.

- `3_study2_0_slurm` — HPC job submission script
- `3_study2_0_run_in_command_line` — runs the simulation locally in the project's Docker container
- `3_study2_1_simulate.R` — for each parameter combination, simulates SDT data and fits the binomial probit model via `brms`/`cmdstanr`
- `3_study2_2_analysis.R` — collates results and compares them against population reliability estimates

Simulation functions used: **`sim_sdt_binomial`** (generates per-subject hit/false-alarm counts from sensitivity and criterion parameters), **`run_study2_simulation`** (orchestrates simulation + brms fit + reliability estimation, internally calling `gbtoolbox::reliability()` for the RMU correlation from posterior draws)

### Study 3 — Reinforcement-learning (two-armed bandit) model (`4_study3_*`, `5_study3_*`, `6_study3_*`)

Simulates choice/outcome sequences from a Rescorla-Wagner-style reinforcement-learning model across sample sizes, trial counts, and between-subject variability in learning rate, then fits `stan_models/stan_two_arm_bandit_v6.stan`.

- `4_study3_0_slurm` — main HPC job submission script (array job looping over sample size/trial-count/learning-rate-sd/rep combinations)
- `4_study3_0_run_in_command_line` — runs the simulation locally in the project's Docker container
- `4_study3_1_simulate.R` — main simulation across sample sizes/trial counts/learning-rate variability, using full MCMC
- `5_study3_0_slurm`, `5_study3_1_simulate_320trials.R` — the n_trials = 320 condition split out into its own array job, since a single fit at this size can take >12h (see the header comments in `5_study3_0_slurm` for why)
- `6_study3_0_slurm`, `6_study3_1_simulate_240pps.R` — the n_pps = 240 condition (missing from the original sweep), with one simulation per array task rather than a loop over several rows (see the header comments in `6_study3_0_slurm`)
- `4_study3_2_analysis.R` — collates and summarises results across all three simulation scripts above (`4_`, `5_`, `6_`)

Simulation functions used: **`sim_ri`** (simulates trial-by-trial choices/outcomes from learning rate, decision noise, and reward probabilities, using **`g_normaluniform`** to draw individual differences in learning rate/decision noise and **`g_softmax`** to convert beliefs into choice probabilities), **`run_study3_simulation`** (orchestrates simulation + full-MCMC Stan fit + reliability estimation, called by all three simulation scripts above)

## Applied example

`applied_example_pike/` applies RMU to real data: reinforcement-learning parameters from the Fluctuating Bandit task in [Pike et al. (2026)](https://www.cambridge.org/core/journals/psychological-medicine/article/recoverability-reliability-and-generalizability-of-reward-processing-parameters-and-relation-to-mental-health-symptoms/C909E875D2E161CE597FCDCEBD704562#article) (data in `data/pike_2026/`), fit with `hBayesDM::bandit4arm_4par()`. RMU is compared against test-retest and split-half reliability.

- `02_01_pike_2026_fit_bandit_models.R` — fits the model separately to sessions 1 and 2 for the N = 115 subjects with both sessions (for test-retest reliability)
- `02_01_pike_2026_fit_bandit_splithalf_models.R` — fits the model to first-half/second-half trials within each session, for the same N = 115 subjects (for split-half reliability)
- `02_01_pike_2026_fit_bandit_fullsample.R` — fits the model (full-length and split-half) to the full N = 547 sample, since RMU doesn't require test-retest data
- `02_02_pike_2026_bandit.qmd` — reads the saved fits from the three scripts above and reports/compares the RMU, test-retest, and split-half reliability estimates (run the three fitting scripts first, e.g. inside `bignardig/tidyverse461:v5`)

## Tutorials

Two standalone worked tutorials, not tied to the paper's simulation studies:

- `tutorial_calculating_rmu_gonogo.qmd` — calculates RMU reliability for the `d'` parameter of a signal-detection model fit to go/no-go task data ([Hedge, Powell & Sumner, 2018](https://link.springer.com/article/10.3758/s13428-017-0935-1); data in `data/osf_hedge_cwzds/`), comparing against test-retest, split-half, and empirical reliability
- `tutorial_rmu_sum_score_reliability.qmd` — demonstrates RMU for estimating mean/sum score reliability using a simple multilevel simulation (repeated length measurements)

## Helpful terminal commands

docker run --rm -it --name bandit_fullsample \
  --user rstudio \
  -v /home/giaco:/home/rstudio \
  -w "/home/rstudio${PWD#/home/giaco}" \
  bignardig/tidyverse461:v5 \
  Rscript 02_01_pike_2026_fit_bandit_fullsample.R


## Helpful SLURM commands

See all completed jobs last 30 days 

```{bash}
sacct -S now-30days --name=study3_array_320trials --format=JobID,JobName,State,ExitCode,Elapsed,AllocCPUS

sacct -S now-30days --name=study3_array --format=JobID,JobName%25,State,Elapsed,ReqMem,MaxRSS%12 --units=G | grep "COMPLETED" | grep "batch"
Command to sync data from hpc:  

rsync -avzP k...@create:/users/k.../8_bayes_reliability/results/ /home/giaco/Downloads/hpc_results/

rsync -av --ignore-existing ~/Downloads/hpc_results/ ~/GitHub/8_bayes_reliability/results/
```