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

> [!WARNING]
> This repository is actively being updated. Scripts, file names, and structure are being reorganized and may change without notice — expect breaking changes until this warning is removed.

## Structure

- `1_setup.R` — loads packages and helper functions
- `2_*`, `3_*`, `4_*`, `5_*` — simulation and analysis scripts for each study
- `helper_functions/` — shared R functions
- `stan_models/` — Stan model files
- `data/` — study data (not tracked in git)
- `results/`, `results_tables/` — simulation outputs
