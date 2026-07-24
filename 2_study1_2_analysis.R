# Study 1: Factor-model simulation - analysis of RMU vs. Alpha / H (FA) / H (IRT)
# --------------------------------------------------------------------------------
# Run using the docker container: bignardig/tidyverse461:v2
#
# Compares four reliability estimators (RMU, Alpha, H (FA), H (IRT)) against
# test-retest reliability (correlation between t1/t2 Bayesian factor scores)
# and, where available, the population coefficient H.

# Load Packages ------------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data ------------------------------------------------------------------------

## Loading conditions used in the simulation --------------------------------------

loadings_list = list(
  c( 0, 0, 0, 0, 0, 0),
  c(.1,.1,.1,.1,.1),
  c(.3,.2,.1),
  c(.4,.3,.3,.2,.1,.0),
  c(.4,.4,.4,.4),
  c(.6,.5,.4,.3,.2),
  c(.7,.6,.5,.5,.5,.4,.4,.3,.3)
)

loadings_list_paste = lapply(loadings_list, function(x) paste0(x, collapse = "_")) %>% unlist()

loadings_list_pretty  = lapply(loadings_list, function(x) paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", ")) %>% unlist()
loadings_list_pretty2 = lapply(loadings_list, function(x) paste0("Loadings:", paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", "))) %>% unlist()

## Read raw simulation results ------------------------------------------------------

# I ran the simulation code multiple times on the cluster, so we have several results files we want to join

results_path = file.path("results","study1_results")
results_files = list.files(results_path,
                           pattern = ".rds",
                           recursive = FALSE,
                           full.names = TRUE
                           )
results = lapply(results_files, function(x) readRDS(x))
results = do.call("c", results)

## Build results_table (wide, one row per replicate) --------------------------------
# Each element of `results` is now a test-retest pair (t1/t2): every estimator
# (RMU/Alpha/H (FA)/H (IRT)) has a _t1 and _t2 version. We keep one row per
# original replicate here, and stack t1/t2 into long format further down.

results_table = data.frame(i = 1:length(results))

results_table$sample_sizes    = sapply(results, function(x) x$settings$n) %>% as.numeric()
results_table$n_items         = sapply(results, function(x) length(x$settings$loadings)) %>% as.numeric()
results_table$loadings        = sapply(results, function(x) paste0(x$settings$loadings, collapse = "_"))
results_table$loading_set     = match(results_table$loadings , loadings_list_paste)
results_table$loading_list_pretty  = loadings_list_pretty[results_table$loading_set]
results_table$loading_list_pretty2 = loadings_list_pretty2[results_table$loading_set]

results_table$pop_coefh         = sapply(results, function(x) x$population_reliability) %>% as.numeric()    # Population coefficient H
results_table$pop_ss_loading    = sapply(results, function(x) sum(x$settings$loadings^2)) %>% as.numeric()
results_table$sec_min_loading   = sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[2]) %>% as.numeric()
results_table$third_min_loading = sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[3]) %>% as.numeric()

# Test-retest reliability: correlation between the Bayesian factor scores estimated
# at t1 and t2. This is now our finite-sample reliability estimand (replacing the
# ASCOTS true-score-based estimand used previously, which is no longer computed).
results_table$test_retest_reliability = sapply(results, function(x) x$test_retest_reliability) %>% as.numeric()

### T1 estimates ---------------------------------------------------------------
results_table$rmu_est_t1  = sapply(results, function(x) x$rmu_est_t1$rmu_estimate) %>% as.numeric()
results_table$rmu_lb_t1   = sapply(results, function(x) x$rmu_est_t1$hdci_lowerbound) %>% as.numeric()
results_table$rmu_ub_t1   = sapply(results, function(x) x$rmu_est_t1$hdci_upperbound) %>% as.numeric()

results_table$h_est_t1    = sapply(results, function(x) x$h_reliability_t1$r) %>% as.numeric()
results_table$h_lb_t1     = sapply(results, function(x) x$h_reliability_t1$ci[1]) %>% as.numeric()
results_table$h_ub_t1     = sapply(results, function(x) x$h_reliability_t1$ci[2]) %>% as.numeric()

results_table$a_est_t1    = sapply(results, function(x) x$alpha_reliability_t1$est) %>% as.numeric()
results_table$a_lb_t1     = sapply(results, function(x) x$alpha_reliability_t1$ci.lower) %>% as.numeric()
results_table$a_ub_t1     = sapply(results, function(x) x$alpha_reliability_t1$ci.upper) %>% as.numeric()

results_table$irth_est_t1 = sapply(results, function(x) x$mcmc_coefh_t1$mcmc_coef_h) %>% as.numeric()
results_table$irth_lb_t1  = sapply(results, function(x) x$mcmc_coefh_t1$.lower) %>% as.numeric()
results_table$irth_ub_t1  = sapply(results, function(x) x$mcmc_coefh_t1$.upper) %>% as.numeric()

results_table$true_score_coverage_t1 = sapply(results, function(x) x$true_score_coverage_t1) %>% as.numeric()
results_table$diag_divergences_t1    = sapply(results, function(x) x$diagnostics_divergences_t1) %>% as.numeric()
results_table$diag_ebfmi_t1          = sapply(results, function(x) length(which(x$diagnostics_ebfmi_t1 < .2)))

### T2 estimates ---------------------------------------------------------------
results_table$rmu_est_t2  = sapply(results, function(x) x$rmu_est_t2$rmu_estimate) %>% as.numeric()
results_table$rmu_lb_t2   = sapply(results, function(x) x$rmu_est_t2$hdci_lowerbound) %>% as.numeric()
results_table$rmu_ub_t2   = sapply(results, function(x) x$rmu_est_t2$hdci_upperbound) %>% as.numeric()

results_table$h_est_t2    = sapply(results, function(x) x$h_reliability_t2$r) %>% as.numeric()
results_table$h_lb_t2     = sapply(results, function(x) x$h_reliability_t2$ci[1]) %>% as.numeric()
results_table$h_ub_t2     = sapply(results, function(x) x$h_reliability_t2$ci[2]) %>% as.numeric()

results_table$a_est_t2    = sapply(results, function(x) x$alpha_reliability_t2$est) %>% as.numeric()
results_table$a_lb_t2     = sapply(results, function(x) x$alpha_reliability_t2$ci.lower) %>% as.numeric()
results_table$a_ub_t2     = sapply(results, function(x) x$alpha_reliability_t2$ci.upper) %>% as.numeric()

results_table$irth_est_t2 = sapply(results, function(x) x$mcmc_coefh_t2$mcmc_coef_h) %>% as.numeric()
results_table$irth_lb_t2  = sapply(results, function(x) x$mcmc_coefh_t2$.lower) %>% as.numeric()
results_table$irth_ub_t2  = sapply(results, function(x) x$mcmc_coefh_t2$.upper) %>% as.numeric()

results_table$true_score_coverage_t2 = sapply(results, function(x) x$true_score_coverage_t2) %>% as.numeric()
results_table$diag_divergences_t2    = sapply(results, function(x) x$diagnostics_divergences_t2) %>% as.numeric()
results_table$diag_ebfmi_t2          = sapply(results, function(x) length(which(x$diagnostics_ebfmi_t2 < .2)))

results_table$sample_sizes = factor(results_table$sample_sizes)

# Sanity Checks on Simulation Output ------------------------------------------------

## Check for duplicate RNG seeds ----------------------------------------------------
# O(n^2) over all replicates - slow, so it's off by default. Set to TRUE to re-run.

run_seed_check = FALSE

if (run_seed_check) {
  n_results <- length(results)
  matches <- matrix(FALSE, nrow=n_results, ncol=n_results)

  for(i in 1:(n_results-1)) {
    for(j in (i+1):n_results) {
      matches[i,j] <- identical(results[[i]]$settings$seed,
                                results[[j]]$settings$seed)
    }
  }

  if(any(matches)) {
    which(matches, arr.ind=TRUE)
  } else {
    print("No identical RNG states found!")
  }
}

# Reshape to Long Format -------------------------------------------------------------

## Stack t1/t2 into long format ------------------------------------------------------
# Each original replicate contributes two rows (t1, t2). `rowid` identifies the
# original replicate and is shared across its t1/t2 rows, so it doubles as the
# clustering variable used below to compute cluster-robust-ish standard errors
# (t1 and t2 estimates from the same replicate are not independent).

results_table_stacked = results_table %>%
  rename(rowid = i) %>%
  pivot_longer(
    cols = matches("_t[12]$"),
    names_to = c(".value", "time"),
    names_pattern = "(.*)_t([12])"
  ) %>%
  mutate(
    time = paste0("t", time),
    diag_divergences_binary = as.numeric(diag_divergences > 0),
    diag_ebfmi_binary       = as.numeric(diag_ebfmi > 0)
  )

## Pivot longer over estimator name --------------------------------------------------

results_table_long = results_table_stacked %>%
  pivot_longer(cols = c(rmu_est, h_est, a_est, irth_est,
                        rmu_lb, h_lb, a_lb, irth_lb,
                        rmu_ub, h_ub, a_ub, irth_ub
                        ), names_to = c("name", ".value"), names_pattern = "(rmu|h|a|irth)_(.*)")

# Performance Metrics ----------------------------------------------------------------

## Overall Performance: test-retest reliability estimand ----------------------------

results_table_long %>%
  group_by(name, loading_set, sample_sizes) %>%
  mutate(
    name       = factor(name, levels = c("rmu","a","h","irth")),
    estimand   = mean(test_retest_reliability),        # mean test-retest correlation within this sample size / loading condition
    mean_est   = mean(est)
  ) %>%
  ungroup() %>%
  group_by(name) %>%
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb,
    ci_be_correct = (lb <= mean_est & ub >= mean_est)
  ) %>%
  summarise(

    estimand      = mean(test_retest_reliability),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    n             = n(),
    n_clusters    = n_distinct(rowid),                         # t1 and t2 rows from the same replicate are not independent

    # Mean Estimate
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n_clusters),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,

    # Empirical Standard Error
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n_clusters-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Bias
    bias        = mean(difference),
    bias_se     = sqrt(1/(n_clusters*(n_clusters-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    # Mean absolute deviation
    mad         = mean(abs(difference)),

    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n_clusters*(n_clusters-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    # Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n_clusters),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    # Bias corrected coverage
    coverage_be    = length(which(ci_be_correct))/length(ci_be_correct),
    coverage_be_se = sqrt((coverage_be*(1-coverage_be))/n_clusters),
    coverage_be_lb = coverage_be - qnorm(0.975)*coverage_be_se,
    coverage_be_ub = coverage_be + qnorm(0.975)*coverage_be_se,

    mean_ci_length = mean(ci_length),

    `Mean True Score Coverage`   = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n

  ) %>%
  ungroup()

## Maximal Reliability: population coefficient H estimand ---------------------------

results_table_long %>%
  mutate(
    name       = factor(name, levels = c("rmu","a","h","irth")),
    estimand   = pop_coefh,
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb,
  ) %>%
  group_by(name) %>%
  summarise(

    estimand      = mean(pop_coefh),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    n             = n(),
    n_clusters    = n_distinct(rowid),

    # Mean Estimate
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n_clusters),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,

    # Empirical Standard Error
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n_clusters-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Bias
    bias        = mean(difference),
    bias_se     = sqrt(1/(n_clusters*(n_clusters-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    # Mean absolute deviation
    mad         = mean(abs(difference)),

    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n_clusters*(n_clusters-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    # Root Mean Squared Error
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    # Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n_clusters),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mean_ci_length = mean(ci_length),
  ) %>%
  ungroup() %>%
  select(name, n,
         RMSE, RMSE_lb, RMSE_ub,
         bias, bias_lb, bias_ub,
         EmpSE, EmpSE_lb, EmpSE_ub,
         coverage, coverage_lb, coverage_ub,
         mean_ci_length
         ) %>%
  gt() %>%
  gt::fmt(columns = !c(name, n), fns = ~ gbtoolbox::apa_num(., n_decimal_places = 3)) |>
  gtsave(filename = file.path("results_tables","2_study1_performance_comparison.html"))

## Performance by Condition (loadings x sample size x estimator) --------------------
# Currently not setting different estimands for different estimators.

results_table_cleaned =
results_table_long %>%
  mutate(
    name = factor(name, levels = c("rmu","a","h","irth"))
  ) %>%
  group_by( loading_set, sample_sizes, name) %>%
  mutate(
    estimand = mean(test_retest_reliability)            # mean test-retest correlation within this sample size / loading condition
        ) %>%
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  summarise(

    estimand      = mean(test_retest_reliability),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    n             = n(),
    n_clusters    = n_distinct(rowid),

    # Mean Estimate
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n_clusters),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,

    # Empirical Standard Error
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n_clusters-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Bias
    bias        = mean(difference),
    bias_se     = sqrt(1/(n_clusters*(n_clusters-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    # Mean absolute deviation
    mad         = mean(abs(difference)),

    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n_clusters*(n_clusters-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    # Root Mean Squared Error
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    # Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n_clusters),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mean_ci_length = mean(ci_length),

    `Mean True Score Coverage`   = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n

  ) %>%
  ungroup() %>%
  mutate(
    loadings_list_pretty = loadings_list_pretty[loading_set],
    loadings_list_pretty2 = loadings_list_pretty2[loading_set],
    name = as.character(name)                            # drop factor now that row order (rmu, a, h, irth) is set; keeps the relabeling below from turning entries into NA
  )

results_table_cleaned[which(results_table_cleaned$name!="rmu"),c("perc_diag_divergences_binary","perc_diag_ebfmi_binary")] = NA
results_table_cleaned$name[results_table_cleaned$name=="rmu"] = "RMU"
results_table_cleaned$name[results_table_cleaned$name=="h"] = "H (FA)"
results_table_cleaned$name[results_table_cleaned$name=="a"] = "Alpha"
results_table_cleaned$name[results_table_cleaned$name=="irth"] = "H (IRT)"

### Export as gt table ---------------------------------------------------------
results_table_cleaned %>%
  select(-any_of(c("coverage_se", "loading_set", "pop_coefh_sd",
                   "loadings_list_pretty2", "mad", "n_clusters",
                   "mean", "mean_se", "mean_ub", "mean_lb",
                   "bias_se"
  ))) %>%
  gt() %>%
  gt::cols_move_to_start(name) %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"),contains("perc_diag")),
    decimals = 1
  ) %>%
  cols_label(
    name         ~ "Est",
    pop_coefh      ~ "coef H",
    loadings_list_pretty ~ "Loadings",
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",
    bias           ~ "bias",
    mean_ci_length ~ "Mean Length",

    EmpSE    ~ "",
    EmpSE_lb ~ "LB",
    EmpSE_ub ~ "UB",
    bias     ~ "",
    bias_lb  ~ "LB",
    bias_ub  ~ "UB",
    RMSE    ~ "",
    RMSE_lb  ~ "LB",
    RMSE_ub  ~ "UB",
    coverage ~ "Cov.",
    coverage_lb  ~ "LB",
    coverage_ub  ~ "UB",
    perc_diag_divergences_binary ~ "% Divergent Transitions",
    perc_diag_ebfmi_binary ~  "% Low E-BFMI"
  )  %>%

  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(estimand, pop_coefh, loadings_list_pretty,sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(contains("RMSE"),contains("EmpSE"), contains("bias"))) %>%
  tab_spanner(label = "Confidence/Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    footnote = html("<b>n<sub>sim</sub></b> = number of simulations completed for this set of simulation parameters (t1 and t2 estimates counted separately).
                <b>n<sub>obs</sub></b> = number of subjects per simulation.
                <b>RMSE</b> = Root Mean Squared Error.
                <b>Coverage</b> = proportion of times the 95% credible intervals include the estimand, which should be around 95%.
                <b>estimand</b> = test-retest reliability, i.e. the correlation between the Bayesian factor scores estimated at t1 and t2, averaged within each sample size / loading condition.
                <b>coef H</b> = Maximal Reliability Estimand (see manuscript).
                <b>Mean Length</b> = Mean length of credible or confidence interval.
                <b>% DT</b> = Percent of simulations with divergent transitions (applies to Bayesian measurement models only).
                <b>% Low E-BFMI</b> = Percent of simulations with E-BFMI value of less than .20."
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      rows = which((name == "RMU"))
    )
  ) %>%
  tab_options(
    table.width = pct(100)
  ) %>%
  gt::cols_hide(
    c(`Mean True Score Coverage`, starts_with("MSE"), ends_with("_se"))
    ) %>%
  cols_width(
    c(name) ~ px(70),
    c(loadings_list_pretty) ~ px(150),
    c(sample_sizes) ~ px(60),
    c(n) ~ px(55),
    c(perc_diag_divergences_binary) ~ px(80),
    c(perc_diag_ebfmi_binary) ~ px(80),
    everything() ~ px(60)
  ) %>%
  opt_horizontal_padding(scale = 1) %>%
  gtsave(filename = file.path("results_tables","2_study1_performance_comparison_differentestimators.html"))

# Does the Estimand Vary with Simulation Conditions? ---------------------------------

mod_estimand = results_table_long |>
  filter(time == "t1") |>
  filter(name == "rmu") |>
  mutate(across(c(loading_set, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ loading_set*sample_sizes, data = _)

mod_estimand0 = results_table_long |>
  filter(time == "t1") |>
  filter(name == "rmu") |>
  mutate(across(c(loading_set, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ loading_set, data = _)

anova(mod_estimand)
anova(mod_estimand0, mod_estimand)

## Export as gt tables ----------------------------------------------------------

table_estimand_anova = anova(mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "ANOVA: test-retest reliability ~ loading_set * sample_sizes") %>%
  fmt_number(columns = c(sumsq, meansq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_anova

gtsave(table_estimand_anova, filename = file.path("results_tables", "2_study1_estimand_anova.html"))

table_estimand_modelcomparison = anova(mod_estimand0, mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "Model comparison: loading_set vs. loading_set * sample_sizes") %>%
  fmt_number(columns = c(rss, sumsq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_modelcomparison

gtsave(table_estimand_modelcomparison, filename = file.path("results_tables", "2_study1_estimand_modelcomparison.html"))

# Plots -------------------------------------------------------------------------------

format_decimals <- function(x, decimals = 2) {
  format_number <- function(num) {
    if (is.na(num)) {
      return(NA_character_)
    } else if (abs(num - 1) < 1e-10) {  # Check if the number is very close to 1
      return("1")
    } else if (abs(num) < 1) {
      return(sub("^-?0.", ".", sprintf(paste0("%.", decimals, "f"), num)))
    } else {
      return(sprintf(paste0("%.", decimals, "f"), num))
    }
  }
  sapply(x, format_number)
}

## Bar chart: bias / EmpSE / MSE / coverage by estimator -----------------------------

data_plot = results_table_cleaned %>%
  rename(
    Estimator = name,
    Coverage  = coverage
         ) %>%
  pivot_longer(cols = c("bias","EmpSE","MSE", "Coverage"), values_to = "Est") %>%
  mutate(
    name = factor(name, levels = c("bias", "EmpSE", "MSE", "Coverage")),
    Estimator = factor(Estimator, levels = c("RMU", "Alpha", "H (FA)", "H (IRT)"))
  )

data_plot %>%
  filter(loading_set!=7) %>%
  ggplot(aes(y = Est,  group = Estimator, x = sample_sizes, fill = Estimator)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = .9) +
  facet_grid(
    rows   = vars(name),
    cols   = vars(loading_set),
    scales = "free_y",
    switch = "y"
    ) +
  labs(
    y = NULL,
    x = expression(paste("Sample Size of Simulated Dataset (n", scriptstyle(obs), ")"))
  ) +
  theme_bw(
    base_size = 20
  ) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_text(hjust = 1),
    strip.placement    = "outside",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    ) +
  ggh4x::facetted_pos_scales(
    y = list(
      name == "bias"     ~ scale_y_continuous(labels = format_decimals),
      name == "EmpSE"    ~ scale_y_continuous(labels = format_decimals),
      name == "MSE"      ~ scale_y_continuous(limits = c(NA, 0.03), labels = format_decimals),
      name == "Coverage" ~ scale_y_continuous(limits = c(.9, 1), labels = function(x) format_decimals(x,decimals = 3) )
    ))

## RMU violin plot ---------------------------------------------------------------

library(grid)

results_table_cleaned2 = results_table_cleaned %>%
  filter(name == "RMU") %>%
  rename(
    loading_list_pretty = loadings_list_pretty
    )

results_table_long  %>%
  filter(name == "rmu") %>%
  group_by(sample_sizes, loading_set) %>%
  mutate(mean_test_retest_reliability = mean(test_retest_reliability)) %>%
  ggplot(aes(y = est, x = sample_sizes)) +
  geom_violin(
    width = .95,
    fill = "grey",
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               shape = 3,
               col = "red",
               stroke = .9
               ) +
  geom_point(
    data = results_table_cleaned2,
    aes(
      y  = estimand,
      x = sample_sizes
      ),
    shape = 1,
    size = 2,
    col = "red",
    stroke = .9
    ) +
  geom_hline(aes(yintercept = pop_coefh), linetype = "dashed") +
  facet_wrap(
    ~ loading_list_pretty,
    nrow = 3,
    ncol = 3
    ) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  geom_rect(
    data = results_table_cleaned2,
    inherit.aes = FALSE,
    aes(
      xmin = .5,
      xmax = 3.5,
      # Position the rectangle differently based on conditions
      ymin = ifelse(loading_set == 7, .06, 0.66),
      ymax = ifelse(loading_set == 7, 0.31, .90)
    ),
    fill = "white",
    alpha = 0.4
  ) +
  geom_text(
    data = results_table_cleaned2,
    aes(
      y = ifelse(loading_list_pretty == " .7,  .6,  .5,  .5,  .5,  .4,  .4,  .3,  .3", 0, 0.89),
      x = factor(sample_sizes),
      label = paste0(
        "B = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", bias)), "\n",
        "E = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", sqrt(MSE))), "\n",
        "C = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", coverage)), "\n"
      )
    ),
    vjust = ifelse(results_table_cleaned2$loading_list_pretty == " .7,  .6,  .5,  .5,  .5,  .4,  .4,  .3,  .3", 0, 1),
    hjust = 1,
    size = 2.6,
    position = position_nudge(x=.42),
    col = "grey20"
  ) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
  )

ggsave(file.path("plots","2_study1_violinplot.pdf"), width = 6.2, height = 7)

## Comparison violin plot: all estimators ---------------------------------------

results_table_long %>%
  mutate(name = factor(name,
                       levels = c("rmu", "a", "h", "irth"),
                       labels = c("RMU", "Alpha", "H (FA)", "H (IRT)"))) %>%
  ggplot(aes(y = est, x = sample_sizes, col = name, fill = name)) +
  # Adjust position and overlap of violins
  geom_violin(
    width = 0.8,
    position = position_dodge(width = 0.8)
    ) +
  geom_hline(aes(yintercept = pop_coefh), linetype = "dashed") +
  # Add mean points for better visibility
  stat_summary(
    fun = mean,
    geom = "point",
    size = .5,
    col = "black",
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ loading_list_pretty, scales = "fixed") +
  coord_cartesian(ylim = c(-.00, .97)) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(
    col = "none",
    fill = guide_legend(title = "Estimator")) +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    panel.spacing = unit(0.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme_bw() +
  theme(
    legend.position = c(.9, .03),    # Adjust these values to position the legend
    legend.justification = c(1, 0),     # Adjust these values to align the legend
    legend.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path("plots","2_study1_violinplot_allestimators.pdf"), width = 6.2, height = 7)

## Credible interval plot (RMU) --------------------------------------------------

results_table_stacked %>%
  arrange(rmu_est) %>%
  group_by(sample_sizes,loading_list_pretty2) %>%
  mutate(
    x = 1:n(),
    estimand = mean(test_retest_reliability),           # mean test-retest correlation within this sample size / loading condition
    ci_correct = (rmu_lb <= estimand & rmu_ub >= estimand),
  ) %>%
  ungroup() %>%
  ggplot(aes(ymin = rmu_lb, ymax = rmu_ub,
             x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) +
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_wrap(~ loading_list_pretty2 + sample_sizes,
             scales = "free", ncol = 3)
