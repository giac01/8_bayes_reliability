# Study 2: SDT simulation - analysis of RMU vs. Split-Half reliability
# --------------------------------------------------------------------------------
# Run using the docker container: bignardig/tidyverse461:v2
#
# Compares two reliability estimators (RMU, Split-Half) against test-retest
# reliability (correlation between t1/t2 Bayesian sensitivity point estimates).

# Load Packages ------------------------------------------------------------------

library(tidyverse)
library(gt)
library(clubSandwich)

rm(list = ls(all.names = TRUE))

# Helper Functions -----------------------------------------------------------------

## Cluster-robust SE of a mean, clustering by sim_id ---------------------------
# results_table_long has two rows per simulation (t1/t2 waves) that share the
# same underlying subject-level truth and are therefore not independent
# (ICC ~ 0.89 for rmu_est by sim_id). The naive SE formulas used in Study 1
# (e.g. sqrt(1/(n*(n-1))*sum((est-mean)^2))) assume iid rows, so they understate
# the Monte Carlo SE of bias/MSE/coverage. This replaces them with a
# cluster-robust (CR2) SE of the mean, via a trivial intercept-only lm().

cluster_se_mean = function(x, cluster){
  fit = lm(x ~ 1)
  sqrt(vcovCR(fit, cluster = cluster, type = "CR2")[1, 1])
}

# Load Data --------------------------------------------------------------------------

## Read raw simulation results -------------------------------------------------

results_path = file.path("results","study2_results")

results_files = list.files(results_path,
                           pattern = "^study2_[0-9]+_.*\\.rds$",   # excludes study2_params_list.rds
                           recursive = FALSE,
                           full.names = TRUE
)
results = lapply(results_files, function(x) readRDS(x))
results = do.call("c", results)

## Build results_table (wide, one row per replicate x wave) --------------------
# Each simulation fits the SDT model separately at t1 and t2 (a full
# test-retest design), producing two waves of RMU/split-half estimates. We
# treat each wave as a separate replicate of the same underlying condition, so
# a single simulation contributes two rows to results_table.

extract_wave = function(results, wave){

  rmu_field  = paste0("rmu_est_", wave)
  sh_field   = paste0("splithalf_a_", wave)
  div_field  = paste0("diagnostics_divergences_", wave)
  bfmi_field = paste0("diagnostics_low_bfmi_chains_", wave)

  # sim_id identifies which element of the `results` list this row came from.
  # Each simulation iteration appears twice in the final results_table (once
  # per wave), sharing the same sim_id.
  df = data.frame(sim_id = 1:length(results))

  df$time          = wave
  df$sample_sizes  = sapply(results, function(x) x$settings$n_pps)
  df$sens_mean     = sapply(results, function(x) x$settings$sens_mean)
  df$sens_sigma    = sapply(results, function(x) x$settings$sens_sigma)
  df$k_mean        = sapply(results, function(x) x$settings$k_mean)
  df$k_sigma       = sapply(results, function(x) x$settings$k_sigma)
  df$n_items       = sapply(results, function(x) x$settings$n_items) * 2 # DOUBLED: n_items is the number of trials in EACH condition (OLD or NEW)

  # Test-retest reliability (correlation of the Bayesian sensitivity point
  # estimates between t1 and t2) is used as the estimand below. Unlike ASCOTS,
  # it doesn't require knowledge of the true simulated parameter values, and
  # is a single value per simulation (identical for its t1 and t2 rows, since
  # it is computed jointly from both waves).
  df$test_retest_reliability = sapply(results, function(x) x$test_retest_reliability) %>% as.numeric()
  df$testretest_cor_dprime   = sapply(results, function(x) as.numeric(x$testretest_cor_dprime$estimate))

  df$rmu_est = sapply(results, function(x) x[[rmu_field]]$rmu_estimate)    %>% as.numeric()
  df$rmu_lb  = sapply(results, function(x) x[[rmu_field]]$hdci_lowerbound) %>% as.numeric()
  df$rmu_ub  = sapply(results, function(x) x[[rmu_field]]$hdci_upperbound) %>% as.numeric()

  df$sh_est  = sapply(results, function(x) x[[sh_field]]$est)      %>% as.numeric()
  df$sh_lb   = sapply(results, function(x) x[[sh_field]]$ci.lower) %>% as.numeric()
  df$sh_ub   = sapply(results, function(x) x[[sh_field]]$ci.upper) %>% as.numeric()

  df$diag_divergences        = sapply(results, function(x) x[[div_field]]) %>% as.numeric()
  df$diag_divergences_binary = as.numeric(df$diag_divergences>0)
  df$diag_low_bfmi           = sapply(results, function(x) length(x[[bfmi_field]]))
  df$diag_low_bfmi_binary    = as.numeric(df$diag_low_bfmi>0)

  df
}

results_table = bind_rows(
  extract_wave(results, "t1"),
  extract_wave(results, "t2")
)

results_table$settings_used = paste(
  results_table$sens_mean,
  results_table$sens_sigma,
  results_table$k_mean,
  results_table$k_sigma,
  results_table$n_items,
  sep = "_"
)

results_table$settings_used_with_npps = paste(
  results_table$sens_mean,
  results_table$sens_sigma,
  results_table$k_mean,
  results_table$k_sigma,
  results_table$n_items,
  results_table$sample_sizes,
  sep = "_"
)

results_table$rmu_ci_length = results_table$rmu_ub - results_table$rmu_lb
results_table$sh_ci_length  = results_table$sh_ub  - results_table$sh_lb

results_table$sample_sizes  = factor(results_table$sample_sizes)

## Filter out trials with k_sigma == 0 ------------------------------------------
# Performance was good in these conditions, I removed this condition to simplify the presentation of the results

results_table = results_table[results_table$k_sigma!=0,]

# Sanity Checks on Simulation Output ------------------------------------------------

## Check for duplicate RNG seeds -----------------------------------------------
# Slow, so it's off by default. Set to TRUE to re-run.

run_seed_check = FALSE

if (run_seed_check) {
  n_check = 100
  n_results <- length(results)
  matches <- matrix(FALSE, nrow=n_check, ncol=n_results)

  for(i in 1:n_check) {
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

## Check for model-fitting issues ------------------------------------------------

results_table$diag_low_bfmi_binary %>% table()
results_table$diag_divergences_binary %>% table()

## Diagnostic scatterplot: RMU vs. test-retest reliability -----------------------

results_table %>%
  mutate(
    sample_sizes = factor(sample_sizes),
    sens_sigma  = factor(sens_sigma),
    n_items     = factor(n_items)
  ) %>%
  ggplot(aes( x = test_retest_reliability, y = rmu_est, shape = sample_sizes, col = n_items)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~sens_sigma)

# Reshape to Long Format -------------------------------------------------------------

## Pivot longer over estimator name --------------------------------------------------

results_table_long = results_table %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmu_est,rmu_lb,rmu_ub, sh_est, sh_lb, sh_ub), names_to = c("name", ".value"), names_pattern = "(rmu|sh)_(.*)")

# Performance Metrics ----------------------------------------------------------------

## Overall Performance -----------------------------------------------------------

results_table_long %>%
  group_by(n_items, sens_sigma, name) %>%       # aggregating over sens_mean & wave (t1/t2); estimand pooled across sample_sizes since it doesn't meaningfully vary with n; k_sigma dropped as it's constant (0.2) in this simulation
  mutate(
    estimand   = mean(test_retest_reliability),
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  ungroup() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    estimand    = mean(test_retest_reliability),
    estimand_sd = sd(test_retest_reliability),
    mean        = mean(est),

    bias        = mean(difference),
    # bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),                # old: assumes n independent rows
    bias_se     = cluster_se_mean(difference, sim_id),                  # new: cluster-robust (CR2), clustered by sim_id
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Mean Squared Error
    MSE         = mean((difference)^2),
    # MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),          # old: assumes n independent rows
    MSE_se      = cluster_se_mean(difference^2, sim_id),                # new: cluster-robust (CR2), clustered by sim_id
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    # Root Mean Squared Error
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    mae         = mean(abs(difference)),

    coverage    = length(which(ci_correct))/length(ci_correct),
    # coverage_se = sqrt((coverage*(1-coverage))/n),                    # old: assumes n independent Bernoulli trials
    coverage_se = cluster_se_mean(as.numeric(ci_correct), sim_id),      # new: cluster-robust (CR2), clustered by sim_id
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mean_ci_length = mean(ci_length),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_low_bfmi_binary    = sum(diag_low_bfmi_binary)/n,
    mean_testretest_cor_dprime   = mean(testretest_cor_dprime)
  )  %>%
  select(-estimand, -estimand_sd, -mean,-ends_with("_se")) %>%
  ungroup() %>%
  knitr::kable(digits = 3)

## Performance by Condition (trials x sensitivity SD x sample size x estimator) ------

results_table_cleaned =
results_table_long %>%
  group_by(n_items, sens_sigma, name) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n; k_sigma dropped as it's constant (0.2) in this simulation
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(n_items, sens_sigma, name, sample_sizes) %>%       # aggregating over sens_mean & wave (t1/t2); performance metrics still broken down by sample size
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    estimand    = mean(estimand),
    estimand_sd = sd(test_retest_reliability),
    mean        = mean(est),

    bias        = mean(difference),
    # bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),                # old: assumes n independent rows
    bias_se     = cluster_se_mean(difference, sim_id),                  # new: cluster-robust (CR2), clustered by sim_id
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Mean Squared Error
    MSE         = mean((difference)^2),
    # MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),          # old: assumes n independent rows
    MSE_se      = cluster_se_mean(difference^2, sim_id),                # new: cluster-robust (CR2), clustered by sim_id
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    # Root Mean Squared Error
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    mae         = mean(abs(difference)),

    coverage    = length(which(ci_correct))/length(ci_correct),
    # coverage_se = sqrt((coverage*(1-coverage))/n),                    # old: assumes n independent Bernoulli trials
    coverage_se = cluster_se_mean(as.numeric(ci_correct), sim_id),      # new: cluster-robust (CR2), clustered by sim_id
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mean_ci_length = mean(ci_length),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    sum_diag_divergences_binary = sum(diag_divergences_binary),
    sum_diag_low_bfmi_binary    = sum(diag_low_bfmi_binary),
    mean_testretest_cor_dprime  = mean(testretest_cor_dprime)
  )  %>%
  ungroup()

### Export as gt table -----------------------------------------------------------

table_performance_comparison =
results_table_cleaned %>%
  mutate(name = case_when(
    name == "rmu"  ~ "RMU",
    name == "sh" ~ "SH",
    TRUE ~ NA_character_  # Catches any other values
  )) %>%
  select(-estimand_sd, -coverage_se) %>%
  select(name, estimand, everything()) %>%
  arrange(sens_sigma, n_items, sample_sizes, name) %>%
  gt() %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n, n_items, sum_diag_divergences_binary, sum_diag_low_bfmi_binary),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"),perc_diag_divergences_binary),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",
    estimand ~ "estimand",
    sens_sigma   ~ "{{:sigma:_d'}}",
    n_items      ~ "{{n_trials}}",
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
    mean_ci_length ~  md("Mean<br>Length"),
    name         ~ "Est",
    perc_diag_divergences_binary ~ md("% DT"),
    sum_diag_divergences_binary ~ md("N Divergent<br>Transitions"),
    sum_diag_low_bfmi_binary ~   md("% Low<br>E-BFMI"),
  )  %>%
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
  tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "Simulation Parameters",
              columns = c(name,estimand, sens_sigma, n_items, sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance",
              columns = c(contains("RMSE"),contains("EmpSE"), contains("bias"))) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    footnote = html("<b>n<sub>sim</sub></b> = number of simulations completed for this set of simulation parameters.
              <b>n<sub>obs</sub></b> = number of subjects per simulation.
              <b>RMSE</b> = Root Mean Squared Error.
              <b>Coverage</b> = proportion of times the 95% credible intervals include the population reliability, which should be around 95%.
              <b>estimand</b> = test-retest reliability, i.e. the mean correlation between Bayesian sensitivity point-estimates at t1 and t2, pooled across all sample sizes for a given combination of trial number and sensitivity SD.
              <b>Mean Length</b> = Mean length of credible or confidence interval.
              <b>σ<sub>d'</sub></b> = standard deviation of population true sensitivity values across subjects.
              <b>n<sub>trials</sub></b> = number of trials completed per participant.
              <b>% DT</b> = Percent of simulations with divergent transitions (applies to Bayesian measurement models only).
                    "
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      rows = which((name=="RMU"))
    )
  ) %>%
  tab_options(
    table.width = pct(35)
  ) %>%
gt::cols_hide(
  c(mae,bias_se, mean, ends_with("_se"),mean_testretest_cor_dprime, starts_with("MSE"), contains("low_bfmi"),sum_diag_divergences_binary)
  )

table_performance_comparison

gtsave(table_performance_comparison, filename = file.path("results_tables","3_study2_performance_comparison.html"))

## Estimator Comparison: RMU vs. Split-Half --------------------------------------
# Same grouping as "Performance by Condition" above, but kept as a separate table
# because it feeds `comparison_statistics` (relative EmpSE/RMSE), which is used
# in the split-violin comparison plot below.

results_table_cleaned_2 =
  results_table_long %>%
  group_by(n_items, sens_sigma, name) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n; k_sigma dropped as it's constant (0.2) in this simulation
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(n_items, sens_sigma, name, sample_sizes) %>%       # aggregating over sens_mean & wave (t1/t2); performance metrics still broken down by sample size
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    estimand    = mean(estimand),
    estimand_sd = sd(test_retest_reliability),
    mean        = mean(est),
    bias        = mean(difference),
    # bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),                # old: assumes n independent rows
    bias_se     = cluster_se_mean(difference, sim_id),                  # new: cluster-robust (CR2), clustered by sim_id
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    # Mean Squared Error
    MSE         = mean((difference)^2),
    # MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),          # old: assumes n independent rows
    MSE_se      = cluster_se_mean(difference^2, sim_id),                # new: cluster-robust (CR2), clustered by sim_id
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    # Root Mean Squared Error
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    mae         = mean(abs(difference)),
    coverage    = length(which(ci_correct))/length(ci_correct),
    # coverage_se = sqrt((coverage*(1-coverage))/n),                    # old: assumes n independent Bernoulli trials
    coverage_se = cluster_se_mean(as.numeric(ci_correct), sim_id),      # new: cluster-robust (CR2), clustered by sim_id
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_low_bfmi_binary    = sum(diag_low_bfmi_binary)/n,
    mean_testretest_cor_dprime   = mean(testretest_cor_dprime)
  )  %>%
  ungroup()

comparison_statistics = results_table_cleaned_2 %>%
  select(
    name,
    sample_sizes, sens_sigma, mean,
    n_items, bias, EmpSE, MSE, n, RMSE
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = c(mean, bias, EmpSE, MSE, RMSE)
  ) %>%
  mutate(
    relative_precision  = 100*((EmpSE_sh^2 / EmpSE_rmu^2 ) - 1),
    relative_empse      = 100*((EmpSE_sh   / EmpSE_rmu ) - 1),
    relative_mse        = 100*((MSE_sh     / MSE_rmu ) - 1),
    relative_rmse       = 100*((RMSE_sh    / RMSE_rmu ) - 1)
  )

comparison_statistics %>%
  print(width = Inf, n = Inf)

# Does the Estimand Vary with Simulation Conditions? ---------------------------------

mod_estimand = results_table |>
  filter(time == "t1") |>
  mutate(across(c(sens_sigma, n_items, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ sens_sigma*n_items*sample_sizes, data = _)

mod_estimand0 = results_table |>
  filter(time == "t1") |>
  mutate(across(c(sens_sigma, n_items, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ sens_sigma*n_items, data = _)

anova(mod_estimand)
anova(mod_estimand0, mod_estimand)

## Export as gt tables ----------------------------------------------------------

table_estimand_anova = anova(mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "ANOVA: test-retest reliability ~ sens_sigma * n_items * sample_sizes") %>%
  fmt_number(columns = c(sumsq, meansq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_anova

gtsave(table_estimand_anova, filename = file.path("results_tables", "3_study2_estimand_anova.html"))

table_estimand_modelcomparison = anova(mod_estimand0, mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "Model comparison: sens_sigma * n_items vs. sens_sigma * n_items * sample_sizes") %>%
  fmt_number(columns = c(rss, sumsq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_modelcomparison

gtsave(table_estimand_modelcomparison, filename = file.path("results_tables", "3_study2_estimand_modelcomparison.html"))

# Additional Diagnostics --------------------------------------------------------------

## Check if RMU/split-half estimates are clustered by simulation condition -----

mod = results_table |>
  lme4::lmer(rmu_est ~ 1 + (1 | sim_id), data = _)

performance::icc(mod)

mod = results_table |>
  lme4::lmer(sh_est ~ 1 + (1 | sim_id), data = _)

performance::icc(mod)

# Plots -------------------------------------------------------------------------------

library(grid)

n_items_labels <- c(
  "20" = "20 Trials",
  "40" = "40 Trials",
  "80" = "80 Trials"
)

sens_sigma_labels <- c(
  "0"   = "Sensitivity SD = 0.00",
  "0.2" = "Sensitivity SD = 0.20",
  "0.4" = "Sensitivity SD = 0.40"
)

## RMU violin plot ---------------------------------------------------------------

results_table_cleaned2 = results_table_cleaned %>%
  filter(name == "rmu")

plot_violinplot = results_table_long  %>%
  filter(name == "rmu") %>%
  group_by(n_items, sens_sigma, sample_sizes) %>%       # aggregating over sample-sizes, sens_mean & wave (t1/t2)
    mutate(
    mean_true_score_model_score_cor2 = mean(test_retest_reliability),
    sample_sizes = factor(sample_sizes),
  ) %>%
  ggplot(
    aes(
      y = est,
      x = sample_sizes
      )
    ) +
  geom_violin(
    width = .95,
    fill = "grey80",
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun = mean,
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
      x = factor(sample_sizes)
    ),
    shape = 1,
    size = 2,
    col = "red",
    stroke = .9
  ) +
  geom_rect(
    data = results_table_cleaned2,
    inherit.aes = FALSE,
    aes(
      xmin = .5,
      xmax = 3.5,
      # Position the rectangle differently based on conditions
      ymin = ifelse(sens_sigma == 0.4 & n_items >= 80, .05, 0.68),
      ymax = ifelse(sens_sigma == 0.4 & n_items >= 80, 0.26, 0.90)
    ),
    fill = "white",
    alpha = 0.4
  ) +
  geom_text(
    data = results_table_cleaned2,
    aes(
      y = ifelse(sens_sigma == 0.4 & n_items >= 80, 0, 0.89),
      x = factor(sample_sizes),
      label = paste0(
        "B = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", bias)), "\n",
        "E = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", sqrt(MSE))), "\n",
        "C = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", coverage)), "\n"
      )
    ),
    vjust = ifelse(results_table_cleaned2$sens_sigma == 0.4 &
                     results_table_cleaned2$n_items >= 80, 0, 1),
    hjust = 1,
    size = 2.6,
    position = position_nudge(x=.42),
    col = "grey20"
  ) +
  facet_grid(
    cols = vars(n_items),
    rows = vars(sens_sigma),
    labeller = labeller(
      n_items = n_items_labels,
      sens_sigma = sens_sigma_labels
    )
  ) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom")
  ) +
  coord_cartesian(ylim=c(0,.87))

plot_violinplot

ggsave(file.path("plots","3_study2_violinplot.png"), plot = plot_violinplot, width = 6.2, height = 7)
ggsave(file.path("plots","3_study2_violinplot.pdf"), plot = plot_violinplot, width = 6.2, height = 7)

## Comparison violin plot: RMU vs. split-half ------------------------------------

source("https://raw.githubusercontent.com/PsyTeachR/introdataviz/7763afad2cea8fd9fa98acf4e389071cad61e758/R/splitviolin.R")

plot_violinplot_comparison = results_table_long  %>%
  group_by(n_items, sens_sigma, sample_sizes) %>%       # aggregating over sample-sizes, sens_mean & wave (t1/t2)
  mutate(
    mean_true_score_model_score_cor2 = mean(test_retest_reliability),
    sample_sizes = factor(sample_sizes),
    name = factor(name, levels = c("rmu", "sh"), labels = c("RMU", "Split-Half"))
  ) %>%
  ungroup() %>%
  arrange(name) %>%
  ggplot(
    aes(
      y = est,
      x = sample_sizes,
      fill = name
      )
  ) +
  geom_split_violin( # sourced from above link!
    width = .95,
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
  ) +
  annotate("rect",
           xmin = .5, xmax = 3.5,  # Adjust these values to cover just the text area
           ymin = -0.65, ymax = -1.24,  # Adjust based on your text position
           fill = "white", alpha = 0.7) +
  geom_text(
    data = comparison_statistics %>% filter(sample_sizes==250),
    inherit.aes = FALSE,
    aes(
      y = ifelse(sens_sigma > 0 , -.7, -.7),
      x = factor(sample_sizes),
      label = paste0(
        "Relative Empirical Standard Error \n\n",
        "Relative Root Mean Squared Error \n\n"
      )
    ),
    vjust =1,
    hjust = .5,
    size = 2.2,
    col = "grey20"
  ) +
  geom_text(
    data = comparison_statistics,
    inherit.aes = FALSE,
    aes(
      y = ifelse(sens_sigma > 0 , -.7, -.7),
      x = factor(sample_sizes),
      label = paste0(
        "\n", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.1f", relative_empse)), "%\n",
        "\n", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.1f", relative_rmse)), "%\n"
      )
    ),
    vjust = ifelse(results_table_cleaned2$sens_sigma > 0, 1, 1),
    hjust = .5,
    size = 2.2,
    col = "grey20"
  ) +
  facet_grid(
    cols = vars(n_items),
    rows = vars(sens_sigma),
    labeller = labeller(
      n_items = n_items_labels,
      sens_sigma = sens_sigma_labels
    )
  ) +
  labs(
    y = "Sample reliability estimate",
    x = "Simulation Sample Size"
    ) +
  guides(
    fill=guide_legend(title="Estimator")
    ) +
  ggplot2::theme_bw() +
  theme(
    legend.position = "none",
  ) +
  coord_cartesian(ylim = c(-1.2,.9))

plot_violinplot_comparison

ggsave(file.path("plots","3_study2_violinplot_comparison.png"), plot = plot_violinplot_comparison, width = 6.2, height = 7)
ggsave(file.path("plots","3_study2_violinplot_comparison.pdf"), plot = plot_violinplot_comparison, width = 6.2, height = 7)

## Credible / confidence interval plots -------------------------------------------
# Per-simulation 95% interval (credible for RMU, confidence for split-half),
# ordered by point estimate within each condition, coloured by whether the
# interval contains the estimand (mean test-retest reliability, pooled across
# sample_sizes since it doesn't meaningfully vary with n). Mirrors the
# "Credible interval plot" in 2_study1_2_analysis.R.

ci_plot_data = results_table_long %>%
  group_by(n_items, sens_sigma, name) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(n_items, sens_sigma, sample_sizes, name) %>%
  arrange(est, .by_group = TRUE) %>%
  mutate(
    x          = 1:n(),
    ci_correct = (lb <= estimand & ub >= estimand)
  ) %>%
  ungroup() %>%
  mutate(
    name = factor(name, levels = c("rmu", "sh"), labels = c("RMU", "Split-Half"))
  )

plot_ci_rmu = ci_plot_data %>%
  filter(name == "RMU") %>%
  ggplot(aes(ymin = lb, ymax = ub, x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) +
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_wrap(~ sens_sigma + n_items + sample_sizes, scales = "free", ncol = 3) +
  labs(x = NULL, y = "RMU reliability estimate", col = "CI contains\nestimand",
       title = "RMU: 95% credible intervals per simulated condition") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_ci_rmu

ggsave(file.path("plots","3_study2_ci_plot_rmu.png"), plot = plot_ci_rmu, width = 9, height = 11)
ggsave(file.path("plots","3_study2_ci_plot_rmu.pdf"), plot = plot_ci_rmu, width = 9, height = 11)

plot_ci_splithalf = ci_plot_data %>%
  filter(sample_sizes == 250) %>%
  filter(name == "Split-Half") %>%
  ggplot(aes(ymin = lb, ymax = ub, x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) +
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_wrap(~ sens_sigma + n_items + sample_sizes, scales = "free", ncol = 3) +
  labs(x = NULL, y = "Split-half reliability estimate", col = "CI contains\nestimand",
       title = "Split-Half: 95% confidence intervals per simulated condition") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_ci_splithalf

ggsave(file.path("plots","3_study2_ci_plot_splithalf.png"), plot = plot_ci_splithalf, width = 9, height = 11)
ggsave(file.path("plots","3_study2_ci_plot_splithalf.pdf"), plot = plot_ci_splithalf, width = 9, height = 11)

# Deprecated / Scratch ------------------------------------------------------------

library(dplyr)
library(brms)

sim_data <- results_table_long %>%
  mutate(
    n_items      = factor(n_items),
    sens_sigma   = factor(sens_sigma),
    sample_sizes = factor(sample_sizes)
  ) %>%
  select(-contains("diag"), -rowid, -contains("sh"),
         -testretest_cor_dprime, -settings_used, -settings_used_with_npps) %>%
  filter(name == "rmu") %>%
  group_by(n_items, sens_sigma) %>%
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(n_items, sens_sigma, sample_sizes) %>%
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  ungroup()


bform <- bf(
  difference ~ n_items * sens_sigma * sample_sizes,
  sigma ~ n_items + sens_sigma + sample_sizes
)

fit_var <- brm(
  formula = bform,
  data = sim_data,
  family = gaussian(),
  cores = 2,
  chains = 2,
  iter = 2000,
  backend = "cmdstanr",
  threads = threading(6),
)

summary(fit_var)

# Calculate the grand marginal mean across all conditions
overall_bias <- emmeans::emmeans(fit_var, ~ 1)
summary(overall_bias)
