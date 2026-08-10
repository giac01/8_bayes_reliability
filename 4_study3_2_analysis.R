# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)
library(clubSandwich)

rm(list = ls(all.names = TRUE))

# Cluster-robust SE of a mean, clustering by sim_id -----------------------------

# results_table has two rows per simulation (t1/t2 waves) that share the same
# underlying subject-level truth and are therefore not independent. The naive SE
# formulas below (e.g. sqrt(1/(n*(n-1))*sum((est-mean)^2))) assume iid rows, so
# they understate the Monte Carlo SE of bias/MSE/coverage. This replaces them
# with a cluster-robust (CR2) SE of the mean, via a trivial intercept-only lm().
# Mirrors 3_study2_2_analysis.R.

cluster_se_mean = function(x, cluster){
  fit = lm(x ~ 1)
  sqrt(vcovCR(fit, cluster = cluster, type = "CR2")[1, 1])
}

# Load Data --------------------------------------------------------------------

# NB: this used to also merge in results from the "s3res_large" subfolder,
# produced by 4_study3_1_largesamplesize_simulate.R. That script still runs the
# old single-occasion (no t1/t2) design and hasn't been updated to match, so its
# output is no longer structurally compatible with results_table below and is
# excluded here.

results_path = file.path("results","study3_results")

results_files = list.files(results_path,
                           pattern = "^study3_results_seed_",
                           recursive = FALSE,
                           full.names = TRUE
)

results_files = results_files[!grepl("/OLD/",results_files)]

results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)

## Extract wave-level (t1 / t2) results into a long results_table --------------

# Each simulation now fits the RL model separately at t1 and t2 (a full
# test-retest design), producing two waves of RMP estimates. We treat each wave
# as a separate replicate of the same underlying condition, so a single
# simulation contributes two rows to results_table. Mirrors 3_study2_2_analysis.R.

extract_wave = function(results, wave){

  rmp_field      = paste0("rmp_est_", wave)
  div_field      = paste0("diag_divergences_", wave)
  ebfmi_field    = paste0("diagnostics_ebfmi_", wave)
  cor_field      = paste0("cor_bayes_estimate_true_", wave)
  coverage_field = paste0("avg_true_score_coverage_", wave)
  summary_field  = paste0("stan_results_summary_", wave)

  # sim_id identifies which element of the `results` list this row came from.
  # Each simulation iteration appears twice in the final results_table (once
  # per wave), sharing the same sim_id.
  df = data.frame(sim_id = 1:length(results))

  df$time                = wave
  df$n_pps               = sapply(results, function(x) x$settings$n_pps) %>% as.numeric()
  df$sample_sizes        = df$n_pps
  df$n_trials            = sapply(results, function(x) x$settings$n_trials) %>% as.numeric()
  df$learning_rate_mean  = sapply(results, function(x) x$settings$learning_rate_mean) %>% as.numeric()
  df$learning_rate_sd    = sapply(results, function(x) x$settings$learning_rate_sd) %>% as.numeric()
  df$decision_noise_mean = sapply(results, function(x) x$settings$decision_noise_mean) %>% as.numeric()
  df$decision_noise_sd   = sapply(results, function(x) x$settings$decision_noise_sd) %>% as.numeric()

  # Test-retest reliability (correlation of the Bayesian learning-rate point
  # estimates between t1 and t2) is used as the estimand below. Unlike the old
  # ground-truth-based rmp, it doesn't require knowledge of the true simulated
  # learning rates, and is a single value per simulation (identical for its t1
  # and t2 rows, since it is computed jointly from both waves).
  df$test_retest_reliability = sapply(results, function(x) x$test_retest_reliability) %>% as.numeric()

  df$rmp_est = sapply(results, function(x) x[[rmp_field]]$rmu_estimate)    %>% as.numeric()
  df$rmp_lb  = sapply(results, function(x) x[[rmp_field]]$hdci_lowerbound) %>% as.numeric()
  df$rmp_ub  = sapply(results, function(x) x[[rmp_field]]$hdci_upperbound) %>% as.numeric()
  df$rmp_ci_length = df$rmp_ub - df$rmp_lb

  # Ground-truth checks (still available here since sim_ri() knows the true
  # simulated learning rate per subject) - kept as secondary diagnostics, not
  # the primary estimand.
  df$cor_bayes_estimate_true = sapply(results, function(x) x[[cor_field]]$estimate) %>% as.numeric()
  df$cor_bayes_estimate_true[is.na(df$cor_bayes_estimate_true)] = 0
  df$true_score_cor2 = df$cor_bayes_estimate_true^2
  df$avg_true_score_coverage = sapply(results, function(x) x[[coverage_field]]) %>% as.numeric()

  # Model parameters (only present when additional_tests==TRUE)
  get_summary_value = function(x, row){
    tryCatch({
      s = x[[summary_field]]
      if (is.null(s)) NA else as.numeric(s[row, "mean"])
    }, error = function(e) NA)
  }
  df$sigma1                   = sapply(results, get_summary_value, row = 4)
  df$mean_pps_learning_rate   = sapply(results, get_summary_value, row = 6)
  df$sd_pps_learning_rate     = sapply(results, get_summary_value, row = 7)
  df$mean_pps_decision_noise  = sapply(results, get_summary_value, row = 8)
  df$sd_pps_decision_noise    = sapply(results, get_summary_value, row = 9)
  df$mean_dist_learning_rate  = sapply(results, get_summary_value, row = 10)
  df$mean_dist_decision_noise = sapply(results, get_summary_value, row = 11)

  df$diag_divergences = sapply(results, function(x) {
    tryCatch(if (is.null(x[[div_field]])) NA else x[[div_field]], error = function(e) NA)
  }) %>% as.numeric()
  df$diag_divergences_binary = as.numeric(df$diag_divergences>0)
  df$diag_ebfmi              = sapply(results, function(x) length(which(x[[ebfmi_field]]<.2)))
  df$diag_ebfmi_binary       = as.numeric(df$diag_ebfmi>0)

  df
}

## Filter out replicates where either wave's model failed to fit ---------------

did_model_run = sapply(results, function(x) isTRUE(x$model_exists_t1) && isTRUE(x$model_exists_t2))
table(did_model_run)
results = results[did_model_run]

results_table = bind_rows(
  extract_wave(results, "t1"),
  extract_wave(results, "t2")
)

results_table$settings_used = paste(
  results_table$n_trials,
  results_table$learning_rate_mean,
  results_table$learning_rate_sd,
  results_table$decision_noise_mean,
  results_table$decision_noise_sd,
  sep = "_"
)

results_table$settings_used_with_npps = paste(
  results_table$n_pps,
  results_table$n_trials,
  results_table$learning_rate_mean,
  results_table$learning_rate_sd,
  results_table$decision_noise_mean,
  results_table$decision_noise_sd,
  sep = "_"
)

results_table$sample_sizes = factor(results_table$sample_sizes)

# Performance Measures ---------------------------------------------------------

## Check for model-fitting issues ----------------------------------------------

results_table$diag_ebfmi_binary %>% table()
results_table$diag_divergences_binary %>% table()

## Overall Performance ---------------------------------------------------------

results_table %>%
  group_by(learning_rate_sd, n_trials) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n
  mutate(
    estimand   = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over waves (t1/t2); performance metrics still broken down by sample size
  mutate(
    difference = rmp_est - estimand,
    ci_correct = (rmp_lb <= estimand & rmp_ub >= estimand),
    ci_length  = rmp_ub - rmp_lb
  ) %>%
  summarise(
    n = n(),
    estimand    = mean(estimand),
    estimand_sd = sd(test_retest_reliability),

    mean        = mean(rmp_est),

    bias        = mean(difference),
    bias_se     = cluster_se_mean(difference, sim_id),                  # cluster-robust (CR2), clustered by sim_id
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    EmpSE       = sd(rmp_est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    mad         = mean(abs(difference)),

    MSE         = mean((difference)^2),
    MSE_se      = cluster_se_mean(difference^2, sim_id),                # cluster-robust (CR2), clustered by sim_id
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = cluster_se_mean(as.numeric(ci_correct), sim_id),      # cluster-robust (CR2), clustered by sim_id
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mean_ci_length   = mean(ci_length),
    mean_ts_coverage = mean(avg_true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
  )  %>%
  ungroup() %>%
  knitr::kable(digits = 3)

## GT table of performance in each condition ------------------------------------

results_table_cleaned = results_table %>%
  group_by(learning_rate_sd, n_trials) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over waves (t1/t2); performance metrics still broken down by sample size
  mutate(
    difference = rmp_est - estimand,
    ci_correct = (rmp_lb <= estimand & rmp_ub >= estimand),
    ci_length  = rmp_ub - rmp_lb
  ) %>%
  summarise(
    n = n(),
    estimand    = mean(estimand),
    estimand_sd = sd(test_retest_reliability),

    mean        = mean(rmp_est),

    bias        = mean(difference),
    bias_se     = cluster_se_mean(difference, sim_id),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,

    EmpSE       = sd(rmp_est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,

    mad         = mean(abs(difference)),

    MSE         = mean((difference)^2),
    MSE_se      = cluster_se_mean(difference^2, sim_id),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,

    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),

    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = cluster_se_mean(as.numeric(ci_correct), sim_id),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,

    mae         = mean(abs(difference)),

    mean_ci_length   = mean(ci_length),
    mean_ts_coverage = mean(avg_true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n,
    sigma1                       = mean(sigma1),
    mean_pps_learning_rate       = mean(mean_pps_learning_rate),
    sd_pps_learning_rate         = mean(sd_pps_learning_rate),
    mean_pps_decision_noise      = mean(mean_pps_decision_noise),
    sd_pps_decision_noise        = mean(sd_pps_decision_noise),
    mean_dist_learning_rate      = mean(mean_dist_learning_rate),
    mean_dist_decision_noise     = mean(mean_dist_decision_noise)
  )  %>%
  ungroup() %>%
  select(-estimand_sd, -coverage_se) %>%
  select(estimand, sample_sizes, learning_rate_sd, everything())

table_performance_comparison = results_table_cleaned %>%
  mutate(learning_rate_sd = case_when(
    learning_rate_sd == 0   ~ 0,
    learning_rate_sd == .25 ~ 0.070,
    learning_rate_sd == .5  ~ .138,
    TRUE ~ NA_real_  # Catches any other values
  )) %>%
  arrange(learning_rate_sd, n_trials, sample_sizes) %>%
  gt() %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n, n_trials, sample_sizes),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"), mean_ts_coverage, contains("perc_diag")),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",
    estimand     ~ "estimand",
    n_trials     ~ "{{n_trials}}",
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
    mean_ci_length ~ md("Mean<br>Length"),
    mean_ts_coverage ~ md("True<br>Score<br>Coverage"),
    learning_rate_sd ~ "{{:sigma:_learnrate}}",
    perc_diag_divergences_binary ~ md("% Divergent<br>Transitions"),
    perc_diag_ebfmi_binary ~   md("% Low<br>E-BFMI")
  )  %>%
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
  tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(estimand, learning_rate_sd, n_trials, sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(RMSE, RMSE_lb, RMSE_ub, bias, bias_lb, bias_ub, EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(coverage, coverage_lb, coverage_ub, mean_ci_length)) %>%
  tab_footnote(
    footnote = html("<b>n<sub>sim</sub></b> = number of simulations completed for this set of simulation parameters.
                <b>n<sub>obs</sub></b> = number of subjects per simulation.
                <b>RMSE</b> = Root Mean Squared Error.
                <b>Coverage</b> = proportion of times the 95% credible intervals include the estimand, which should be around 95%.
                <b>estimand</b> = test-retest reliability, i.e. the mean correlation between Bayesian learning-rate point-estimates at t1 and t2, pooled across all sample sizes for a given combination of trial number and learning-rate SD.
                <b>Mean Length</b> = Mean length of credible interval.
                <b>σ<sub>learnrate</sub></b> = standard deviation of population true learning rates across subjects.
                <b>n<sub>trials</sub></b> = number of trials completed per participant.
                <b>EmpSE</b> = Empirical Standard Error (sd of RMP estimates).
                <b>True score coverage</b> = proportion of times the 95% credible interval for each subject's learning rate contains the true learning rate for that subject."
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      rows = which((n_trials == 100))
    )
  ) %>%
  tab_options(
    table.width = pct(35)
  ) %>%
  gt::cols_hide(
    c(
      mae, bias_se, mean, ends_with("_se"), mad,
      starts_with("MSE"),
      mean_pps_decision_noise, mean_dist_learning_rate, sd_pps_learning_rate, sd_pps_decision_noise, mean_pps_learning_rate, mean_dist_decision_noise, sigma1
    )
  )

table_performance_comparison

gtsave(table_performance_comparison, filename = file.path("results_tables","4_study3_performance_comparison.html"))

# Test if estimand is affected by simulation conditions ------------------------

mod_estimand = results_table |>
  filter(time == "t1") |>
  mutate(across(c(learning_rate_sd, n_trials, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ learning_rate_sd*n_trials*sample_sizes, data = _)

mod_estimand0 = results_table |>
  filter(time == "t1") |>
  mutate(across(c(learning_rate_sd, n_trials, sample_sizes), factor)) |>
  lm(test_retest_reliability ~ learning_rate_sd*n_trials, data = _)

anova(mod_estimand)
anova(mod_estimand0, mod_estimand)

## Export as gt tables ----------------------------------------------------

table_estimand_anova = anova(mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "ANOVA: test-retest reliability ~ learning_rate_sd * n_trials * sample_sizes") %>%
  fmt_number(columns = c(sumsq, meansq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_anova

gtsave(table_estimand_anova, filename = file.path("results_tables", "4_study3_estimand_anova.html"))

table_estimand_modelcomparison = anova(mod_estimand0, mod_estimand) %>%
  broom::tidy() %>%
  gt() %>%
  tab_header(title = "Model comparison: learning_rate_sd * n_trials vs. learning_rate_sd * n_trials * sample_sizes") %>%
  fmt_number(columns = c(rss, sumsq, statistic, p.value), decimals = 3) %>%
  sub_missing(missing_text = "")

table_estimand_modelcomparison

gtsave(table_estimand_modelcomparison, filename = file.path("results_tables", "4_study3_estimand_modelcomparison.html"))

# Test if RMPs are clustered by simulation condition ---------------------------

mod = results_table |>
  lme4::lmer(rmp_est ~ 1 + (1 | sim_id), data = _)

performance::icc(mod)

# Check for any identical seeds ------------------------------------------------

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

# Plots -----------------------------------------------------------------------

## RMP violin plot ---------------------------------------------------------------

library(grid)

n_trials_labels <- c(
  "100" = "100 Trials",
  "200" = "200 Trials",
  "400" = "400 Trials"
)

learning_rate_labels <- c(
  "0"   = "Learning Rate SD = 0",
  "0.2" = "Learning Rate SD = 0.056",
  "0.4" = "Learning Rate SD = 0.112"
)

plot_violinplot = results_table %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over waves (t1/t2)
  mutate(
    sample_sizes = factor(sample_sizes)
  ) %>%
  ggplot(aes(y = rmp_est, x = sample_sizes)) +
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
    data = results_table_cleaned,
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
    data = results_table_cleaned,
    inherit.aes = FALSE,
    aes(
      xmin = .5,
      xmax = 2.5,
      ymin = ifelse(learning_rate_sd == 0.4 & n_trials > 100, .06, 0.69),
      ymax = ifelse(learning_rate_sd == 0.4 & n_trials > 100, 0.27, 0.91)
    ),
    fill = "white",
    alpha = 0.4
  ) +
  geom_text(
    data = results_table_cleaned,
    aes(
      y = ifelse(learning_rate_sd == 0.4 & n_trials >= 200, 0, 0.89),
      x = factor(sample_sizes),
      label = paste0(
        "B = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", bias)), "\n",
        "E = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", RMSE)), "\n",
        "C = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", coverage)), "\n"
      )
    ),
    vjust = ifelse(results_table_cleaned$learning_rate_sd == 0.4 &
                     results_table_cleaned$n_trials >= 200, 0, 1),
    hjust = 1,
    size = 2.6,
    position = position_nudge(x=.42),
    col = "grey20"
  ) +
  facet_grid(
    cols = vars(n_trials),
    rows = vars(learning_rate_sd),
    labeller = labeller(
      n_trials = n_trials_labels,
      learning_rate_sd = learning_rate_labels
    )
  ) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom")
  )

plot_violinplot

ggsave(file.path("plots","4_study3_violinplot.png"), plot = plot_violinplot, width = 6.2, height = 7)
ggsave(file.path("plots","4_study3_violinplot.pdf"), plot = plot_violinplot, width = 6.2, height = 7)

## Credible Intervals ------------------------------------------------------------

# Per-simulation 95% credible interval, ordered by point estimate within each
# condition, coloured by whether the interval contains the estimand (mean
# test-retest reliability, pooled across sample_sizes since it doesn't
# meaningfully vary with n). Mirrors the "Credible Intervals" plot in
# 2_study1_2_analysis.R / 3_study2_2_analysis.R.

ci_plot_data = results_table %>%
  group_by(learning_rate_sd, n_trials) %>%       # estimand pooled across sample_sizes since it doesn't meaningfully vary with n
  mutate(
    estimand = mean(test_retest_reliability)
  ) %>%
  ungroup() %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%
  arrange(rmp_est, .by_group = TRUE) %>%
  mutate(
    x          = 1:n(),
    ci_correct = (rmp_lb <= estimand & rmp_ub >= estimand)
  ) %>%
  ungroup()

plot_ci_rmp = ci_plot_data %>%
  ggplot(aes(ymin = rmp_lb, ymax = rmp_ub, x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) +
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_wrap(~ learning_rate_sd + n_trials + sample_sizes, scales = "free", ncol = 3) +
  labs(x = NULL, y = "RMP reliability estimate", col = "CI contains\nestimand",
       title = "RMP: 95% credible intervals per simulated condition") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_ci_rmp

ggsave(file.path("plots","4_study3_ci_plot_rmp.png"), plot = plot_ci_rmp, width = 9, height = 11)
ggsave(file.path("plots","4_study3_ci_plot_rmp.pdf"), plot = plot_ci_rmp, width = 9, height = 11)

### Plot of distribution of learning rates -------------------------------------

source(file.path("helper_functions","g_normaluniform.R"))

sd(g_normaluniform(400000000, .2, .2)) # SD of learning rate in second condition
sd(g_normaluniform(400000000, .2, .4)) # SD of learning rate in third condition

n = 1000000
d1 = g_normaluniform(n, .2, 0)

ggplot(data.frame(d1 = d1), aes(x = d1)) +
  xlim(c(0,1)) +
  geom_histogram() +
  theme_bw() +
  labs(x = NULL, y = "Learning Rate Distribution") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(file.path("plots","4_learningrate_density_1.png"),width = 4, height = 3)

n = 10000000
d1 = g_normaluniform(n, .2, .2)

ggplot(data.frame(d1 = d1), aes(x = d1)) +
  geom_density(fill = "grey", alpha = 1) +
  xlim(c(0,1)) +
  theme_bw() +
  labs(x = NULL, y = "Learning Rate Distribution") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(file.path("plots","4_learningrate_density_2.png"),width = 4, height = 3)

n = 10000000
d1 = g_normaluniform(n, .2, .4)

ggplot(data.frame(d1 = d1), aes(x = d1)) +
  geom_density(fill = "grey", alpha = 1) +
  xlim(c(0,1)) +
  theme_bw() +
  labs(x = NULL, y = "Learning Rate Distribution") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(file.path("plots","4_learningrate_density_3.png"),width = 4, height = 3)
