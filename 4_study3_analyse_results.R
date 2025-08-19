# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results_path = file.path("results","study3_results")

results_files = list.files(results_path, 
                           pattern = "^study3_results_seed_",
                           # pattern = "^study3_results_seed_",
                           recursive = FALSE,
                           full.names = TRUE
)

results_files = results_files[!grepl("/OLD/",results_files)]

# results_files = results_files[-4:-7]

results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)

# Including individual saves

results_files_large = list.files(file.path("results","study3_results","s3res_large"), 
                                                                  
                                 # pattern = "^study3",
                                 # pattern = "^study3_results_seed_",
                                 recursive = FALSE,
                                 full.names = TRUE
)

results_large = lapply(results_files_large, function(x) readRDS(x))
results_large = do.call("c",results_large)

results = c(results, results_large)

# Create results table----------------------------------------------------------

did_model_run = rep(NA, length(results))
for(i in 1:length(results)){
  did_model_run[i] = !identical(results[[i]]$cor_bayes_estimate_true,9999)
}

results = results[(did_model_run)]

results_table = data.frame(i = 1:length(results))

# Pop Reliability estimates
# results_table$true_score_cor2         = simulated_reliabilities[match(results_table$settings_used,settings_used)]
results_table$true_score_cor2 = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
results_table$true_score_cor2 = ifelse(is.na(results_table$true_score_cor2), 0 , results_table$true_score_cor2)
results_table$avg_true_score_coverage = sapply(results, function(x) x$avg_true_score_coverage) %>% as.numeric()

# Settings

results_table$n_pps               = sapply(results, function(x) x$settings$n_pps) %>% as.numeric()
results_table$sample_sizes        = sapply(results, function(x) x$settings$n_pps) %>% as.numeric()
results_table$n_trials            = sapply(results, function(x) x$settings$n_trials) %>% as.numeric()
results_table$learning_rate_mean  = sapply(results, function(x) x$settings$learning_rate_mean) %>% as.numeric()
results_table$learning_rate_sd    = sapply(results, function(x) x$settings$learning_rate_sd) %>% as.numeric()
results_table$decision_noise_mean = sapply(results, function(x) x$settings$decision_noise_mean) %>% as.numeric()
results_table$decision_noise_sd   = sapply(results, function(x) x$settings$decision_noise_sd) %>% as.numeric()

# Relative Measurement Precision
results_table$rmp_est       = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
results_table$rmp_lb        = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
results_table$rmp_ub        = sapply(results, function(x) x$rmp[3]) %>% as.numeric()
results_table$rmp_ci_length = results_table$rmp_ub - results_table$rmp_lb
results_table$rmp_pd        = sapply(results, function(x) x$rmp_pd) %>% as.numeric()

# model parameters
results_table$sigma1                    = sapply(results, function(x) x$stan_results_summary[4, "mean"]) %>% as.numeric()
results_table$mean_pps_learning_rate    = sapply(results, function(x) x$stan_results_summary[6, "mean"]) %>% as.numeric()
results_table$sd_pps_learning_rate      = sapply(results, function(x) x$stan_results_summary[7, "mean"]) %>% as.numeric()
results_table$mean_pps_decision_noise   = sapply(results, function(x) x$stan_results_summary[8, "mean"]) %>% as.numeric()
results_table$sd_pps_decision_noise     = sapply(results, function(x) x$stan_results_summary[9, "mean"]) %>% as.numeric()
results_table$mean_dist_learning_rate   = sapply(results, function(x) x$stan_results_summary[10,"mean"]) %>% as.numeric()
results_table$mean_dist_decision_noise  = sapply(results, function(x) x$stan_results_summary[11,"mean"]) %>% as.numeric()

# Other metrics

results_table$cor_bayes_estimate_true = sapply(results, function(x) x$cor_bayes_estimate_true$estimate) %>% as.numeric()
results_table$cor_bayes_estimate_true[is.na(results_table$cor_bayes_estimate_true)] = 0
results_table$cor_bayes_estimate_true_lb = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[1]) %>% as.numeric()
results_table$cor_bayes_estimate_true_lb[is.na(results_table$cor_bayes_estimate_true_lb)] = 0
results_table$cor_bayes_estimate_true_ub = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[2]) %>% as.numeric()
results_table$cor_bayes_estimate_true_ub[is.na(results_table$cor_bayes_estimate_true_ub)] = 0

#Diagnostics

# results_table$diag_divergences        = sapply(results, function(x) x$diagnostics$diag_divergences) %>% as.numeric()
results_table$diag_divergences = sapply(results, function(x) {
  tryCatch(
    if (is.null(x$diagnostics$diag_divergences)) NA
    else x$diagnostics$diag_divergences,  
    error = function(e) NA
  )
}) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi              = sapply(results, function(x) length(which(x$diagnostics$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary       = as.numeric(results_table$diag_ebfmi>0)


# Add population reliability

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

# results_table$pop_rel = simulated_reliabilities[match(results_table$settings_used, settings_used)]

# Performance measures ---------------------------------------------------------

## Check for model-fitting issues ----------------------------------------------

results_table$diag_ebfmi_binary %>% table()
results_table$diag_divergences_binary %>% table()

## Overall Performance ---------------------------------------------------------

results_table %>%
  # filter(sample_sizes == 240) %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    pop_rel = (sum(true_score_cor2) - true_score_cor2) / (n() - 1),
    # pop_rel     =  mean(true_score_cor2),
    mean_rmp_est = mean(rmp_est)
  ) %>% 
  mutate(
    est        = rmp_est,
    ub         = rmp_ub,
    lb         = rmp_lb,
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb,
    ci_mean_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_be_correct = (lb <= mean_rmp_est & ub >= mean_rmp_est),
  ) %>%
  ungroup() %>%
  # group_by(sample_sizes) %>%
  summarise(
    n = n(),
    pop_rel     = mean(pop_rel),
    pop_rel_sd      = sd(pop_rel),
    
    # Mean Estimate
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,
    
    # Empirical Standard Error 
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,
    
    #bias
    bias        = mean(difference),
    bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,
    
    # Mean absolute deviation
    mad         = mean(abs(difference)),
    
    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,
    
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),
    
    #Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    
    # Bias corrected coverage 
    coverage_be    = length(which(ci_be_correct))/length(ci_be_correct),
    coverage_be_se = sqrt((coverage_be*(1-coverage_be))/n),
    coverage_be_lb = coverage_be - qnorm(0.975)*coverage_be_se,
    coverage_be_ub = coverage_be + qnorm(0.975)*coverage_be_se,
    
    mean_ci_length = mean(ci_length),
    mean_ts_coverage = mean(avg_true_score_coverage),
    n_diag_divergences_binary = sum(diag_divergences_binary),
    n_diag_ebfmi_binary       = sum(diag_ebfmi_binary),
    # sigma1                    = mean(sigma1),
    # mean_pps_learning_rate    = mean(mean_pps_learning_rate),
    # sd_pps_learning_rate      = mean(sd_pps_learning_rate),
    # mean_pps_decision_noise   = mean(mean_pps_decision_noise),
    # sd_pps_decision_noise     = mean(sd_pps_decision_noise),
    # mean_dist_learning_rate   = mean(mean_dist_learning_rate),
    # mean_dist_decision_noise  = mean(mean_dist_decision_noise)
  )  %>%
  ungroup() %>%
  knitr::kable(digits = 3)


## GT table of performance in each condition ---------------------------------------------------

results_table_cleaned = results_table %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    pop_rel = (sum(true_score_cor2) - true_score_cor2) / (n() - 1),
    # pop_rel     =  mean(true_score_cor2),
    mean_rmp_est = mean(rmp_est)
    ) %>% 
  mutate(
    est        = rmp_est,
    ub         = rmp_ub,
    lb         = rmp_lb,
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb,
    ci_mean_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_be_correct = (lb <= mean_rmp_est & ub >= mean_rmp_est),
  ) %>%
  summarise(
    n = n(),
    pop_rel     = mean(pop_rel),
    pop_rel_sd      = sd(pop_rel),
    
    # Mean Estimate
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,
    
    # Empirical Standard Error 
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,
    
    #bias
    bias        = mean(difference),
    bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,
    
    # Mean absolute deviation
    mad         = mean(abs(difference)),
    
    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,
    
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),
    
    #Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    
    # Bias corrected coverage 
    coverage_be    = length(which(ci_be_correct))/length(ci_be_correct),
    coverage_be_se = sqrt((coverage_be*(1-coverage_be))/n),
    coverage_be_lb = coverage_be - qnorm(0.975)*coverage_be_se,
    coverage_be_ub = coverage_be + qnorm(0.975)*coverage_be_se,
    
    mae         = mean(abs(difference)),

    mean_ci_length = mean(ci_length),
    mean_ts_coverage = mean(avg_true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n,
    sigma1                    = mean(sigma1),
    mean_pps_learning_rate    = mean(mean_pps_learning_rate),
    sd_pps_learning_rate      = mean(sd_pps_learning_rate),
    mean_pps_decision_noise   = mean(mean_pps_decision_noise),
    sd_pps_decision_noise     = mean(sd_pps_decision_noise),
    mean_dist_learning_rate   = mean(mean_dist_learning_rate),
    mean_dist_decision_noise  = mean(mean_dist_decision_noise)
    )  %>%
  ungroup() %>% 
  # knitr::kable(digits =2)
  select(-pop_rel_sd, -coverage_se) %>%
  select(pop_rel,sample_sizes,learning_rate_sd, everything())

# learning_rate_labels <- c(
#   "0"   = "Learning Rate SD = 0",
#   "0.2" = "Learning Rate SD = 0.056",
#   "0.4" = "Learning Rate SD = 0.112"
# )

results_table_cleaned %>%
  mutate(learning_rate_sd = case_when(
    learning_rate_sd == 0  ~ 0,
    learning_rate_sd == .2 ~ 0.056,
    learning_rate_sd == .4 ~ .112,
    TRUE ~ NA_real_  # Catches any other values
  )) %>%
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
    columns = c(starts_with("coverage"),mean_ts_coverage),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"),contains("perc_diag")),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",      # avg_cor2     ~ "{{:rho:_:theta:,x^2}}",
    # avg_cor2      ~ "{{mean[:rho:_:theta:,x^2 ]}}",
    pop_rel ~ "estimand",
    n_trials      ~ "{{n_trials}}",
    bias           ~ "bias",
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
    coverage_be ~ md("Bias<br>Eliminated<br>Coverage"),
    mean_ts_coverage ~ md("True<br>Score<br>Coverage"),
    learning_rate_sd ~ "{{:sigma:_learnrate}}",
    perc_diag_divergences_binary ~ md("% Divergent<br>Transitions"),
    perc_diag_ebfmi_binary ~   md("% Low<br>E-BFMI")
  )  %>%
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
  tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(pop_rel,learning_rate_sd,n_trials,sample_sizes,n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(RMSE, RMSE_lb, RMSE_ub, bias, bias_lb, bias_ub,EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(coverage, coverage_lb, coverage_ub,mean_ci_length,coverage_be  )) %>%
  cols_move(
    columns = coverage_be,
    after = mean_ci_length
  ) %>%
  tab_footnote(
    footnote = html("<b>n<sub>sim</sub></b> = number of simulations completed for this set of simulation parameters.
                <b>n<sub>obs</sub></b> = number of subjects per simulation.
                <b>RMSE</b> = Root Mean Squared Error.
                <b>Coverage</b> = proportion of times the 95% credible intervals include the population reliability, which should be around 95%.
                <b>estimand</b> = ASCOTS (Average Squared Correlation between Observed and True Scores).
                <b>Mean Length</b> = Mean length of credible or confidence interval.
                <b>σ<sub>learnrate</sub></b> = standard deviation of population true learning rates across subjects.
                <b>n<sub>trials</sub></b> = number of trials completed per participant.
                <b>EmpSE</b> = Empirical Standard Error (sd of RMU estimates).
                <b>Bias-eliminated coverage</b> = percentage of simulations where the 95% credible intervals contain the mean RMU estimate on a given condition.
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
      mae, bias_se, mean, ends_with("_se"),mean_lb,mean_ub, mad,
      starts_with("MSE"), starts_with("coverage_be_"),
      mean_pps_decision_noise,mean_dist_learning_rate, sd_pps_learning_rate, sd_pps_decision_noise, mean_pps_learning_rate, mean_dist_decision_noise, sigma1,
      perc_diag_divergences_binary, perc_diag_ebfmi_binary
    )
  ) %>%
  gtsave(filename = file.path("results_tables","4_study3_supplementary_table.html"))




##### Using raw data -----------------------------------------------------------

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

results_table  %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    mean_true_score_model_score_cor2 = mean(true_score_cor2),
    sample_sizes = factor(sample_sizes),
    est = rmp_est
  ) %>%
  ggplot(aes(y = est, x = sample_sizes)) +
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
               # y = mean,
               size = 3,
               shape = 3,
               col = "red",
               stroke = .9
  ) +
  geom_point(
    data = results_table_cleaned,
    aes(
      y  = pop_rel,
      x = factor(sample_sizes)
    ),
    # alpha = .6,
    shape = 1,
    size = 2,
    col = "red",
    stroke = .9
  ) +
  geom_rect(
    data = results_table,
    inherit.aes = FALSE,
    aes(
      xmin = .5,
      xmax = 3.5,
      # Position the rectangle differently based on conditions
      ymin = ifelse(learning_rate_sd == 0.4 & n_trials > 100, .06, 0.69),
      ymax = ifelse(learning_rate_sd == 0.4 & n_trials > 100, 0.27, 0.91)
    ),
    fill = "white",
    alpha = 0.4
  ) +
  geom_text(
    data = results_table_cleaned %>% filter(),
    aes(
      y = ifelse(learning_rate_sd == 0.4 & n_trials >= 200, 0, 0.89),
      x = factor(sample_sizes),
      label = paste0(
        "B = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", bias)), "\n",
        "E = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", RMSE)), "\n",
        "C = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", coverage)), "\n"
        # "EmpSE = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", EmpSE)), "\n",
        # "N = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.f", n))
      )
    ),
    # parse = TRUE,
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
  guides(col=guide_legend(title="Estimator")) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom")
  )

ggsave(file.path("plots","4_study3_violinplot.png"), width = 6.2, height = 7)
ggsave(file.path("plots","4_study3_violinplot.pdf"), width = 6.2, height = 7)

### Distribution of point-estimates --------------------------------------------

# Dotplot 

results_table_long = results_table %>%
  # select(-any_of(c(starts_with("diag")))) %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est,
                        rmp_lb,
                        rmp_ub
  ), names_to = c("name", ".value"), names_pattern = "(rmp|h|a|irth)_(.*)") 


results_table_long  %>%
  mutate(sample_sizes = factor(sample_sizes)) %>%
  ggplot(aes(y = est, x = sample_sizes)) +
  geom_jitter(width = 0.1, height = 0, shape = 1, alpha = .5) +
  # geom_hline(aes(yintercept = pop_rel)) +
  stat_summary(fun.data = ggplot2::mean_cl_normal,
               geom = "errorbar",
               size = 1
  ) +
  geom_point(data = results_table_cleaned %>% mutate(sample_sizes = factor(sample_sizes)),aes(y = pop_rel), col = "red", size = 5) +
  facet_wrap(~ n_trials +learning_rate_sd  ) +
  labs(y = "Sample reliability estimate", x = "Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    # legend.box.just = "right",
    # legend.margin = margin(6, 6, 6, 6)
  ) 

### Plot Credible Intervals ----------------------------------------------------

results_table %>%
  filter(sample_sizes==240) %>%
  arrange(rmp_est) %>%  
  group_by(learning_rate_sd, n_trials) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    x = 1:n(),
    estimand = mean(true_score_cor2),                           # mean correlation between bayes IRT score and true score
    ci_correct = (rmp_lb <= estimand & rmp_ub >= estimand),
  ) %>%
  ungroup() %>%
  ggplot(aes(ymin = rmp_lb, ymax = rmp_ub, 
             x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) + 
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_grid(
    cols = vars(n_trials),
    rows = vars(learning_rate_sd),
    labeller = labeller(
      n_trials = n_trials_labels,
      learning_rate_sd = learning_rate_labels
    )
  ) 

### Plot of distribution of learning rates -------------------------------------

sd(g_normaluniform(400000000, .2, .2)) # SD of learning rate in second conditions
sd(g_normaluniform(400000000, .2, .4)) # SD of learning rate in third condition 

source(file.path("helper_functions","g_normaluniform.R"))

n = 1000000
d1 = g_normaluniform(n, .2, 0)

ggplot(data.frame(d1 = d1), aes(x = d1)) +
  # geom_density(fill = "grey", alpha = 0.5) +
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
  # geom_histogram() +
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
  # geom_histogram() +
  theme_bw() +
  labs(x = NULL, y = "Learning Rate Distribution") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(file.path("plots","4_learningrate_density_3.png"),width = 4, height = 3)









# Modelling ------------------------------------------------------


results_table %>%
  mutate(
    learning_rate_sd = factor(learning_rate_sd),
    n_trials         = factor(n_trials)
  ) %>%
  lm(
    cor_bayes_estimate_true ~ learning_rate_sd + n_trials + sample_sizes,
    data = .
    ) %>%
  summary()


#ggeom_histogram()#gtsave(filename = file.path("results","7_results_table_1.html"))

# Examine coverage issues in more detail ---------------------------------------

# results_table %>%
#   mutate(
#     sample_sizes = factor(sample_sizes),
#     learning_rate_sd  = factor(learning_rate_sd),
#     n_trials     = factor(n_trials),
#     true_score_cor2 = cor_bayes_estimate_true^2
#   ) %>%
#   ggplot(aes( x = pop_rel, y = rmp_est, shape = sample_sizes, col = n_trials)) + 
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1) +
#   facet_wrap(~learning_rate_sd + sample_sizes)
# 
# results_table %>%
#   mutate(
#     sample_sizes = factor(sample_sizes),
#     learning_rate_sd  = factor(learning_rate_sd),
#     n_trials     = factor(n_trials),
#     true_score_cor2 = cor_bayes_estimate_true^2
#   ) %>%
#   ggplot(aes( x = pop_rel, y = true_score_cor2, shape = sample_sizes, col = n_trials)) + 
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1) +
#   facet_wrap(~learning_rate_sd + sample_sizes)

# results_table %>%
#   # filter(sample_sizes==1000) %>%
#   # filter(n_trials ==200) %>%
#   # filter(learning_rate_sd==.15) %>%
#   mutate(
#     sample_sizes = factor(sample_sizes),
#     learning_rate_sd  = factor(learning_rate_sd),
#     n_trials     = factor(n_trials),
#     true_score_cor2 = cor_bayes_estimate_true^2
#   ) %>%
#   arrange(rmp_est) %>%
#   group_by(sample_sizes,learning_rate_sd, n_trials) %>%
#   mutate(i = 1:n()) %>%
#   ggplot(aes( x = i, y = rmp_est, shape = sample_sizes, col = sample_sizes)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = rmp_lb, ymax = rmp_ub)) +
#   # geom_hline(aes(yintercept =pop_rel)) +
#   coord_cartesian(ylim =c(0,1)) 


results_table %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(pop_rel = mean(true_score_cor2_loo)) %>% 
  mutate(
    sample_sizes = factor(sample_sizes),
    learning_rate_sd  = factor(learning_rate_sd),
    n_trials     = factor(n_trials),
    true_score_cor2 = cor_bayes_estimate_true^2
  ) %>%
  arrange(rmp_est) %>%
  # group_by(sample_sizes,learning_rate_sd, n_trials) %>%
  mutate(i = 1:n()) %>%
  ggplot(aes( x = i, y = rmp_est, shape = sample_sizes, col = n_trials)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = rmp_lb, ymax = rmp_ub), alpha = .03) +
  geom_hline(aes(yintercept =pop_rel, col = n_trials), size = 1.2) +
  coord_cartesian(ylim =c(-.12,1)) +
  theme_bw() +
  facet_wrap(~ learning_rate_sd)




test = 
results_table %>%
  filter(sample_sizes==200) %>%
  filter(n_trials ==50) %>%
  filter(learning_rate_sd==0) 

table((test$rmp_lb<=0) & (test$rmp_ub>=0))



# Check for any identical seeds ------------------------------------------------

n_check = 2000
n_results <- length(results)
matches <- matrix(FALSE, nrow=n_check, ncol=n_results)

for(i in 1:n_check) {
  for(j in (i+1):n_results) {
    matches[i,j] <- identical(results[[i]]$settings$seed, 
                              results[[j]]$settings$seed)
  }
}

# Check if we found any matches
if(any(matches)) {
  # Get indices of matches
  which(matches, arr.ind=TRUE)
} else {
  print("No identical RNG states found!")
}

identical(results[[1]]$settings$seed,results[[2]]$settings$seed)


table(duplicated(results_table$rmp_est))

table(duplicated(results_table$rmp_ub))

# table(duplicated(results_table$))




# Distribution of learning rates -----------------------------------------------

g_normaluniform(100000, .2, .1) %>% hist()

g_normaluniform(100000, .2, .3) %>% hist()

g_normaluniform(100000, .5, 1) %>% hist()

g_normaluniform(100000, .5, .1) %>% hist()

# learning_rate_mean  = 0.5,
# learning_rate_sd    = c(0, .3, 1),


results_table$rmp_est %>% is.na() %>% which()

results[[297]]

.Random.seed = results[[297]]$settings$seed
