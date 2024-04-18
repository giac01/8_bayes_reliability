# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results_path = file.path("results","7_ri_results")

results_files = list.files(results_path, 
                           pattern = "^7_results_",
                           recursive = FALSE,
                           full.names = TRUE
)
results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)

# results_pop = readRDS(file = file.path("results","7_results_pop_c.rds"))

# Sample size used in "population" simulations. 
# results_pop[[1]]$settings$n_pps

## Extract our estimates of the "true" reliabilities ---------------------------

# simulated_reliabilities         = sapply(results_pop, function(x) x$cor_bayes_estimate_true$estimate^2) 
# simulated_reliabilities         = ifelse(is.na(simulated_reliabilities), 0,  simulated_reliabilities)
# sens_mean                       = sapply(results_pop, function(x) x$settings$sens_mean)
# sens_sigma                      = sapply(results_pop, function(x) x$settings$sens_sigma) 
# k_mean                          = sapply(results_pop, function(x) x$settings$k_mean) 
# k_sigma                         = sapply(results_pop, function(x) x$settings$k_sigma) 
# n_items                         = sapply(results_pop, function(x) x$settings$n_items) 
# n_pps                           = sapply(results_pop, function(x) x$settings$n_pps) 
# 
# settings_used = paste(sens_mean, sens_sigma, k_mean, k_sigma, n_items, sep ="_")
# 
# table( table(unique(settings_used))) # should equal 1
# 
# results_table = params_list
# 
# results_table$settings_used = paste(results_table$sens_mean, 
#                                     results_table$sens_sigma, 
#                                     results_table$k_mean, 
#                                     results_table$k_sigma, 
#                                     results_table$n_items, 
#                                     sep = "_")

## Create results table for simulation results ---------------------------------
results_table = data.frame(i = 1:length(results))

results[[14]]$cor_bayes_estimate_true$conf.int[2]

# Pop Reliability estimates
# results_table$pop_rel         = simulated_reliabilities[match(results_table$settings_used,settings_used)]
# results_table$true_score_cor2 = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
# results_table$true_score_cor2 = ifelse(is.na(results_table$true_score_cor2), 0 , results_table$true_score_cor2)

# Settings

results_table$n_pps           = sapply(results, function(x) x$settings$n_pps) %>% as.numeric()
results_table$sample_sizes           = sapply(results, function(x) x$settings$n_pps) %>% as.numeric()
results_table$n_trials           = sapply(results, function(x) x$settings$n_trials) %>% as.numeric()
results_table$learning_rate_mean           = sapply(results, function(x) x$settings$learning_rate_mean) %>% as.numeric()
results_table$learning_rate_sd           = sapply(results, function(x) x$settings$learning_rate_sd) %>% as.numeric()
results_table$decision_noise_mean           = sapply(results, function(x) x$settings$decision_noise_mean) %>% as.numeric()
results_table$decision_noise_sd           = sapply(results, function(x) x$settings$decision_noise_sd) %>% as.numeric()

# Relative Measurement Precision
results_table$rmp_est         = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb
results_table$rmp_pd          = sapply(results, function(x) x$rmp_pd) %>% as.numeric()

# Other metrics

results_table$cor_bayes_estimate_true = sapply(results, function(x) x$cor_bayes_estimate_true$estimate) %>% as.numeric()
results_table$cor_bayes_estimate_true[is.na(results_table$cor_bayes_estimate_true)] = 0
results_table$cor_bayes_estimate_true_lb = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[1]) %>% as.numeric()
results_table$cor_bayes_estimate_true_lb[is.na(results_table$cor_bayes_estimate_true_lb)] = 0
results_table$cor_bayes_estimate_true_ub = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[2]) %>% as.numeric()
results_table$cor_bayes_estimate_true_ub[is.na(results_table$cor_bayes_estimate_true_ub)] = 0

# Performance Measures ----------------------------------------------------------

# Empty - calculed on the fly below now

# Reminder of different settings used ------------------------------------------

params_list %>%
  filter(run_rep ==1)

# Results Table-----------------------------------------------------------------

results_table %>%
  mutate(
    sample_sizes = factor(sample_sizes),
    sens_sigma  = factor(sens_sigma),
    n_items     = factor(n_items)
  ) %>%
  ggplot(aes( x = true_score_cor2, y = rmp_est, shape = sample_sizes, col = n_items)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~sens_sigma)

# results_table_long = results_table %>%
#   rowid_to_column() %>%
#   pivot_longer(cols = c(rmp_est,rmp_lb,rmp_ub, sh_est, sh_lb, sh_ub), names_to = c("name", ".value"), names_pattern = "(rmp|sh)_(.*)") 

# Analysis of coverage/bias ---------------------------------------------------

results_table %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(pop_rel     = mean(cor_bayes_estimate_true^2)) %>%
  mutate(
    # pop_rel     = mean(cor_bayes_estimate_true^2),
    est        = rmp_est,
    ub         = rmp_ub,
    lb         = rmp_lb,
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    pop_rel_mean    = mean(pop_rel),
    # pop_rel_sd      = sd(pop_rel),
    # avg_cor2        = mean(true_score_cor2),
    mean        = mean(est),
    bias        = mean(difference),
    bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,      mae         = mean(abs(difference)),
    MSE         = mean((difference)^2),
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length)
    # perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    # perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
  )  %>%
  ungroup()

  select(-pop_rel_sd, -coverage_se) %>%
  select(pop_rel_mean, everything()) %>%
  arrange(sens_sigma, n_items, k_sigma, sample_sizes,name) %>%
  gt() %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n, n_items, perc_diag_divergences_binary, perc_diag_ebfmi_binary),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = starts_with("coverage"),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",      # avg_cor2     ~ "{{:rho:_:theta:,x^2}}",
    avg_cor2      ~ "{{mean[:rho:_:theta:,x^2 ]}}",
    pop_rel_mean ~ "{{R_pop}}",
    # sens_mean    ~ "{{:mu:_d'}}",
    sens_sigma   ~ "{{:sigma:_di'}}",
    k_sigma      ~ "{{:sigma:_:kappa:}}",
    n_items      ~ "{{n_trials}}",
    bias           ~ "bias",
    bias_lb  ~ "LB",
    bias_ub  ~ "UB",
    mean_ci_length ~ "Mean Length",
    name         ~ "Estimator",
    perc_diag_divergences_binary ~ "% Divergent Transitions",
    perc_diag_ebfmi_binary ~  "% Low E-BFMI"
    
  )  %>%
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(pop_rel_mean, sens_sigma, n_items, k_sigma, n, sample_sizes,name)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(mean, mae, MSE, contains("bias"))) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    "{{R_pop}} = Simulated Population Reliability. MSE = Mean Squared Error. n = number of simulations. These results are averaged over the different sample sizes"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      rows = which((name == "rmp"))
    )
  ) %>% 
  gt::cols_hide(c(mae,avg_cor2, bias_se, mean))

gtsave(filename = file.path("results","6_results_table_1.html"))



