# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results_path = file.path("results","7_ri_results")

results_files = list.files(results_path, 
                           pattern = "^7_results_ri_seed3",
                           recursive = FALSE,
                           full.names = TRUE
)

results = lapply(results_files[!grepl("pop",results_files)], function(x) readRDS(x))

results = do.call("c", results)


# results_pop = readRDS(file = results_files[grepl("pop",results_files)])

# Sample size used in "population" simulations. 
# results_pop[[1]]$settings$n_pps

# Extract Population Reliability simulations -----------------------------------

# results_pop[[1]]$settings$n_pps

# simulated_reliabilities     = sapply(results_pop, function(x) x$cor_bayes_estimate_true$estimate^2)
# simulated_reliabilities     = ifelse(is.na(simulated_reliabilities), 0,  simulated_reliabilities)
# sample_sizes                = sapply(results_pop, function(x) x$settings$n_pps)
# n_trials                    = sapply(results_pop, function(x) x$settings$n_trials)
# learning_rate_mean          = sapply(results_pop, function(x) x$settings$learning_rate_mean)
# learning_rate_sd            = sapply(results_pop, function(x) x$settings$learning_rate_sd)
# decision_noise_mean         = sapply(results_pop, function(x) x$settings$decision_noise_mean)
# decision_noise_sd           = sapply(results_pop, function(x) x$settings$decision_noise_sd)
# 
# settings_used = paste(n_trials,learning_rate_mean, learning_rate_sd, decision_noise_mean,decision_noise_sd, sep = "_")

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

# Other metrics

results_table$cor_bayes_estimate_true = sapply(results, function(x) x$cor_bayes_estimate_true$estimate) %>% as.numeric()
results_table$cor_bayes_estimate_true[is.na(results_table$cor_bayes_estimate_true)] = 0
results_table$cor_bayes_estimate_true_lb = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[1]) %>% as.numeric()
results_table$cor_bayes_estimate_true_lb[is.na(results_table$cor_bayes_estimate_true_lb)] = 0
results_table$cor_bayes_estimate_true_ub = sapply(results, function(x) x$cor_bayes_estimate_true$conf.int[2]) %>% as.numeric()
results_table$cor_bayes_estimate_true_ub[is.na(results_table$cor_bayes_estimate_true_ub)] = 0

# Diagnostics 
results_table$max_iter = sapply(results, function(x) x$diagnostics$max_iter) %>% as.numeric()

results[[1]]$diagnostics$max_iter

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

## calculate LOO cross-validated true_score_cor  -----------------------------------
results_table$true_score_cor2_loo = NA

for (i in 1:nrow(results_table)){
  results_table$true_score_cor2_loo[i] = results_table[-i,] %>%
    dplyr::filter(settings_used_with_npps==results_table[-i,"settings_used_with_npps"]) %>% 
    pull(true_score_cor2) %>%
    mean()
}


# Analysis of coverage/bias ---------------------------------------------------

results_table %>%
  group_by(learning_rate_sd, n_trials, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(pop_rel = mean(true_score_cor2_loo)) %>% 
  mutate(mean_rmp_est = mean(rmp_est)) %>% 
  mutate(
    est        = rmp_est,
    ub         = rmp_ub,
    lb         = rmp_lb,
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb,
    ci_mean_correct = (lb <= true_score_cor2_loo & ub >= true_score_cor2_loo),
    ci_bias_elimated_correct = (lb <= mean_rmp_est & ub >= mean_rmp_est),
    mean_rmp_est = mean(rmp_est)
  ) %>%
  summarise(
    n = n(),
    pop_rel     = mean(pop_rel),
    pop_rel_sd      = sd(pop_rel),
    avg_cor2        = mean(true_score_cor2),
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
    coverage_true_score_cor = length(which(ci_mean_correct))/length(ci_mean_correct),
    coverage_bias_eliminated = length(which(ci_bias_elimated_correct))/length(ci_bias_elimated_correct),
    mean_ci_length = mean(ci_length),
    mean_ts_coverage = mean(avg_true_score_coverage)
  )  %>%
  ungroup() %>% 
  # knitr::kable(digits =2)
  select(-pop_rel_sd, -coverage_se) %>%
  select(pop_rel,avg_cor2,sample_sizes,learning_rate_sd, everything()) %>%
  gt() %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n, n_trials),
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
    pop_rel ~ "{{R_pop}}",
    n_trials      ~ "{{n_trials}}",
    bias           ~ "bias",
    bias_lb  ~ "LB",
    bias_ub  ~ "UB",
    coverage_lb  ~ "LB",
    coverage_ub  ~ "UB",
    mean_ci_length ~ "Mean Length"
  )  %>%
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(pop_rel,avg_cor2,sample_sizes,learning_rate_sd)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(mean, mae, MSE, contains("bias"))) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    "{{R_pop}} = Simulated Population Reliability. MSE = Mean Squared Error. n = number of simulations. These results are averaged over the different sample sizes"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      rows = which((learning_rate_sd == .200))
    )
  ) %>%
  gt::cols_hide(c(mae, bias_se, mean))


#gtsave(filename = file.path("results","7_results_table_1.html"))

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
  ggplot(aes( x = i, y = rmp_est, shape = sample_sizes, col = sample_sizes)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = rmp_lb, ymax = rmp_ub)) +
  geom_hline(aes(yintercept =pop_rel)) +
  coord_cartesian(ylim =c(0,1)) +
  facet_wrap(~ learning_rate_sd+ n_trials)




test = 
results_table %>%
  filter(sample_sizes==200) %>%
  filter(n_trials ==50) %>%
  filter(learning_rate_sd==0) 

table((test$rmp_lb<=0) & (test$rmp_ub>=0))
