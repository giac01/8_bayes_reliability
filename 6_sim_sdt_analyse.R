# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results_path = file.path("results","6_sdt_results")

results_files = list.files(results_path, 
                           pattern = "^6_results_",
                           recursive = FALSE,
                           full.names = TRUE
)
results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)


## Create results table for simulation results ---------------------------------
results_table = data.frame(i = 1:length(results))

results_table$sample_sizes    = sapply(results, function(x) x$settings$n_pps) 
results_table$sens_mean       = sapply(results, function(x) x$settings$sens_mean)
results_table$sens_sigma      = sapply(results, function(x) x$settings$sens_sigma) 
results_table$k_mean          = sapply(results, function(x) x$settings$k_mean) 
results_table$k_sigma         = sapply(results, function(x) x$settings$k_sigma) 
results_table$n_items         = sapply(results, function(x) x$settings$n_items) 

results_table$settings_used = paste(results_table$sens_mean, 
                                    results_table$sens_sigma, 
                                    results_table$k_mean, 
                                    results_table$k_sigma, 
                                    results_table$n_items, 
                                    sep = "_")

results_table$settings_used_with_npps = paste(results_table$sens_mean, 
                                    results_table$sens_sigma, 
                                    results_table$k_mean, 
                                    results_table$k_sigma, 
                                    results_table$n_items, 
                                    results_table$sample_sizes,
                                    sep = "_")

# Pop Reliability estimates
results_table$true_score_cor2 = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
results_table$true_score_cor2 = ifelse(is.na(results_table$true_score_cor2), 0 , results_table$true_score_cor2)

# Relative Measurement Precision
results_table$rmp_est         = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb

results_table$rmp_pd          = sapply(results, function(x) x$rmp_pd) %>% as.numeric()

# Split Half 
results_table$sh_est         = sapply(results, function(x) x$splithalf_a$est) %>% as.numeric()
results_table$sh_lb          = sapply(results, function(x) x$splithalf_a$ci.lower) %>% as.numeric()
results_table$sh_ub          = sapply(results, function(x) x$splithalf_a$ci.upper) %>% as.numeric()
results_table$sh_ci_length   = results_table$sh_ub - results_table$sh_lb

results_table$sample_sizes   = factor(results_table$sample_sizes)

#Diagnostics
results_table$diag_divergences        = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi              = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary       = as.numeric(results_table$diag_ebfmi>0)

# Filter out trials with k_sigma ==0 --------------------------------------------

# Performance was good in these conditions, I removed this condition to simply the presentation of the results

results_table = results_table[results_table$k_sigma!=0,]

# Performance Measures ---------------------------------------------------------

# Create leave-one-out measure of population reliability for each setting combo 

# results_table$true_score_cor2_loo = NA
# 
# results_table <- results_table %>%
#   group_by(settings_used_with_npps) %>%
#   mutate(
#     pop_rel_loo = (sum(true_score_cor2) - true_score_cor2) / (n() - 1),
#     pop_rel     =  mean(true_score_cor2)
#          ) %>%
#   ungroup()

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

# qnorm(.025, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# qnorm(.975, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# plogis(-1.0593)

results_table_long = results_table %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est,rmp_lb,rmp_ub, sh_est, sh_lb, sh_ub), names_to = c("name", ".value"), names_pattern = "(rmp|sh)_(.*)") 

# Analysis of coverage/bias ---------------------------------------------------

  results_summarised_over_groups = 
  results_table_long %>%
    group_by(n_items, sens_sigma, k_sigma, name, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
    mutate(
      pop_rel_loo = (sum(true_score_cor2) - true_score_cor2) / (n() - 1),
      pop_rel     =  mean(true_score_cor2),
      difference = est - pop_rel_loo,
      ci_correct = (lb <= pop_rel_loo & ub >= pop_rel_loo),
      ci_length  = ub - lb
    ) %>%
    summarise(
      n = n(),
      pop_rel_mean= mean(true_score_cor2),
      pop_rel_sd  = sd(true_score_cor2),
      mean        = mean(est),
      bias        = mean(difference),
      bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
      bias_lb     = bias - qnorm(0.975)*bias_se,
      bias_ub     = bias + qnorm(0.975)*bias_se,      
      mae         = mean(abs(difference)),
      MSE         = mean((difference)^2),
      coverage    = length(which(ci_correct))/length(ci_correct),
      coverage_se = sqrt((coverage*(1-coverage))/n),
      coverage_lb = coverage - 1.96*coverage_se,
      coverage_ub = coverage + 1.96*coverage_se,
      mean_ci_length = mean(ci_length),
      perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
      perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
    )  %>%
    ungroup()

  results_summarised_over_groups %>%
    filter(name == "rmp") %>%
    select(-pop_rel_sd, -coverage_se) %>%
    select(pop_rel_mean, everything()) %>%
    arrange(sens_sigma, n_items, k_sigma, sample_sizes, name) %>%
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
      # pop_rel      ~ "{{mean[:rho:_:theta:,x^2 ]}}",
      pop_rel_mean ~ "{{R_pop}}",
      # sens_mean    ~ "{{:mu:_d'}}",
      sens_sigma   ~ "{{:sigma:_d'}}",
      k_sigma      ~ "{{:sigma:_:kappa:}}",
      n_items      ~ "{{n_trials}}",
      bias           ~ "bias",
      bias_lb  ~ "LB",
      bias_ub  ~ "UB",
      coverage_lb ~ "LB",
      coverage_ub ~ "UB",
      mean_ci_length ~  md("Mean<br>Length"),
      name         ~ "Estimator",
      perc_diag_divergences_binary ~ md("% Divergent<br>Transitions"),
      perc_diag_ebfmi_binary ~   md("% Low<br>E-BFMI"),
      # .fn = md
      
    )  %>%
    tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
    tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
    
    tab_spanner(label = "Simulation Parameters", columns = c(pop_rel_mean, sens_sigma, n_items, k_sigma, n, sample_sizes,name)) %>%
    tab_spanner(label = "Estimator Performance", columns = c(mean, mae, MSE, contains("bias"))) %>%
    tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
    tab_footnote(
      footnote = md("R<sub>pop</sub> = Simulated Population Reliability. 
                    σ<sub>d'</sub> = standard deviation of true sensitivity values across subjects.
                    n<sub>trials</sub> = number of trials completed per participant.
                    n<sub>sim</sub> = number of simulations completed for this set of simulation parameters. 
                    n<sub>obs</sub> = number of subjects per simulation.
                    MSE = Mean Squared Error.
                    R<sub>pop</sub> is average squared correlation between the posterior mean estimates and the true score estimates across the simulations.
                    Coverage is the proportion of times the 95% credible intervals include the population reliability, which shoud be around 95%.
                    "
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(
        columns = everything(),
        rows = which((sample_sizes==200))
      )
    ) %>%
    tab_options(
      table.width = pct(35)
    ) %>%
  gt::cols_hide(c(mae,bias_se, mean, k_sigma)) 

  gtsave(filename = file.path("results","6_results_table_1.html"))



# Comparison of split half and RMP ---------------------------------------------
    
   # Comparison of CI lengths
    
    results_table_splithalfcomp = results_table %>%
      mutate(length_ratio = rmp_ci_length/sh_ci_length)
    
    results_table$sh_ci_length %>% hist()
    results_table$rmp_ci_length %>% hist()
    
    results_table_splithalfcomp$length_ratio %>% median()    # there are some weird issues with some sh_ci_lengths being negative!!
    
    results_table
    
  # Relative increase in precision 
    
    results_table_precision = results_table %>%
      mutate(
        rmp_diff = rmp_est - pop_rel,
        sh_diff  = sh_est  - pop_rel
             ) %>%
      group_by(n_items, sens_sigma, k_sigma, sample_sizes) %>%
      summarise(
        n_sim        = n(),
        rmp_diff_var = var(rmp_diff),
        sh_diff_var  = var(sh_diff)
                ) %>%
      mutate(
        relative_precision = 100*(rmp_diff_var/sh_diff_var-1)
      )
    
    results_table_precision
    

head(results_table_precision)

# Plots of summary statistics --------------------------------------------------
results_summarised_over_groups %>%
  ggplot(aes( y = bias, ymin = bias_lb, bias = bias_ub)) + 
  geom_errorbar()


# Plots of each sample statistic -----------------------------------------------

results_table_long %>%
  arrange(est) %>%
  filter(name == "rmp") %>%
  filter(k_sigma ==0.2) %>%
  mutate(n_items = factor(n_items)) %>%
  group_by(n_items, sens_sigma, k_sigma, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(i = 1:n()) %>%
  ggplot(aes( x = i, y = est, shape = sample_sizes, col = n_items)) + 
  geom_hline(yintercept = 0) +
  geom_point() + 
  geom_errorbar(aes(ymin = lb, ymax = ub), alpha = .1) +
  geom_hline(aes(yintercept =pop_rel, col = n_items), size = 1.2) +
  coord_cartesian(ylim =c(-.7,1)) +
  theme_bw() +
  facet_grid(~ sample_sizes + sens_sigma)



  results_table_long %>%
    filter(name == "rmp") %>%
    ggplot(aes(x = pop_rel, y = est)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~sample_sizes + n_items)

  results_table_long %>%
    filter(name == "rmp") %>%
    ggplot(aes(x = pop_rel, y = est)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    geom_errorbar(aes(ymin = lb, ymax = ub)) +
    facet_wrap(~sample_sizes + n_items)
  
  results_table_long %>%
    filter(name == "rmp") %>%
    ggplot(aes(x = pop_rel, y = est)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    geom_errorbar(aes(ymin = lb, ymax = ub)) +
    facet_wrap(~sample_sizes + k_sigma)
  
  
  
  
  

  table(results_table_long$settings_used)
  
  results_table_long %>% 
    filter(name == "rmp") %>%
    mutate(
      settings_used_pretty = paste0(
        "mu[beta]^2 == ", round(sens_mean, digits = 2),
        "*','~sigma[theta]^2 == ", round(sens_sigma, digits = 2),
        "*','~mu[kappa]^2 == ", round(k_mean, digits = 2),
        "*','~sigma[kappa]^2 == ", round(k_sigma, digits = 2),
        "*','~N[items] == ", round(n_items, digits = 0)
     )
    ) %>%
    ggplot(aes(x = sample_sizes, y = est)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = lb, ymax = ub)) +
    geom_hline(aes(yintercept = pop_rel), col = "red") +
    facet_wrap(~settings_used_pretty, labeller = label_parsed)
  
  
  
  # results_table$settings_used = paste(results_table$sens_mean, 
  #                                     results_table$sens_sigma, 
  #                                     results_table$k_mean, 
  #                                     results_table$k_sigma, 
  #                                     results_table$n_items, 
  #                                     sep = "_")
  # 
  


  