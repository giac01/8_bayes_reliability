# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results     = readRDS(file = file.path("results","6_results_20240402_065858.rds"))   # Need to re-run this as i accidently squared sigma
results_pop = readRDS(file = file.path("results","6_results_pop_c.rds"))
params_list = readRDS(file = file.path("results","6_params_list_c.rds"))

# Sample size used in "population" simulations. 
results_pop[[1]]$settings$n_pps

## Extract our estimates of the "true" reliabilities ---------------------------

simulated_reliabilities         = sapply(results_pop, function(x) x$cor_bayes_estimate_true$estimate^2) 
simulated_reliabilities         = ifelse(is.na(simulated_reliabilities), 0,  simulated_reliabilities)
sens_mean                       = sapply(results_pop, function(x) x$settings$sens_mean)
sens_sigma                      = sapply(results_pop, function(x) x$settings$sens_sigma) 
k_mean                          = sapply(results_pop, function(x) x$settings$k_mean) 
k_sigma                         = sapply(results_pop, function(x) x$settings$k_sigma) 
n_items                         = sapply(results_pop, function(x) x$settings$n_items) 
n_pps                           = sapply(results_pop, function(x) x$settings$n_pps) 

settings_used = paste(sens_mean, sens_sigma, k_mean, k_sigma, n_items, sep ="_")

table( table(unique(settings_used))) # should equal 1
  
results_table = params_list

results_table$settings_used = paste(results_table$sens_mean, 
                                    results_table$sens_sigma, 
                                    results_table$k_mean, 
                                    results_table$k_sigma, 
                                    results_table$n_items, 
                                    sep = "_")

### Check results match with params_list ---------------------------------------
results[[1]]$settings$n_pps
check_sens_mean    = sapply(results, function(x) x$settings$sens_mean)
check_sens_sigma   = sapply(results, function(x) x$settings$sens_sigma)
check_k_mean       = sapply(results, function(x) x$settings$k_mean)
check_k_sigma      = sapply(results, function(x) x$settings$k_sigma)
check_n_items      = sapply(results, function(x) x$settings$n_items)
check_sample_sizes = sapply(results, function(x) x$settings$n_pps)

if (!identical(check_sens_mean,   params_list$sens_mean))   {stop("Check parmas_list matches with results")}
if (!identical(check_sens_sigma,  params_list$sens_sigma))  {stop("Check parmas_list matches with results")}
if (!identical(check_k_mean,      params_list$k_mean))      {stop("Check parmas_list matches with results")}
if (!identical(check_k_sigma,     params_list$k_sigma))     {stop("Check parmas_list matches with results")}
if (!identical(check_n_items,     params_list$n_items))     {stop("Check parmas_list matches with results")}
if (!identical(check_sample_sizes,params_list$sample_sizes)){stop("Check parmas_list matches with results")}

## Create results table for simulation results ---------------------------------

# Pop Reliability estimates
results_table$pop_rel = simulated_reliabilities[match(results_table$settings_used,settings_used)]
results_table$true_score_cor2 = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
results_table$true_score_cor2 = ifelse(is.na(results_table$true_score_cor2), 0 , results_table$true_score_cor2)

# Relative Measurement Precision
results_table$rmp_est         = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb

# Split Half 
results_table$sh_est         = sapply(results, function(x) x$splithalf_a$est) %>% as.numeric()
results_table$sh_lb          = sapply(results, function(x) x$splithalf_a$ci.lower) %>% as.numeric()
results_table$sh_ub          = sapply(results, function(x) x$splithalf_a$ci.upper) %>% as.numeric()
results_table$sh_ci_length   = results_table$sh_ub - results_table$sh_lb

results_table$sample_sizes = factor(results_table$sample_sizes)

#Diagnostics
results_table$diag_divergences        = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi              = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary       = as.numeric(results_table$diag_ebfmi>0)

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

# qnorm(.025, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# qnorm(.975, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# plogis(-1.0593)

results_table_long = results_table %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est,rmp_lb,rmp_ub, sh_est, sh_lb, sh_ub), names_to = c("name", ".value"), names_pattern = "(rmp|sh)_(.*)") 

## Analysis of coverage/bias ---------------------------------------------------

  results_table_long %>%
    group_by(n_items, sens_sigma, k_sigma, name) %>%       # aggregating over sample-sizes & sens_mean
    mutate(
      difference = est - pop_rel,
      ci_correct = (lb < pop_rel & ub > pop_rel),
      ci_length  = ub - lb
    ) %>%
    summarise(
      n = n(),
      pop_rel_mean    = mean(pop_rel),
      pop_rel_sd      = sd(pop_rel),
      avg_cor2        = mean(true_score_cor2),
      mean        = mean(est),
      md          = mean(difference),
      mae         = mean(abs(difference)),
      mse         = mean((difference)^2),
      coverage    = length(which(ci_correct))/length(ci_correct),
      coverage_se = sqrt((coverage*(1-coverage))/n),
      coverage_lb = coverage - 1.96*coverage_se,
      coverage_ub = coverage + 1.96*coverage_se,
      mean_ci_length = mean(ci_length),
      sum_div     = sum(diag_divergences_binary),
      sum_ebfmi   = sum(diag_ebfmi_binary)
      
    )  %>%
    ungroup() %>%
    select(-pop_rel_sd, -coverage_se) %>%
    select(pop_rel_mean, everything()) %>%
    gt() %>%
    fmt(
      columns = where(is.numeric),
      fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
    ) %>%
    fmt_number(
      columns = c(n, n_items, sum_div, sum_ebfmi),
      decimals = 0
    ) %>%
    fmt_percent(
      columns = starts_with("coverage"),
      decimals = 1
    ) %>%
    cols_label(
      # sample_sizes = "Sample Size",
      # avg_cor2     ~ "{{:rho:_:theta:,x^2}}",
      avg_cor2      ~ "$$\\overline{\\rho(\\theta, x)^2}$$",
      pop_rel_mean ~ "{{R_pop}}",
      sens_sigma   ~ "{{:sigma:_sens}}",
      n_items      ~ "# Trials",
      md           ~ "bias",
      mean_ci_length ~ "Mean Length"
      
    )  %>%
    tab_spanner(label = "Simulation Parameters", columns = c(pop_rel_mean, sens_sigma, n_items, n)) %>%
    tab_spanner(label = "Estimator Performance", columns = c(mean, mae, md)) %>%
    tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
    tab_footnote(
      "{{R_pop}} = Simulated Population Reliability. mae = Mean Absolute Error. n = number of simulations. These results are averaged over the different sample sizes"
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(
        columns = everything(),
        rows = which((n_items ==10 | n_items == 40))
      )
    )

  gtsave(filename = file.path("results","6_results_table_1.html"))
  
  
  results_table_long %>%
    group_by(sample_sizes, n_items, sens_sigma, k_sigma, name, sens_mean) %>%
    mutate(
      difference = est - pop_rel,
      ci_correct = (lb < pop_rel & ub > pop_rel),
      ci_length  = ub - lb
    ) %>%
    summarise(
      n = n(),
      pop_rel_mean         = mean(pop_rel),
      pop_rel_sd      = sd(pop_rel),
      mean     = mean(est),
      md       = mean(difference),
      mae      = mean(abs(difference)),
      coverage = length(which(ci_correct))/length(ci_correct),
      coverage_se = sqrt((coverage*(1-coverage))/n),
      coverage_lb = coverage - 1.96*coverage_se,
      coverage_ub = coverage + 1.96*coverage_se,
      mean_ci_length = mean(ci_length)
    )  %>%
    ungroup() %>%
    select(-pop_rel_sd, -coverage_se) %>%
    select(pop_rel_mean, everything()) %>%
    gt() %>%
    fmt(
      columns = where(is.numeric),
      fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
    ) %>%
    fmt_number(
      columns = c(n, n_items),
      decimals = 0
    ) %>%
    fmt_percent(
      columns = starts_with("coverage"),
      decimals = 1
    ) %>%
    cols_label(
      sample_sizes = "Sample Size",
      pop_rel_mean ~ "{{R_pop}}",
      # sens_sigma   ~ "{{:sigma:_sens}}",
      n_items      ~ "# Trials",
      md           ~ "bias",
      mean_ci_length ~ "Mean Length"
    )  %>%
    tab_spanner(label = "Simulation Parameters", columns = c(pop_rel_mean, n_items, n, sample_sizes)) %>%
    tab_spanner(label = "Estimator Performance", columns = c(mean, mae, md)) %>%
    tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
    tab_footnote(
      md("{{R_pop}} = Simulated Population Reliability. mae = Mean Absolute Error. n = number of simulations. These results are averaged over the different sample sizes"
    )) %>%
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(
        columns = everything(),
        rows = which((sample_sizes ==100 | sample_sizes == 1000))
      )
    ) %>%
    gtsave(filename = file.path("results","6_results_table_2.html"))
  

# Plots ------------------------------------------------------------------------
  
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
  


  