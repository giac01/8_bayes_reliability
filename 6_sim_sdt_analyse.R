# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results     = readRDS(file = file.path("results","6_results_a.rds"))
results_pop = readRDS(file = file.path("results","6_results_pop_vi.rds"))
params_list = readRDS(file = file.path("results","6_params_list_a.rds"))

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

## Create results table for simulation results ---------------------------------

results_table$pop_rel = simulated_reliabilities[match(results_table$settings_used,settings_used)]

results_table$true_score_cor2 = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()

results_table$true_score_cor2 = ifelse(is.na(results_table$true_score_cor2), 0 , results_table$true_score_cor2)

results_table$mrp_est         = sapply(results, function(x) x$rmp[1]) %>% as.numeric()
results_table$mrp_lb          = sapply(results, function(x) x$rmp[2]) %>% as.numeric()
results_table$mrp_ub          = sapply(results, function(x) x$rmp[3]) %>% as.numeric()
results_table$mrp_ci_length   = results_table$mrp_ub - results_table$mrp_lb

results_table$sh_est         = sapply(results, function(x) x$splithalf_a$est) %>% as.numeric()
results_table$ss_lb          = sapply(results, function(x) x$splithalf_a$ci.lower) %>% as.numeric()
results_table$ss_ub          = sapply(results, function(x) x$splithalf_a$ci.upper) %>% as.numeric()
results_table$ss_ci_length   = results_table$mrp_ub - results_table$mrp_lb

results_table$sample_sizes = factor(results_table$sample_sizes)

# results_table$diag_divergences = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
# results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
# results_table$diag_ebfmi = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
# results_table$diag_ebfmi_binary = as.numeric(results_table$diag_ebfmi>0)

# Performance Measuers
# results_table$rel_diff =  results_table$rel_est - results_table$pop_rel 

# Results Table-----------------------------------------------------------------

results_table %>%
  mutate(
    sample_sizes = factor(sample_sizes),
    sens_sigma  = factor(sens_sigma),
    n_items     = factor(n_items)
  ) %>%
  ggplot(aes( x = true_score_cor2, y = mrp_est, shape = sample_sizes, col = n_items)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~sens_sigma)

# qnorm(.025, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# qnorm(.975, mean = -1.0593, sd =  sqrt(1.7528 + 0.2484)) %>% plogis()
# plogis(-1.0593)

results_table_long = results_table %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(mrp_est,mrp_lb,mrp_ub, ), names_to = c("name", ".value"), names_pattern = "(mrp|a)_(.*)") 

## Analysis of coverage/bias ---------------------------------------------------

  results_table_long %>%
    group_by(n_items, sens_sigma) %>%
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
      mad      = mean(abs(difference)),
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
      # sample_sizes = "Sample Size",
      pop_rel_mean ~ "{{R_pop}}",
      sens_sigma   ~ "{{:sigma:_sens}}",
      n_items      ~ "# Trials",
      md           ~ "bias",
      mean_ci_length ~ "Mean Length"
      
    )  %>%
    tab_spanner(label = "Simulation Parameters", columns = c(pop_rel_mean, sens_sigma, n_items, n)) %>%
    tab_spanner(label = "Estimator Performance", columns = c(mean, mad, md)) %>%
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
    ) %>%

  gtsave(filename = file.path("results","6_results_table_1.html"))
  
  
  results_table_long %>%
    group_by(sample_sizes, n_items, sens_sigma, sens_mean) %>%
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
      mad      = mean(abs(difference)),
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
    tab_spanner(label = "Estimator Performance", columns = c(mean, mad, md)) %>%
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
    )
  
  gtsave(filename = file.path("results","6_results_table_1.html"))
  

# Plots ------------------------------------------------------------------------
  
  results_table_long %>% 
    ggplot(aes(x = pop_rel, y = est)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~sample_sizes + n_items)
  
  results_table_long %>% 
    ggplot(aes(x = pop_rel, y = est)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) +
    geom_errorbar(aes(ymin = lb, ymax = ub)) +
    facet_wrap(~sample_sizes + n_items)
  
  table(results_table_long$settings_used)
  results_table_long %>% 
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
  
  
  
  results_table$settings_used = paste(results_table$sens_mean, 
                                      results_table$sens_sigma, 
                                      results_table$k_mean, 
                                      results_table$k_sigma, 
                                      results_table$n_items, 
                                      sep = "_")
  
  


  