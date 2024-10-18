# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)
rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

results_path = file.path("results","5_factor_tauequivalent_results")

results_files = list.files(results_path, 
                           pattern = "^5_results_tauequiv_",
                           recursive = FALSE,
                           full.names = TRUE
)
results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)

# params_list = readRDS(file = file.path("results","5_params_list_a.rds"))

## Create results table for simulation results ---------------------------------

results_table = data.frame(i = 1:length(results))

results_table$n    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )
results_table$sample_sizes    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )

results_table$n_items         = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n_items )


results_table$pop_rel         = sapply(results, function(x) x$population_reliability) %>% as.numeric()
results_table$pop_ss_loading  = sapply(results, function(x) sum(x$settings$loadings^2)) %>% as.numeric()
results_table$sec_min_loading = sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[2]) %>% as.numeric()
results_table$third_min_loading =  sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[3]) %>% as.numeric()

results_table$rmp_est         = sapply(results, function(x) x$rmp_est[1] )%>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp_est[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp_est[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb

results_table$a_est           = sapply(results, function(x) x$alpha_reliability$est)%>% as.numeric()
results_table$a_lb            = sapply(results, function(x) x$alpha_reliability$ci.lower)%>% as.numeric()
results_table$a_ub            = sapply(results, function(x) x$alpha_reliability$ci.upper)%>% as.numeric()

# Other coverage statistics

results_table$true_score_cor2              = sapply(results, function(x) x$true_score_cor$estimate^2) %>% as.numeric()
# results_table$true_score_coverage          = sapply(results, function(x) x$true_score_coverage)%>% as.numeric()
results_table$true_score_factor_score_cor2 = sapply(results, function(x) x$true_score_factor_score_cor$estimate^2)%>% as.numeric()   # This is the NON-SQUARED CORRELATION CURRENTLY - SHOULD CHANGE!
results_table$true_score_model_score_cor2  = sapply(results, function(x) x$true_score_model_score_cor$estimate^2)%>% as.numeric()

results_table$sample_sizes = factor(results_table$sample_sizes)
results_table$diag_divergences = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary = as.numeric(results_table$diag_ebfmi>0)

# Performance Measuers
# results_table$rel_diff =  results_table$rel_est - results_table$pop_rel 

# Results Table -----------------------------------------------------------------
results_table_long = results_table %>%
  # select(-any_of(c(starts_with("diag")))) %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est, a_est, rmp_lb, a_lb, rmp_ub, a_ub), names_to = c("name", ".value"), names_pattern = "(rmp|a)_(.*)") 

## Comparison between RMP and H ------------------------------------------------

results_table_cleaned =  results_table_long %>%
  group_by(n_items, sample_sizes, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    pop_rel     = mean(pop_rel),
    pop_rel_sd  = sd(pop_rel, na.rm = TRUE),
    n           = n(),
    mean        = mean(est),
    mean_se     = sd(est)/sqrt(n),
    mean_lb     = mean - qnorm(0.975)*mean_se,
    mean_ub     = mean + qnorm(0.975)*mean_se,
    bias        = mean(difference),
    bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,
    mad         = mean(abs(difference)),
    MSE         = mean((difference)^2),
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    mean_ci_length = mean(ci_length),
    # `Mean True Score Coverage` = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
  ) %>%
  ungroup() 

results_table_cleaned[which(results_table_cleaned$name=="a"),c("perc_diag_divergences_binary","perc_diag_ebfmi_binary")] = NA
results_table_cleaned$name[results_table_cleaned$name=="a"] = "Alpha"
results_table_cleaned$name[results_table_cleaned$name=="rmp"] = "RMP"

results_table_cleaned %>%
  select(-any_of(c("coverage_se", "loading_set", "pop_rel_sd", 
         "loadings_list_pretty2", "mad",
         "mean", "mean_se", "mean_ub", "mean_lb",
         "bias_se"))
  ) %>%
  # filter(name == "rmp") %>%
  gt() %>%
  gt::cols_move_to_start(name) %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n, n_items),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"),contains("perc_diag")),
    decimals = 1
  ) %>%
  cols_label(
    pop_rel      ~ "{{R_pop}}",
    n_items      ~ "{{n_items}}",
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",
    bias           ~ "bias",
    mean_ci_length ~ "Mean Length",
    coverage_lb  ~ "LB",
    coverage_ub  ~ "UB",
    bias_lb  ~ "LB",
    bias_ub  ~ "UB",
    perc_diag_divergences_binary ~ "% Divergent Transitions",
    perc_diag_ebfmi_binary ~  "% Low E-BFMI"
  )  %>%
  
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(n_items,sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c( bias, bias_lb, bias_ub, MSE)) %>%
  tab_spanner(label = "Confidence/Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    footnote = md(" n<sub>items</sub> = number of items completed per participant.
                    n<sub>sim</sub> = number of simulations completed for this set of simulation parameters. 
                    n<sub>obs</sub> = number of subjects per simulation.
                    MSE = Mean Squared Error.
                    Mean Length = Mean Length of credible or confidence interval. 
                    The percentage of simulations where the MCMC algorithm had at least one divergent transition or E-BFMI value of less than .20 are shown.
                    "
    )) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      # rows = which((sample_sizes ==200 | sample_sizes == 2000))
      rows = which(name == "RMP")
    )
  ) %>%
  tab_options(
    table.width = pct(35)
  ) %>%
  gt::cols_hide(c(pop_rel)) 
  
  
  gtsave(filename = file.path("results","5_table_A.html"))
  
  
  
# What factors predict divergent transitions? ----------------------------------
  
  ggplot(results_table, aes(x = pop_rel, y = diag_divergences_binary)) + 
    geom_jitter() +
    facet_wrap(~sample_sizes)
  
  
  
  