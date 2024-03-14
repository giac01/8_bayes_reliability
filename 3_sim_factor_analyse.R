library(tidyverse)
rm(list = ls(all.names = TRUE))

load(file = file.path("results","results_b.Rds"))
params_list = readRDS(file = file.path("results","3_params_list.rds"))


results_table = params_list

results_table$pop_rel = sapply(results, function(x) x$reliability) %>% as.numeric()
results_table$rel_est = sapply(results, function(x) x$r_est[1])%>% as.numeric()
results_table$rel_lb = sapply(results, function(x) x$r_est[2]) %>% as.numeric()
results_table$rel_ub = sapply(results, function(x) x$r_est[3]) %>% as.numeric()
results_table$rel_ci_length =  results_table$rel_ub - results_table$rel_lb
results_table$true_score_cor2 = sapply(results, function(x) x$true_score_cor) %>% as.numeric()

results_table$ci_contain = (results_table$pop_rel > results_table$rel_lb) & (results_table$pop_rel < results_table$rel_ub) 

# results_table$true_score_cor2 = as.numeric(results_table$true_score_cor)


results_table$sample_sizes = factor(results_table$sample_sizes)


results_table$diag_divergences = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()


# Performance Measuers
results_table$rel_diff =  results_table$rel_est - results_table$pop_rel 




results_table %>%
  ggplot(aes(x = pop_rel, y = rel_est, col = sample_sizes, shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = rel_lb, ymax = rel_ub), width = .01) + 
  theme_bw() + 
  coord_cartesian(xlim=0:1, ylim = 0:1) + 
  facet_wrap(~tau_equivalence)


results_table %>%
  ggplot(aes(x = pop_rel, y = (true_score_cor2), col = sample_sizes, shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  theme_bw() + 
  coord_cartesian(xlim=0:1, ylim = 0:1) + 
  facet_wrap(~tau_equivalence)


# Coverage analysis

results_table %>%
  group_by(sample_sizes) %>%
  summarise(coverage = length(which(ci_contain))/length(ci_contain),
            mean_ci_length = mean(rel_ci_length),
            n = n())

results_table %>%
  group_by(n_items) %>%
  summarise(coverage = length(which(ci_contain))/length(ci_contain),
            mean_ci_length = mean(rel_ci_length),
            n = n())

results_table$pop_rel %>% hist()

library(gt)
results_table %>%
  gt() %>%
  fmt_number()
  


results_table %>%
  rowid_to_column(var = "i") %>%
  arrange(desc(abs(rel_diff))) %>%
  gt() %>%
  fmt_number()


inspect = results[[14]]

inspect$regression_factor_score=NULL


results[[1]]$
