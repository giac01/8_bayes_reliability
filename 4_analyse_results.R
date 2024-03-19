# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

loadings_list = list(
  c( 0, 0, 0, 0, 0, 0),
  c(.3,.2,.1),
  c(.3,.2,.1,.1,.1,.1),
  c(.4,.3,.3,.2,.1,.0),
  c(.6,.5,.3,.1,.1,.1),
  c(.7,.6,.6,.5,.4),
  c(.7,.6,.5,.5,.4,.4,.4,.3,.3)
)

results     = readRDS(file = file.path("results","4_results_tauinequiv_aa.rds"))
params_list = readRDS(file = file.path("results","4_params_list_aa.rds"))

params_list %>%
  filter(run_rep ==1)

## Create results table for simulation results ---------------------------------

results_table = params_list

results_table$n_items         = sapply(1:nrow(results_table), function(i) length(loadings_list[[results_table$loading_set[i]]]) )

results_table$pop_rel         = sapply(results, function(x) x$population_reliability) %>% as.numeric()
results_table$pop_ss_loading  =  sapply(results, function(x) sum(x$settings$loadings^2)) %>% as.numeric()
results_table$sec_min_loading =  sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[2]) %>% as.numeric()
results_table$third_min_loading =  sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[3]) %>% as.numeric()

results_table$rmp_est         = sapply(results, function(x) x$r_est[1] )%>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$r_est[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$r_est[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb
# results_table$rmp_ci_contain  = (results_table$pop_rel > results_table$rmp_lb) & (results_table$pop_rel < results_table$rmp_ub) 

results_table$h_est           = sapply(results, function(x) x$h_reliability$r)%>% as.numeric()
results_table$h_lb            = sapply(results, function(x) x$h_reliability$ci[1])%>% as.numeric()
results_table$h_ub            = sapply(results, function(x) x$h_reliability$ci[2])%>% as.numeric()
# results_table$h_ci_contain    = (results_table$pop_rel > results_table$h_lb) & (results_table$pop_rel < results_table$h_ub) 

# results[[10]]$true_score_model_score_cor$estimate^2

# Other coverage statistics

results_table$true_score_cor2              = sapply(results, function(x) x$true_score_cor$estimate^2) %>% as.numeric()
results_table$true_score_coverage          = sapply(results, function(x) x$true_score_coverage)%>% as.numeric()
results_table$true_score_factor_score_cor2 = sapply(results, function(x) x$true_score_factor_score_cor$estimate^2)%>% as.numeric()   # This is the NON-SQUARED CORRELATION CURRENTLY - SHOULD CHANGE!
results_table$true_score_model_score_cor2  = sapply(results, function(x) x$true_score_model_score_cor$estimate^2)%>% as.numeric()

results_table$sample_sizes = factor(results_table$sample_sizes)
results_table$diag_divergences = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary = as.numeric(results_table$diag_ebfmi>0)

# Performance Measuers
# results_table$rel_diff =  results_table$rel_est - results_table$pop_rel 

# Results Table-----------------------------------------------------------------
results_table_long = results_table %>%
  select(-any_of(c(starts_with("diag")))) %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est, h_est, rmp_lb, h_lb, rmp_ub, h_ub), names_to = c("name", ".value"), names_pattern = "(rmp|h)_(.*)") 

## Comparison between RMP and H ------------------------------------------------

results_table_long %>%
  group_by( loading_set, sample_sizes, n_items, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    pop_rel  = mean(pop_rel),
    pop_rel_sd = sd(pop_rel, na.rm = TRUE),
    n = n(),
    mean     = mean(est),
    md       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length),
    `Mean True Score Coverage` = mean(true_score_coverage)
  ) %>%
  ungroup() %>%
  # arrange(desc(coverage)) %>%
  select(-coverage_se) %>%
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
    columns = starts_with("coverage"),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "Sample Size",
    # constained_loadings ~ "Loadings Constrained",
    # pop_rel_mean ~ "{{R_pop}}",
    # sens_sigma   ~ "{{:sigma:_sens}}",
    n_items      ~ "# Items",
    md           ~ "bias",
    mean_ci_length ~ "Mean Length"
    
  )  %>%
  tab_spanner(label = "Simulation Parameters", columns = c(sample_sizes, n_items, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(mean, mad, md)) %>%
  tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    "{{R_pop}} = Simulated Population Reliability. mae = Mean Absolute Error. n = number of simulations. These results are averaged over the different sample sizes"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = everything(),
      # rows = which((n_items ==10 | n_items == 40))
      rows = which(name == "rmp")
    )
  ) 

### Smaller table with fewer comparisons ---------------------------------------

results_table_long %>%
  group_by( sample_sizes, n_items, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    mean     = mean(est),
    md       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length),
    `Mean True Score Coverage` = mean(true_score_coverage)
  ) %>%
  ungroup() %>%
  # arrange(desc(coverage)) %>%
  filter(name !="h") %>%
  select(-coverage_se, -name) %>%
  gt() %>%
  # gt::cols_move_to_start(name) %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = starts_with("n"),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = starts_with("coverage"),
    decimals = 1
  ) %>%
  cols_label(
    sample_sizes ~ "Sample Size",
    # constained_loadings ~ "Loadings Constrained",
    # pop_rel_mean ~ "{{R_pop}}",
    # sens_sigma   ~ "{{:sigma:_sens}}",
    n_items      ~ "# Items",
    md           ~ "bias",
    mean_ci_length ~ "Mean Length"
    
  )  %>%
  tab_spanner(label = "Simulation Parameters", columns = c(sample_sizes, n_items, n)) %>%
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
  ) 

## Plots -----------------------------------------------------------------------

### Coverage -------------------------------------------------------------------

results_table_long %>%
  group_by( sample_sizes, constained_loadings, n_items, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    mean     = mean(est),
    md       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length)
  ) %>%
  ungroup() %>%
  mutate(n_items = factor(n_items)) %>%
  ggplot(aes(y = coverage, ymin = coverage_lb, ymax = coverage_ub, x = n_items, col = name)) +
  geom_errorbar(size = 2) +  
  geom_hline(yintercept = 0.95) +
  jtools::theme_apa() +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(x = "Number of Items Per Simulation", y = "Credible Interval Coverage") +
  coord_cartesian(ylim = c(.5,1)) +
  facet_wrap(~constained_loadings + sample_sizes) 

## Do models without model fit issues fit beter ?? -----------------------------

# Does population coefficient H describe how well the observed factor scores and true scores correlate? -------------------

## Correlation between cor(observed factor scores )

results_table %>%
  ggplot(aes(x = true_score_cor2, y = pop_rel)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) + 
  labs(x = "cor(factor scores calculated with pop weights, true scores)^2", 
       y = "pop reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0,1), xlim=c(0,1))

## Correlation between stan (with TRUE weights) and true scores?

results_table %>%
  ggplot(aes(x = true_score_model_score_cor2^2, y = pop_rel)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "pop reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0,1), xlim=c(0,1))
ggsave(file.path("plots","4_stan_true_scot_plot.png"), width = 10, height = 6)

results_table %>%
  ggplot(aes(x = true_score_factor_score_cor2^2, y = pop_rel)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from psych::fa, true scores)^2", 
       y = "pop reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0,1), xlim=c(0,1))


## Correlation between sca and RMP?

results_table %>%
  filter(sample_sizes %in% c(200,500) & n_items == 4) %>%
  ggplot(aes(x = true_score_model_score_cor2^2, y =  rmp_est)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "Estimated reliability (RMP)") +
  geom_smooth(method = "lm") +
  coord_fixed(ylim = c(0,1), xlim=c(0,1))
# 
# lm(rmp_est ~ I(true_score_model_score_cor2^2), data=results_table) %>% summary()
# lm(pop_rel ~ I(true_score_model_score_cor2^2), data=results_table) %>% summary()
# lm(rmp_est ~ I(true_score_model_score_cor2^2), data=results_table) %>% confint()

results_table %>%
  filter(sample_sizes %in% c(200,500)  & n_items == 4) %>%
  ggplot(aes(x = true_score_model_score_cor2^2, y =  pop_rel)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "Population reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_fixed(ylim = c(0,1), xlim=c(0,1))



results_table %>%
  # filter(sample_sizes %in% c(200,500)  & n_items == 4) %>%
  ggplot(aes(x = pop_rel, y =  rmp_est)) + 
  geom_point() +
  geom_errorbar(aes(ymin = rmp_lb, ymax = rmp_ub)) +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "Relative Measurement Precision", 
       y = "Population reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_fixed(ylim = c(0,1), xlim=c(0,1))




## Correlation between observed factor scores (with SAMPLE weights) and true scores? 






# old --------------------------------------------------------------------------

## Does population reliability influence results -------------------------------

results_table_long %>%
  mutate(
    pop_rel_dec = cut(pop_rel, breaks = c(0,.2,.4,.6,.8, 1))
  ) %>% 
  group_by( pop_rel_dec, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    md       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length)
  )

## Does third minimum loading? influence results -------------------------------

results_table_long %>%
  filter(sample_sizes != 100) %>%
  mutate(
    third_min_loading = cut(third_min_loading, breaks = c(0,.2,.4,.6,.8, 1))
  ) %>% 
  group_by(third_min_loading, n_items, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    md       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length)
  )

## Does nunmber of items influence results -------------------------------

results_table_long %>%
  filter(sample_sizes != 100) %>%
  filter(name != "h") %>%
  filter(third_min_loading >.3) %>%
  group_by(n_items, name) %>%
  mutate(
    difference = est - pop_rel,
    ci_correct = (lb < pop_rel & ub > pop_rel),
    ci_length  = ub - lb
  ) %>%
  summarise(
    n = n(),
    mad      = mean(abs(difference)),
    md       = mean(difference),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - 1.96*coverage_se,
    coverage_ub = coverage + 1.96*coverage_se,
    mean_ci_length = mean(ci_length)
  )


## What factors predict divergences? ------------------------------------------

results_table %>% head()

results_table %>%
  group_by( sample_sizes, n_items, tau_equivalence) %>%
  summarise(
    sum_divergences = sum(diag_divergences_binary),
    n               = n(),
    perc_divergences = sum_divergences/n,
    sum_ebfmi = sum(diag_ebfmi_binary),
    perc_ebfmi = sum_ebfmi/n
  ) %>%
  arrange(desc(perc_ebfmi))

model =
results_table %>%
 glm(diag_ebfmi_binary ~ pop_ss_loading + n_items + sample_sizes, data = ., family = "binomial")

model %>% summary()

results_table %>%
  mutate(
    pop_ss_loading_decile = 
  ) %>%
  ggplot(aes(x = pop_ss_loading, y = ))

results_table %>%
  mutate(
    diag_divergences_binary = as.numeric(diag_divergences > 0),
    diag_ebfmi_binary = as.numeric(diag_ebfmi > 0)
  ) %>%
cor.test(~ diag_divergences_binary + pop_ss_loading, data = .)

results_table %>%
  mutate(
    diag_divergences_binary = as.numeric(diag_divergences > 0),
    diag_ebfmi_binary = as.numeric(diag_ebfmi > 0)
  ) %>%
  cor.test(~ diag_ebfmi_binary + pop_ss_loading, data = .)






results_table %>%
  # filter(pop_rel>.3 & pop_rel <.8) %>%
  filter(diag_ebfmi==0) %>%
  group_by(sample_sizes, tau_equivalence) %>%
  summarise(
    mad      = mean(abs(rmp_diff)),
    md       = mean((rmp_diff)),
    coverage = length(which(ci_contain))/length(ci_contain),
    mean_ci_length = mean(rel_ci_length),
    n = n()
  )

results_table %>%
  group_by(n_items) %>%
  summarise(coverage = length(which(ci_contain))/length(ci_contain),
            mean_ci_length = mean(rel_ci_length),
            n = n())

results_table %>%
  pull(pop_rel) %>%
  hist()

results_table %>%
  # filter(sample_sizes == 2500) %>%
  # filter(n_items == 6) %>%
  ggplot(aes(x = pop_rel, y = rel_est, col = factor(n_items), shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = rel_lb, ymax = rel_ub), width = .01) + 
  theme_bw() + 
  coord_cartesian(xlim=0:1, ylim = 0:1) + 
  facet_wrap(~tau_equivalence)

results_table %>%
  ggplot(aes(x = pop_rel, y = rel_est, col = sample_sizes, shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  # geom_errorbar(aes(ymin = rel_lb, ymax = rel_ub), width = .01) + 
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
  group_by(sample_sizes, tau_equivalence) %>%
  summarise(
    mad      = mean(abs(rel_diff)),
    md       = mean((rel_diff)),
    coverage = length(which(ci_contain))/length(ci_contain),
    mean_ci_length = mean(rel_ci_length),
    n = n()
    )

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
