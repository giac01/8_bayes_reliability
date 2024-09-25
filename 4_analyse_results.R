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

loadings_list_paste = lapply(loadings_list, function(x) paste0(x, collapse = "_")) %>% unlist()

loadings_list_pretty =  lapply(loadings_list, function(x) paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", ")) %>% unlist()
loadings_list_pretty2 = lapply(loadings_list, function(x) paste0("Loadings:", paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", "))) %>% unlist()
loadings_list_pretty
loadings_list_pretty2

# I ran the simulation code multiple times on the cluster, so we have several results files we want to join

results_path = file.path("results","4_factor_inequivalent_results")
results_files = list.files(results_path, 
                           pattern = "^4_results_tauinequiv_seed",
                           recursive = FALSE,
                           full.names = TRUE
                           )
results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)
# results = unlist(results)

## Create results table for simulation results ---------------------------------

results_table = data.frame(i = 1:length(results))


results_table$n    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )
results_table$sample_sizes    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )

results_table$n_items         = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n_items )
results_table$loadings        = unlist(sapply(1:nrow(results_table), function(i) paste0(results[[i]]$settings$loadings, collapse = "_" )))
results_table$loading_set     = match(results_table$loadings , loadings_list_paste) 
results_table$loading_list_pretty  = loadings_list_pretty[results_table$loading_set]
results_table$loading_list_pretty2 = loadings_list_pretty2[results_table$loading_set]
results_table$intercepts      = sapply(1:nrow(results_table), function(i) results[[i]]$settings$intercepts )

results_table$pop_rel         = sapply(results, function(x) x$population_reliability) %>% as.numeric()
results_table$pop_ss_loading  = sapply(results, function(x) sum(x$settings$loadings^2)) %>% as.numeric()
results_table$sec_min_loading = sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[2]) %>% as.numeric()
results_table$third_min_loading =  sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[3]) %>% as.numeric()

results_table$rmp_est         = sapply(results, function(x) x$rmp_est[1] )%>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp_est[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp_est[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb
# results_table$rmp_ci_contain  = (results_table$pop_rel > results_table$rmp_lb) & (results_table$pop_rel < results_table$rmp_ub) 

results_table$h_est           = sapply(results, function(x) x$h_reliability$r)%>% as.numeric()
results_table$h_lb            = sapply(results, function(x) x$h_reliability$ci[1])%>% as.numeric()
results_table$h_ub            = sapply(results, function(x) x$h_reliability$ci[2])%>% as.numeric()
# results_table$h_ci_contain    = (results_table$pop_rel > results_table$h_lb) & (results_table$pop_rel < results_table$h_ub) 

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

# Results Table -----------------------------------------------------------------
results_table_long = results_table %>%
  # select(-any_of(c(starts_with("diag")))) %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est, h_est, rmp_lb, h_lb, rmp_ub, h_ub), names_to = c("name", ".value"), names_pattern = "(rmp|h)_(.*)") 

## Comparison between RMP and H ------------------------------------------------

results_table_cleaned =  results_table_long %>%
  group_by( loading_set, sample_sizes, name) %>%
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
    `Mean True Score Coverage` = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
  ) %>%
  ungroup() %>%
  mutate(
    loadings_list_pretty = loadings_list_pretty[loading_set],
    loadings_list_pretty2 = loadings_list_pretty2[loading_set]
  )

results_table_cleaned[which(results_table_cleaned$name=="h"),c("perc_diag_divergences_binary","perc_diag_ebfmi_binary")] = NA

results_table_cleaned %>%
  select(-coverage_se, -loading_set, - pop_rel_sd, 
         -loadings_list_pretty2, -mad,
         -mean, -mean_se, -mean_ub, -mean_lb,
         -bias_se
         ) %>%
  # filter(name == "rmp") %>%
  gt() %>%
  gt::cols_move_to_start(name) %>%
  fmt(
    columns = where(is.numeric),
    fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
  ) %>%
  fmt_number(
    columns = c(n),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(starts_with("coverage"),contains("perc_diag")),
    decimals = 1
  ) %>%
  cols_label(
    pop_rel      ~ "R",
    loadings_list_pretty ~ "Loadings",
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
  tab_spanner(label = "Simulation Parameters", columns = c(pop_rel, loadings_list_pretty,sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c( bias, bias_lb, bias_ub, MSE)) %>%
  tab_spanner(label = "Confidence/Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
  tab_footnote(
    footnote = md(" n<sub>sim</sub> = number of simulations completed for this set of simulation parameters. 
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
      rows = which((sample_sizes ==200 & name == "h"))
      # rows = which(name == "rmp")
    )
  ) %>%
  tab_options(
    table.width = pct(49)
    # table.width = pct(38)
    
  ) %>%
  gt::cols_hide(c(`Mean True Score Coverage` )) %>%
  
  # gt::cols_hide(c(name,`Mean True Score Coverage` )) %>%
  cols_width(
    c(loadings_list_pretty) ~ pct(14),  # Set width of 'Name' column to 100 pixels
    c(perc_diag_divergences_binary) ~ pct(8),  # Set width of 'Name' column to 100 pixels
    c(perc_diag_ebfmi_binary) ~ pct(8)  # Set width of 'Occupation' column to 150 pixels
  )



gtsave(filename = file.path("results","4_table_A.html"))

### Smaller table with fewer comparisons ---------------------------------------
# 
# results_table_long %>%
#   group_by( sample_sizes, n_items, name) %>%
#   mutate(
#     difference = est - pop_rel,
#     ci_correct = (lb < pop_rel & ub > pop_rel),
#     ci_length  = ub - lb
#   ) %>%
#   summarise(
#     n = n(),
#     mean     = mean(est),
#     bias       = mean(difference),
#     mad      = mean(abs(difference)),
#     coverage = length(which(ci_correct))/length(ci_correct),
#     coverage_se = sqrt((coverage*(1-coverage))/n),
#     coverage_lb = coverage - qnorm(0.975)*coverage_se,
#     coverage_ub = coverage + qnorm(0.975)*coverage_se,
#     mean_ci_length = mean(ci_length),
#     `Mean True Score Coverage` = mean(true_score_coverage)
#   ) %>%
#   ungroup() %>%
#   # arrange(desc(coverage)) %>%
#   filter(name !="h") %>%
#   select(-coverage_se, -name) %>%
#   gt() %>%
#   # gt::cols_move_to_start(name) %>%
#   fmt(
#     columns = where(is.numeric),
#     fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
#   ) %>%
#   fmt_number(
#     columns = starts_with("n"),
#     decimals = 0
#   ) %>%
#   fmt_percent(
#     columns = starts_with("coverage"),
#     decimals = 1
#   ) %>%
#   cols_label(
#     sample_sizes ~ "Sample Size",
#     # constained_loadings ~ "Loadings Constrained",
#     # pop_rel_mean ~ "{{R_pop}}",
#     # sens_sigma   ~ "{{:sigma:_sens}}",
#     # n_items      ~ "# Items",
#     bias           ~ "bias",
#     mean_ci_length ~ "Mean Length"
#     
#   )  %>%
#   tab_spanner(label = "Simulation Parameters", columns = c(sample_sizes, n)) %>%
#   tab_spanner(label = "Estimator Performance", columns = c(mean, mad, bias)) %>%
#   tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
#   tab_footnote(
#     "{{R_pop}} = Simulated Population Reliability. mae = Mean Absolute Error. n = number of simulations. These results are averaged over the different sample sizes"
#   ) %>%
#   tab_style(
#     style = cell_fill(color = "lightgray"),
#     locations = cells_body(
#       columns = everything(),
#       rows = which((n_items ==10 | n_items == 40))
#     )
#   ) 

## Plots -----------------------------------------------------------------------

### Coverage -------------------------------------------------------------------

results_table_cleaned %>%
  mutate(loadings_list_pretty2 = factor(loadings_list_pretty2)) %>%
  ggplot(aes(y = coverage, 
             ymin = coverage_lb, 
             ymax = coverage_ub, 
             x = sample_sizes, 
             col = name)) +
  geom_errorbar(size = 2) +  
  geom_hline(yintercept = 0.95) +
  jtools::theme_apa() +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(x = "Number of Items Per Simulation", 
       y = "Credible Interval Coverage") +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~ loadings_list_pretty2) 


### Distribution of effect sizes -----------------------------------------------

##### Using just summary data --------------------------------------------------

results_table_cleaned %>%
  ggplot(aes(y = mean, ymin = mean_lb, ymax = mean_ub, x = sample_sizes, col = name)) +
  geom_point() +
  geom_errorbar(size = 2) +  
  geom_hline(aes(yintercept = pop_rel)) +
  jtools::theme_apa() +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(x = "Number of Items Per Simulation", y = "Credible Interval Coverage") +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~ loadings_list_pretty2) 

##### Using raw data ----------------------------------------------------------

results_table_long  %>%
  ggplot(aes(y = est, x = sample_sizes, col = name)) +
  geom_jitter(width = 0.1, height = 0, shape = 1, alpha = .5) +
  geom_hline(aes(yintercept = pop_rel)) +
  stat_summary(fun.data = ggplot2::mean_cl_normal,
               geom = "errorbar",
               size = 1
               ) +
  facet_wrap(~ loading_list_pretty2) + 
  labs(y = "Sample reliability estimate", x = "Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    # legend.box.just = "right",
    # legend.margin = margin(6, 6, 6, 6)
  )

ggsave(file.path("plots","4_rawdata_plot.png"), width = 8, height = 7.5)

colnames(results_table_long)
## Do models without model fit issues fit better ?? ----------------------------

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
    bias       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
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
    bias       = mean(difference),
    mad      = mean(abs(difference)),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
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
    bias       = mean(difference),
    coverage = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
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
    bias       = mean((rmp_diff)),
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
    bias       = mean((rel_diff)),
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
