# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

loadings_list = list(
  c( 0, 0, 0, 0, 0, 0),
  c(.1,.1,.1,.1,.1),
  c(.3,.2,.1),
  c(.4,.3,.3,.2,.1,.0),
  c(.4,.4,.4,.4),
  c(.6,.5,.3,.1,.1,.1),
  c(.7,.6,.5,.5,.4,.4,.4,.3,.3)
)

loadings_list_paste = lapply(loadings_list, function(x) paste0(x, collapse = "_")) %>% unlist()

loadings_list_pretty =  lapply(loadings_list, function(x) paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", ")) %>% unlist()
loadings_list_pretty2 = lapply(loadings_list, function(x) paste0("Loadings:", paste0(gbtoolbox::apa_num(x, n_decimal_places = 1), collapse = ", "))) %>% unlist()
loadings_list_pretty
loadings_list_pretty2

# I ran the simulation code multiple times on the cluster, so we have several results files we want to join

results_path = file.path("results","study1b_results")
results_files = list.files(results_path, 
                           pattern = ".rds",
                           # pattern = "^4_results_tauinequiv_seed",
                           recursive = FALSE,
                           full.names = TRUE
                           )
results = lapply(results_files, function(x) readRDS(x))

results = do.call("c", results)
# results = unlist(results)

## Create results table for simulation results ---------------------------------

results_table = data.frame(i = 1:length(results))


# results_table$n    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )
results_table$sample_sizes    = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n )

results_table$n_items         = sapply(1:nrow(results_table), function(i) results[[i]]$settings$n_items )
results_table$loadings        = unlist(sapply(1:nrow(results_table), function(i) paste0(results[[i]]$settings$loadings, collapse = "_" )))
results_table$loading_set     = match(results_table$loadings , loadings_list_paste) 
results_table$loading_list_pretty  = loadings_list_pretty[results_table$loading_set]
results_table$loading_list_pretty2 = loadings_list_pretty2[results_table$loading_set]
results_table$intercepts      = sapply(1:nrow(results_table), function(i) results[[i]]$settings$intercepts )

results_table$pop_coefh         = sapply(results, function(x) x$population_reliability) %>% as.numeric()    # Population coefficient H 
results_table$pop_ss_loading  = sapply(results, function(x) sum(x$settings$loadings^2)) %>% as.numeric()
results_table$sec_min_loading = sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[2]) %>% as.numeric()
results_table$third_min_loading =  sapply(results, function(x) sort(x$settings$loadings, decreasing = TRUE)[3]) %>% as.numeric()

results_table$rmp_est         = sapply(results, function(x) x$rmp_est[1] )%>% as.numeric()
results_table$rmp_lb          = sapply(results, function(x) x$rmp_est[2]) %>% as.numeric()
results_table$rmp_ub          = sapply(results, function(x) x$rmp_est[3]) %>% as.numeric()
results_table$rmp_ci_length   = results_table$rmp_ub - results_table$rmp_lb

results_table$h_est           = sapply(results, function(x) x$h_reliability$r)%>% as.numeric()
results_table$h_lb            = sapply(results, function(x) x$h_reliability$ci[1])%>% as.numeric()
results_table$h_ub            = sapply(results, function(x) x$h_reliability$ci[2])%>% as.numeric()

results_table$a_est           = sapply(results, function(x) x$alpha_reliability$est)%>% as.numeric()
results_table$a_lb            = sapply(results, function(x) x$alpha_reliability$ci.lower)%>% as.numeric()
results_table$a_ub            = sapply(results, function(x) x$alpha_reliability$ci.upper)%>% as.numeric()

# MCMC IRT coefficient H

results_table$irth_est           = sapply(results, function(x) x$mcmc_coefh$mcmc_coef_h)%>% as.numeric()
results_table$irth_lb            = sapply(results, function(x) x$mcmc_coefh$.lower)%>% as.numeric()
results_table$irth_ub            = sapply(results, function(x) x$mcmc_coefh$.upper)%>% as.numeric()

# Other coverage statistics

# results_table$true_score_cor2              = sapply(results, function(x) x$true_score_cor$estimate^2) %>% as.numeric()
results_table$true_score_coverage          = sapply(results, function(x) x$true_score_coverage)%>% as.numeric()
results_table$true_score_factor_score_cor2 = sapply(results, function(x) x$true_score_factor_score_cor$estimate^2) %>% as.numeric() # Correlation between true score and psych::fa factor scores 
results_table$ascots  = sapply(results, function(x) x$true_score_model_score_cor$estimate^2) %>% as.numeric()  # Correlation between true score and bIRT factor scores 

results_table$sample_sizes = factor(results_table$sample_sizes)
results_table$diag_divergences = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary = as.numeric(results_table$diag_ebfmi>0)


# PERFORMANCE CALCULATION  -----------------------------------------------------------------

## Divergences -----------------------------------------------------------------
divergences = 
  results_table %>%
  filter(diag_divergences>0) %>%
  pull(diag_divergences) 

cumsum(table(divergences))/sum(table(divergences))


# Convert data to long 

results_table_long = results_table %>%
  # select(-any_of(c(starts_with("diag")))) %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est, h_est, a_est, irth_est,
                        rmp_lb, h_lb, a_lb, irth_lb,
                        rmp_ub, h_ub, a_ub, irth_ub
                        ), names_to = c("name", ".value"), names_pattern = "(rmp|h|a|irth)_(.*)") 

## Mean Sample reliability ---------------------------------------------------------

results_table_long %>%
  # filter(name == "rmp") %>%
  group_by(name, loading_set, sample_sizes) %>%
  mutate(
    name       = factor(name, levels = c("rmp","a","h","irth")),
    # estimand = mean(ascots),                           # mean correlation between bayes IRT score and true score
    estimand                          = (sum(ascots)-ascots)/(n() -1 ),
    mean_est                          = mean(est),
    mean_true_score_factor_score_cor2 = mean(true_score_factor_score_cor2)  # mean correlation between psych::fa factor score and true score
  ) %>% # select(3:5,"pop_coefh","estimand","estimand2")
  ungroup() %>%
  group_by(name) %>%
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb,
    ci_be_correct = (lb <= mean_est & ub >= mean_est)
  ) %>%
  summarise(
    
    estimand    =  mean(ascots),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    mean_true_score_factor_score_cor2 = mean(mean_true_score_factor_score_cor2),
    n           = n(),
    
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
    
    `Mean True Score Coverage`   = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
    
  ) %>%
  ungroup() %>% 
  knitr::kable()
  # select(name, n, 
  #        estimand,
  #          RMSE, RMSE_lb, RMSE_ub,
  #          bias, bias_lb, bias_ub,
  #          EmpSE, EmpSE_lb, EmpSE_ub,
  #          coverage, coverage_lb, coverage_ub
  # ) %>%
  # write.csv(file.path("results_tables","2_results_performance_samplereliability.csv"))


## Maximal Reliability Comparison ----------------------------------------------

results_table_long %>%
  mutate(
    name       = factor(name, levels = c("rmp","a","h","irth")),
    estimand   = pop_coefh,
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb,
    # ci_be_correct = (lb <= mean_est & ub >= mean_est)
  ) %>%
  group_by(name) %>%
  summarise(
    
    estimand    = mean(pop_coefh),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    n           = n(),
    
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
    MSE_ub      = MSE - qnorm(0.975)*MSE_se,
    
    # Root Mean Squared Error 
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),
    
    #Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    
    mean_ci_length = mean(ci_length),
    
    # `Mean True Score Coverage`   = mean(true_score_coverage),
    # perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    # perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
  ) %>%
  ungroup() %>%
  select(name, n, 
         RMSE, RMSE_lb, RMSE_ub,
         bias, bias_lb, bias_ub,
         EmpSE, EmpSE_lb, EmpSE_ub,
         coverage, coverage_lb, coverage_ub
         ) %>%
  write.csv(file.path("results_tables","2_results_method_comparison.csv"))

# Check seeds ------------------------------------------------------------------
n_results <- 10000
matches <- matrix(FALSE, nrow=n_results, ncol=n_results)

for(i in 1:(n_results-1)) {
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

identical(results[[1]]$settings$seed,results[[65]]$settings$seed)


# Performance in each condition ------------------------------------------------

# currently not setting specific estimands for different methods

results_table_cleaned = 
results_table_long %>%
  group_by( loading_set, sample_sizes, name) %>%
  mutate(
    # estimand = mean(ascots),                           # mean correlation between bayes IRT score and true score
    estimand = (sum(ascots)-ascots)/(n() -1 ),
    mean_true_score_factor_score_cor2 = mean(true_score_factor_score_cor2)  # mean correlation between psych::fa factor score and true score
        ) %>%
  mutate(
    difference = est - estimand,
    ci_correct = (lb <= estimand & ub >= estimand),
    ci_length  = ub - lb
  ) %>%
  summarise(
    
    estimand    =  mean(ascots),
    pop_coefh     = mean(pop_coefh),                           # population coefficient H
    pop_coefh_sd  = sd(pop_coefh, na.rm = TRUE),               # sanity check (should be 0)
    mean_true_score_factor_score_cor2 = mean(mean_true_score_factor_score_cor2),
    n           = n(),
    
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
    
    # Root Mean Squared Error 
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),
    
    #Coverage
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    
    # Bias corrected coverage 
    # cove_cor   = length(which(ci_bias_elimated_correct))/length(ci_bias_elimated_correct),
    # cove_cor   = length(which(ci_bias_elimated_correct))/length(ci_bias_elimated_correct),
    # cove_cor   = length(which(ci_bias_elimated_correct))/length(ci_bias_elimated_correct),
    # 
    
    mean_ci_length = mean(ci_length),
    
    `Mean True Score Coverage`   = mean(true_score_coverage),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n
    
  ) %>%
  ungroup() %>%
  mutate(
    loadings_list_pretty = loadings_list_pretty[loading_set],
    loadings_list_pretty2 = loadings_list_pretty2[loading_set]
  )

results_table_cleaned[which(results_table_cleaned$name!="rmp"),c("perc_diag_divergences_binary","perc_diag_ebfmi_binary")] = NA
results_table_cleaned[which(results_table_cleaned$name!="rmp"),c("mean_true_score_cor2")] = NA
results_table_cleaned$name[results_table_cleaned$name=="rmp"] = "RMU"
results_table_cleaned$name[results_table_cleaned$name=="h"] = "H (FA)"
results_table_cleaned$name[results_table_cleaned$name=="a"] = "Alpha"
results_table_cleaned$name[results_table_cleaned$name=="irth"] = "H (IRT)"

## gt table ---------------------------------------------------------------------
results_table_cleaned %>%
  select(-any_of(c("coverage_se", "loading_set", "pop_coefh_sd", "mean_true_score_cor2",
                   "loadings_list_pretty2", "mad",
                   "mean", "mean_se", "mean_ub", "mean_lb",
                   "bias_se"
  ))) %>%
  # filter(name == "RMU") %>%
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
    name         ~ "Est",
    pop_coefh      ~ "coef H",
    mean_true_score_factor_score_cor2 ~ "Estimand (FA)",
    # mean_true_score_cor2 ~ "{{R_(*ρ^2*)}}",
    # mean_factor_score_cor2 ~ "R_fac",
    loadings_list_pretty ~ "Loadings",
    sample_sizes ~ "{{n_obs}}",
    n            ~ "{{n_sim}}",
    bias           ~ "bias",
    mean_ci_length ~ "Mean Length",

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
    perc_diag_divergences_binary ~ "% Divergent Transitions",
    perc_diag_ebfmi_binary ~  "% Low E-BFMI"
  )  %>%
  
  tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
  tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
  tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
  tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
  tab_spanner(label = "Simulation Parameters", columns = c(estimand, pop_coefh, loadings_list_pretty,sample_sizes, n)) %>%
  tab_spanner(label = "Estimator Performance", columns = c(contains("RMSE"),contains("EmpSE"), contains("bias"))) %>%
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
      rows = which((name == "RMU"))
    )
  ) %>%
  tab_options(
    table.width = pct(45)
  ) %>%
  gt::cols_hide(
    c(`Mean True Score Coverage`, starts_with("MSE"), mean_true_score_factor_score_cor2, ends_with("_se"))
    ) %>%
  cols_width(
    c(loadings_list_pretty) ~ pct(10),         # Set width of 'Name' column to 100 pixels
    c(perc_diag_divergences_binary) ~ pct(8),  # Set width of 'Name' column to 100 pixels
    c(perc_diag_ebfmi_binary) ~ pct(8),        # Set width of 'Occupation' column to 150 pixels
    everything() ~ px(.4)
  ) %>%
  opt_horizontal_padding(scale = 0) %>%

  gtsave(filename = file.path("results_tables","2_study1_performance_comparison_differentestimators.html"))


# gtsave(filename = file.path("results","4_table_A.html"))
# gtsave(filename = file.path("results","4_table_withcoefficient_h.docx"))
# 


# Plots -----------------------------------------------------------------------

## bar Charts ------------------------------------------------------

# write.csv(results_table_cleaned, "99_claude_data.csv")

data_plot = results_table_cleaned %>% 
  rename(
    Estimator = name,
    Coverage  = coverage
         ) %>%
  pivot_longer(cols = c("bias","EmpSE","MSE", "Coverage"), values_to = "Est") %>%
  mutate(
    name = factor(name, levels = c("bias", "EmpSE", "MSE", "Coverage")),
    Estimator = factor(Estimator, levels = c("RMU", "Alpha", "H (FA)", "H (IRT)"))
  )

format_decimals <- function(x, decimals = 2) {
  format_number <- function(num) {
    if (abs(num - 1) < 1e-10) {  # Check if the number is very close to 1
      return("1")
    } else if (abs(num) < 1) {
      return(sub("^-?0.", ".", sprintf(paste0("%.", decimals, "f"), num)))
    } else {
      return(sprintf(paste0("%.", decimals, "f"), num))
    }
  }
  sapply(x, format_number)
}

data_plot %>%
  filter(loading_set!=7) %>%
  ggplot(aes(y = Est,  group = Estimator, x = sample_sizes, fill = Estimator)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = .9) +
  facet_grid(
    rows   = vars(name), 
    cols   = vars(loading_set),
    scales = "free_y", 
    switch = "y"
    ) + 
  labs(
    y = NULL, 
    x = expression(paste("Sample Size of Simulated Dataset (n", scriptstyle(obs), ")"))
  ) +
  theme_bw(
    base_size = 20
  ) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_text(hjust = 1),
    strip.placement    = "outside",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    ) +
  ggh4x::facetted_pos_scales(
    y = list(
      name == "bias"     ~ scale_y_continuous(labels = format_decimals),
      name == "EmpSE"    ~ scale_y_continuous(labels = format_decimals),
      name == "MSE"      ~ scale_y_continuous(limits = c(NA, 0.03), labels = format_decimals),
      name == "Coverage" ~ scale_y_continuous(limits = c(.9, 1), labels = function(x) format_decimals(x,decimals = 3) )
    ))


## RMU violine plot  -----------------------------------------------------------
library(grid)

results_table_cleaned2 = results_table_cleaned %>% 
  filter(name == "RMU") %>% 
  rename(
    loading_list_pretty = loadings_list_pretty
    )

results_table_long  %>%
  filter(name == "rmp") %>%
  group_by(sample_sizes, loading_set) %>%
  mutate(mean_ascots = mean(ascots)) %>%
  ggplot(aes(y = est, x = sample_sizes)) +
  # geom_jitter(
  #   width = 0.1, 
  #   height = 0, 
  #   shape = 1, 
  #   alpha = .4
  #   ) +
  # see::geom_violinhalf
  geom_violin(
    width = .95, 
    fill = "grey",
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
    position = position_dodge(width = 0.2)
  ) + 
  stat_summary(fun = mean,
               geom = "point",
               # y = mean,
               size = 3,
               shape = 3,
               col = "red",
               stroke = .9
               ) +
  geom_point(
    data = results_table_cleaned2,
    aes(
      y  = estimand, 
      x = sample_sizes
      ), 
    # alpha = .6,
    shape = 1,
    size = 2,
    col = "red",
    stroke = .9
    ) +
  geom_hline(aes(yintercept = pop_coefh), linetype = "dashed") +
  facet_wrap(
    ~ loading_list_pretty,
    nrow = 3,
    ncol = 3
    ) + 
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  geom_text(
    data = results_table_cleaned2,
    aes(
      y = ifelse(loading_list_pretty == " .7,  .6,  .5,  .5,  .4,  .4,  .4,  .3,  .3", 0, 0.89),
      # y = .89,
      x = factor(sample_sizes),
      label = paste0(
        "bias = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", bias)), "\n",
        "RMSE = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", sqrt(MSE))), "\n",
        "cov = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", coverage)), "\n",
        "EmpSE = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", EmpSE)), "\n",
        "N = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.f", n))
        
      )
    ),
    vjust = ifelse(results_table_cleaned2$loading_list_pretty == " .7,  .6,  .5,  .5,  .4,  .4,  .4,  .3,  .3", 0, 1),
    # vjust = 1,
    hjust = 1,
    size = 1.8,
    position = position_nudge(x=.42),
    col = "grey46"
  ) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    # legend.box.just = "right",
    # legend.margin = margin(6, 6, 6, 6)
  )
  
ggsave(file.path("plots","2_study1b_violinplot.pdf"), width = 6.2, height = 7)
# ggsave(file.path("plots","2_study1b_violinplot.png"), width = 6.2, height = 7)

colnames(results_table_long)

## Comparison violin plot -----------------------------------------------------------

results_table_long %>%
  mutate(name = factor(name, 
                       levels = c("rmp", "a", "h", "irth"),
                       labels = c("RMU", "Alpha", "H (FA)", "H (IRT)"))) %>%
  ggplot(aes(y = est, x = sample_sizes, col = name, fill = name)) +
  # Adjust position and overlap of violins
  geom_violin(
    width = 0.8, 
    position = position_dodge(width = 0.8)
    ) + 
  geom_hline(aes(yintercept = pop_coefh), linetype = "dashed") +
  # Adjust error bar position to match violin positions
  # stat_summary(
  #   fun.data = ggplot2::mean_cl_normal,
  #   geom = "errorbar",
  #   width = 0.2,
  #   position = position_dodge(width = 0.8)
  # ) +
  # Add mean points for better visibility
  stat_summary(
    fun = mean,
    geom = "point",
    size = .5,
    col = "black",
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ loading_list_pretty, scales = "fixed") +
  coord_cartesian(ylim = c(-.00, .97)) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(
    col = "none", 
    fill = guide_legend(title = "Estimator")) + 
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    panel.spacing = unit(0.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  theme_bw() + 
  theme(
    legend.position = c(.9, .03),    # Adjust these values to position the legend
    legend.justification = c(1, 0),     # Adjust these values to align the legend
    legend.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path("plots","2_study1b_violinplot_allestimators.pdf"), width = 6.2, height = 7)


## Credible Intervals ------------------------------------------------------

results_table %>%
  # filter(sample_sizes==1000) %>%
  arrange(rmp_est) %>%  
  group_by(sample_sizes,loading_list_pretty2) %>%
  mutate(
    x = 1:n(),
    estimand = mean(ascots),                           # mean correlation between bayes IRT score and true score
    ci_correct = (rmp_lb <= estimand & rmp_ub >= estimand),
  ) %>%
  ungroup() %>%
  ggplot(aes(ymin = rmp_lb, ymax = rmp_ub, 
             x = factor(x))) +
  geom_errorbar(aes(col = ci_correct)) + 
  geom_hline(aes(yintercept = estimand), col = "red") +
  facet_wrap(~ loading_list_pretty2 + sample_sizes,
             scales = "free", ncol = 3)


# OLD ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## Do models without model fit issues fit better ?? ----------------------------

# Does population coefficient H describe how well the observed factor scores and true scores correlate? -------------------

## Correlation between cor(observed factor scores )

results_table %>%
  ggplot(aes(x = true_score_cor2, y = pop_coefh)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) + 
  labs(x = "cor(factor scores calculated with pop weights, true scores)^2", 
       y = "pop reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0,1), xlim=c(0,1))

## Correlation between stan (with TRUE weights) and true scores?

results_table %>%
  ggplot(aes(x = ascots^2, y = pop_coefh)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "pop reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0,1), xlim=c(0,1))
ggsave(file.path("plots","2_stan_true_scot_plot.png"), width = 10, height = 6)

results_table %>%
  ggplot(aes(x = true_score_factor_score_cor2^2, y = pop_coefh)) + 
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
  ggplot(aes(x = ascots^2, y =  rmp_est)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "Estimated reliability (RMP)") +
  geom_smooth(method = "lm") +
  coord_fixed(ylim = c(0,1), xlim=c(0,1))
# 
# lm(rmp_est ~ I(ascots^2), data=results_table) %>% summary()
# lm(pop_coefh ~ I(ascots^2), data=results_table) %>% summary()
# lm(rmp_est ~ I(ascots^2), data=results_table) %>% confint()

results_table %>%
  filter(sample_sizes %in% c(200,500)  & n_items == 4) %>%
  ggplot(aes(x = ascots^2, y =  pop_coefh)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "Red") +
  facet_wrap(~n_items + sample_sizes) +
  labs(x = "cor(factor score from stan model, true scores)^2", 
       y = "Population reliability (true coefficient H)") +
  geom_smooth(method = "lm") +
  coord_fixed(ylim = c(0,1), xlim=c(0,1))



results_table %>%
  # filter(sample_sizes %in% c(200,500)  & n_items == 4) %>%
  ggplot(aes(x = pop_coefh, y =  rmp_est)) + 
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
    pop_coefh_dec = cut(pop_coefh, breaks = c(0,.2,.4,.6,.8, 1))
  ) %>% 
  group_by( pop_coefh_dec, name) %>%
  mutate(
    difference = est - pop_coefh,
    ci_correct = (lb < pop_coefh & ub > pop_coefh),
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
    difference = est - pop_coefh,
    ci_correct = (lb < pop_coefh & ub > pop_coefh),
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
    difference = est - pop_coefh,
    ci_correct = (lb < pop_coefh & ub > pop_coefh),
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
  # filter(pop_coefh>.3 & pop_coefh <.8) %>%
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
  pull(pop_coefh) %>%
  hist()

results_table %>%
  # filter(sample_sizes == 2500) %>%
  # filter(n_items == 6) %>%
  ggplot(aes(x = pop_coefh, y = rel_est, col = factor(n_items), shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = rel_lb, ymax = rel_ub), width = .01) + 
  theme_bw() + 
  coord_cartesian(xlim=0:1, ylim = 0:1) + 
  facet_wrap(~tau_equivalence)

results_table %>%
  ggplot(aes(x = pop_coefh, y = rel_est, col = sample_sizes, shape = tau_equivalence)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point( size = 3) +
  # geom_errorbar(aes(ymin = rel_lb, ymax = rel_ub), width = .01) + 
  theme_bw() + 
  coord_cartesian(xlim=0:1, ylim = 0:1) + 
  facet_wrap(~tau_equivalence)



results_table %>%
  ggplot(aes(x = pop_coefh, y = (true_score_cor2), col = sample_sizes, shape = tau_equivalence)) +
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

results_table$pop_coefh %>% hist()

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


# Check weird results -----------------------------------------

results_table %>%
  filter(loading_set==7) %>% 
  filter(rmp_est<.1) %>%
  slice(1:5)

.Random.seed = results[[357]]$settings$seed

results[[357]]


results[[483]]


