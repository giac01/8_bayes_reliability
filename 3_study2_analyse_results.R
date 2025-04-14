# Load Packages ----------------------------------------------------------------

library(tidyverse)
library(gt)

rm(list = ls(all.names = TRUE))

# Load Data --------------------------------------------------------------------

population_reliability = readRDS(file.path("results","6_sdt_results","population_reliability_estimates.Rds"))
population_reliability$n_items = population_reliability$n_items *2 # In the results, we define n_items as the TOTAL number of trials across both conditions

results_path = file.path("results","6_sdt_results")

results_files = list.files(results_path, 
                           pattern = "^6_results_seed",
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
results_table$n_items         = sapply(results, function(x) x$settings$n_items) * 2 # This is DOUBLED because we define n_items as the number of trials in the each condition (OLD or NEW)

results_table$settings_used = paste(
  results_table$sens_mean, 
  results_table$sens_sigma, 
  results_table$k_mean, 
  results_table$k_sigma, 
  results_table$n_items, 
  sep = "_"
)

results_table$settings_used_with_npps = paste(
  results_table$sens_mean, 
  results_table$sens_sigma, 
  results_table$k_mean, 
  results_table$k_sigma, 
  results_table$n_items, 
  results_table$sample_sizes,
  sep = "_"
)

population_reliability$settings_used_with_npps = paste(
  population_reliability$sens_mean, 
  population_reliability$sens_sigma, 
  population_reliability$k_mean, 
  population_reliability$k_sigma, 
  population_reliability$n_items, 
  population_reliability$sample_sizes,
  sep = "_")

# Pop Reliability estimates
results_table$ascots = sapply(results, function(x) x$cor_bayes_estimate_true$estimate^2) %>% as.numeric()
results_table$ascots = ifelse(is.na(results_table$ascots), 0 , results_table$ascots)

# Large sumulation 10 mill pop reliability estiamte 

results_table$population_reliability_ls = population_reliability$population_reliability[match(results_table$settings_used_with_npps, population_reliability$settings_used_with_npps )]

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

results_table$testretest_cor = sapply(results, function(x) as.numeric(x$testretest_cor[4])) %>% as.numeric()

#Diagnostics
results_table$diag_divergences        = sapply(results, function(x) x$diagnostics_divergences) %>% as.numeric()
results_table$diag_divergences_binary = as.numeric(results_table$diag_divergences>0)
results_table$diag_ebfmi              = sapply(results, function(x) length(which(x$diagnostics_ebfmi<.2)))
results_table$diag_ebfmi_binary       = as.numeric(results_table$diag_ebfmi>0)

# Filter out trials with k_sigma ==0 --------------------------------------------

# Performance was good in these conditions, I removed this condition to simply the presentation of the results

results_table = results_table[results_table$k_sigma!=0,]

# Performance Measures ---------------------------------------------------------

## Check for model-fitting issues ----------------------------------------------

results_table$diag_ebfmi_binary %>% table()
results_table$diag_divergences_binary %>% table()

## Create results_table_long -----------------------------------------------------------------

results_table %>%
  mutate(
    sample_sizes = factor(sample_sizes),
    sens_sigma  = factor(sens_sigma),
    n_items     = factor(n_items)
  ) %>%
  ggplot(aes( x = ascots, y = rmp_est, shape = sample_sizes, col = n_items)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~sens_sigma)

results_table_long = results_table %>%
  rowid_to_column() %>%
  pivot_longer(cols = c(rmp_est,rmp_lb,rmp_ub, sh_est, sh_lb, sh_ub), names_to = c("name", ".value"), names_pattern = "(rmp|sh)_(.*)") 

## Overall Performance ---------------------------------------------------------

results_table_long %>%
  group_by(n_items, sens_sigma, k_sigma, name, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    pop_rel    = (sum(ascots) - ascots) / (n() - 1),
    # pop_rel   =  mean(ascots),
    difference = est - pop_rel,
    ci_correct = (lb <= pop_rel & ub >= pop_rel),
    ci_length  = ub - lb
  ) %>% 
  ungroup() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    pop_rel     = mean(ascots),
    pop_rel_sd  = sd(ascots),
    # population_reliability_ls = unique(population_reliability_ls),
    mean        = mean(est),
    
    bias        = mean(difference),
    bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
    bias_lb     = bias - qnorm(0.975)*bias_se,
    bias_ub     = bias + qnorm(0.975)*bias_se,  
    
    EmpSE       = sd(est),
    EmpSE_se    = EmpSE/sqrt(2*(n-1)),
    EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
    EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,
    
    # Mean Squared Error
    MSE         = mean((difference)^2),
    MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),
    MSE_lb      = MSE - qnorm(0.975)*MSE_se,
    MSE_ub      = MSE + qnorm(0.975)*MSE_se,
    
    # Root Mean Squared Error 
    RMSE        = sqrt(MSE),
    RMSE_lb     = sqrt(MSE_lb),
    RMSE_ub     = sqrt(MSE_ub),
    
    mae         = mean(abs(difference)),
    
    coverage    = length(which(ci_correct))/length(ci_correct),
    coverage_se = sqrt((coverage*(1-coverage))/n),
    coverage_lb = coverage - qnorm(0.975)*coverage_se,
    coverage_ub = coverage + qnorm(0.975)*coverage_se,
    
    mean_ci_length = mean(ci_length),
    perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
    perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n,
    mean_testretest_cor = mean(testretest_cor)
  )  %>%
  select(-pop_rel, -pop_rel_sd, -mean,-ends_with("_se")) %>%
  ungroup() %>%
  knitr::kable(digits = 3)

## Relative RMSE ---------------------------------------------------------------

x = 0.21065
y = 0.27458

rp = 100*(y/x-1)

x + rp/100*x


## Performance in each condition ---------------------------------------------------

  results_table_cleaned = 
  results_table_long %>%
    group_by(n_items, sens_sigma, k_sigma, name, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
    mutate(
      pop_rel_loo = (sum(ascots) - ascots) / (n() - 1),
      pop_rel     =  mean(ascots),
      difference = est - pop_rel,
      ci_correct = (lb <= pop_rel & ub >= pop_rel),
      ci_length  = ub - lb
    ) %>%
    summarise(
      n = n(),
      pop_rel     = mean(ascots),
      pop_rel_sd  = sd(ascots),
      population_reliability_ls = unique(population_reliability_ls),
      mean        = mean(est),
      
      bias        = mean(difference),
      bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
      bias_lb     = bias - qnorm(0.975)*bias_se,
      bias_ub     = bias + qnorm(0.975)*bias_se,  
      
      EmpSE       = sd(est),
      EmpSE_se    = EmpSE/sqrt(2*(n-1)),
      EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
      EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,
      
      # Mean Squared Error
      MSE         = mean((difference)^2),
      MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),
      MSE_lb      = MSE - qnorm(0.975)*MSE_se,
      MSE_ub      = MSE + qnorm(0.975)*MSE_se,
      
      # Root Mean Squared Error 
      RMSE        = sqrt(MSE),
      RMSE_lb     = sqrt(MSE_lb),
      RMSE_ub     = sqrt(MSE_ub),
      
      mae         = mean(abs(difference)),
      
      coverage    = length(which(ci_correct))/length(ci_correct),
      coverage_se = sqrt((coverage*(1-coverage))/n),
      coverage_lb = coverage - qnorm(0.975)*coverage_se,
      coverage_ub = coverage + qnorm(0.975)*coverage_se,
      
      mean_ci_length = mean(ci_length),
      perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
      sum_diag_divergences_binary = sum(diag_divergences_binary),
      sum_diag_ebfmi_binary       = sum(diag_ebfmi_binary),
      mean_testretest_cor = mean(testretest_cor)
    )  %>%
    ungroup()

  results_table_cleaned %>%
    # filter(name == "rmp") %>%
    mutate(name = case_when(
      name == "rmp"  ~ "RMU",
      name == "sh" ~ "SH",
      TRUE ~ NA_character_  # Catches any other values
    )) %>%
    select(-pop_rel_sd, -coverage_se) %>%
    select(name, pop_rel, population_reliability_ls, everything()) %>%
    arrange(sens_sigma, n_items, k_sigma, sample_sizes, name) %>%
    gt() %>%
    fmt(
      columns = where(is.numeric),
      fns = function(x) gbtoolbox::apa_num(x, n_decimal_places = 3)
    ) %>%
    fmt_number(
      columns = c(n, n_items, sum_diag_divergences_binary, sum_diag_ebfmi_binary),
      decimals = 0
    ) %>%
    fmt_percent(
      columns = c(starts_with("coverage"),perc_diag_divergences_binary),
      decimals = 1
    ) %>%
    cols_label(
      sample_sizes ~ "{{n_obs}}",
      n            ~ "{{n_sim}}",      # avg_cor2     ~ "{{:rho:_:theta:,x^2}}",
      # pop_rel      ~ "{{mean[:rho:_:theta:,x^2 ]}}",
      pop_rel ~ "estimand",
      # sens_mean    ~ "{{:mu:_d'}}",
      sens_sigma   ~ "{{:sigma:_d'}}",
      k_sigma      ~ "{{:sigma:_:kappa:}}",
      n_items      ~ "{{n_trials}}",
      population_reliability_ls ~ "{{R_trt}}",
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
      mean_ci_length ~  md("Mean<br>Length"),
      name         ~ "Est",
      perc_diag_divergences_binary ~ md("% DT"),
      sum_diag_divergences_binary ~ md("N Divergent<br>Transitions"),
      sum_diag_ebfmi_binary ~   md("% Low<br>E-BFMI"),
      # .fn = md
      
    )  %>%
    tab_spanner(label = "Bias 95% CI", columns = c(bias, bias_lb, bias_ub)) %>%
    tab_spanner(label = "Coverage 95% CI", columns = c(coverage, coverage_lb, coverage_ub)) %>%
    tab_spanner(label = "RMSE 95% CI", columns = c(RMSE, RMSE_lb, RMSE_ub)) %>%
    tab_spanner(label = "EmpSE 95% CI", columns = c(EmpSE, EmpSE_lb, EmpSE_ub)) %>%
    tab_spanner(label = "Simulation Parameters", 
                columns = c(name,pop_rel,population_reliability_ls, sens_sigma, n_items, k_sigma, sample_sizes, n)) %>%
    tab_spanner(label = "Estimator Performance",
                columns = c(contains("RMSE"),contains("EmpSE"), contains("bias"))) %>%
    tab_spanner(label = "Credible Interval Performance", columns = c(starts_with("coverage"),"mean_ci_length")) %>%
    tab_footnote(
      footnote = html("<b>n<sub>sim</sub></b> = number of simulations completed for this set of simulation parameters.
                <b>n<sub>obs</sub></b> = number of subjects per simulation.
                <b>RMSE</b> = Root Mean Squared Error.
                <b>Coverage</b> = proportion of times the 95% credible intervals include the population reliability, which should be around 95%.
                <b>estimand</b> = ASCOTS (Average Squared Correlation between Observed and True Scores).
                <b>Mean Length</b> = Mean length of credible or confidence interval.
                <b>σ<sub>d'</sub></b> = standard deviation of population true sensitivity values across subjects.
                <b>n<sub>trials</sub></b> = number of trials completed per participant.
                <b>% DT</b> = Percent of simulations with divergent transitions (applies to Bayesian measurement models only).     
                      "
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(
        columns = everything(),
        rows = which((name=="RMU"))
      )
    ) %>%
    tab_options(
      table.width = pct(35)
    ) %>%
  gt::cols_hide(
    c(mae,bias_se, k_sigma, mean, ends_with("_se"),mean_testretest_cor, starts_with("MSE"), contains("ebfmi"),sum_diag_divergences_binary,population_reliability_ls)
    ) %>%

  gtsave(filename = file.path("results_tables","3_study2_performance_comparison.html"))
  

# Comparison of split half and RMP ---------------------------------------------
  
  # same as above, but we use the simulated reliability as the TARGET ESTIMAND!   
  
  results_table_cleaned_2 = 
    results_table_long %>%
    group_by(n_items, sens_sigma, k_sigma, name, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
    mutate(
      pop_rel = (sum(ascots) - ascots) / (n() - 1),
      # pop_rel     =  unique(population_reliability_ls),
      difference = est - pop_rel,
      ci_correct = (lb <= pop_rel & ub >= pop_rel),
      ci_length  = ub - lb
    ) %>%
    summarise(
      n = n(),
      pop_rel     = mean(ascots),
      pop_rel_sd  = sd(ascots),
      population_reliability_ls = unique(population_reliability_ls),
      mean        = mean(est),
      bias        = mean(difference),
      bias_se     = sqrt(1/(n*(n-1))*sum((est-mean)^2)),
      bias_lb     = bias - qnorm(0.975)*bias_se,
      bias_ub     = bias + qnorm(0.975)*bias_se,  
      EmpSE       = sd(est),
      EmpSE_se    = EmpSE/sqrt(2*(n-1)),
      EmpSE_lb    = EmpSE - qnorm(0.975)*EmpSE_se,
      EmpSE_ub    = EmpSE + qnorm(0.975)*EmpSE_se,
      
      # Mean Squared Error
      MSE         = mean((difference)^2),
      MSE_se      = sqrt(sum((difference^2-MSE)^2)/(n*(n-1))),
      MSE_lb      = MSE - qnorm(0.975)*MSE_se,
      MSE_ub      = MSE + qnorm(0.975)*MSE_se,
      
      # Root Mean Squared Error 
      RMSE        = sqrt(MSE),
      RMSE_lb     = sqrt(MSE_lb),
      RMSE_ub     = sqrt(MSE_ub),
      
      mae         = mean(abs(difference)),
      coverage    = length(which(ci_correct))/length(ci_correct),
      coverage_se = sqrt((coverage*(1-coverage))/n),
      coverage_lb = coverage - 1.96*coverage_se,
      coverage_ub = coverage + 1.96*coverage_se,
      mean_ci_length = mean(ci_length),
      perc_diag_divergences_binary = sum(diag_divergences_binary)/n,
      perc_diag_ebfmi_binary       = sum(diag_ebfmi_binary)/n,
      mean_testretest_cor = mean(testretest_cor)
    )  %>%
    ungroup()
  

comparison_statistics = results_table_cleaned_2 %>%
    select(
      name, 
      sample_sizes, sens_sigma, mean,
      n_items, bias, EmpSE, MSE, n, RMSE
    ) %>%
    pivot_wider(
      names_from = name,
      values_from = c(mean, bias, EmpSE, MSE, RMSE)
      # values_from = everything()
    ) %>%
    mutate(
      relative_precision  = 100*((EmpSE_sh^2 / EmpSE_rmp^2 ) - 1),
      relative_empse      = 100*((EmpSE_sh   / EmpSE_rmp ) - 1),
      relative_mse        = 100*((MSE_sh     / MSE_rmp ) - 1),
      relative_rmse       = 100*((RMSE_sh    / RMSE_rmp ) - 1)
    ) 
  
  comparison_statistics%>%
    print(width = Inf, n = Inf)
    
  
results_table_cleaned %>%
  ggplot(
    aes( x = mean, y = population_reliability_ls )
  ) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  coord_fixed()

# Check for any identical seeds ------------------------------------------------

n_check = 100
n_results <- length(results)
matches <- matrix(FALSE, nrow=n_check, ncol=n_results)

for(i in 1:n_check) {
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


table(duplicated(results_table$rmp_est))

table(duplicated(results_table$sh_est))


# Plots ------------------------------------------------------------------------


##### Using raw data -----------------------------------------------------------

library(grid)
# 
n_items_labels <- c(
  "20" = "20 Trials",
  "40" = "40 Trials",
  "80" = "80 Trials"
)

sens_sigma_labels <- c(
  "0"   = "Sensitivity SD = 0.00",
  "0.2" = "Sensitivity SD = 0.20",
  "0.4" = "Sensitivity SD = 0.40"
)

results_table_cleaned2 = results_table_cleaned %>%
  filter(name == "rmp")

results_table_long  %>%
  filter(name == "rmp") %>% 
  group_by(n_items, sens_sigma, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
    mutate(
    mean_true_score_model_score_cor2 = mean(ascots),
    sample_sizes = factor(sample_sizes),
    # est = rmp_est
  ) %>%
  ggplot(
    aes(
      y = est, 
      x = sample_sizes
      )
    ) +
  geom_violin(
    width = .95, 
    fill = "grey80",
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun = mean,
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
      y  = pop_rel,
      x = factor(sample_sizes)
    ),
    # alpha = .6,
    shape = 1,
    size = 2,
    col = "red",
    stroke = .9
  ) +
  # annotate(
  #   "rect", 
  #   xmin = 0, 
  #   xmax = 4,  # Adjust these values to cover just the text area
  #   ymin = .63, 
  #   ymax = .91,  # Adjust based on your text position
  #   fill = "white", 
  #   alpha = 0.7
  # ) +
  geom_rect(
    data = results_table_cleaned2,
    inherit.aes = FALSE,
    aes(
      xmin = .5,
      xmax = 3.5,
      # Position the rectangle differently based on conditions
      ymin = ifelse(sens_sigma == 0.4 & n_items >= 80, -.01, 0.66),
      ymax = ifelse(sens_sigma == 0.4 & n_items >= 80, 0.24, 0.90)
    ),
    fill = "white",
    alpha = 0.4
  ) +
  geom_text(
    data = results_table_cleaned2,
    aes(
      y = ifelse(sens_sigma == 0.4 & n_items >= 80, 0, 0.89),
      # y=.8,
      x = factor(sample_sizes),
      label = paste0(
        "bias = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", bias)), "\n",
        "RMSE = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.3f", sqrt(MSE))), "\n",
        "cov = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", coverage)), "\n",
        "EmpSE = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.2f", EmpSE)), "\n",
        "N = ", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.f", n))
      )
    ),
    vjust = ifelse(results_table_cleaned2$sens_sigma == 0.4 &
                     results_table_cleaned2$n_items >= 80, 0, 1),
    hjust = 1,
    size = 1.8,
    position = position_nudge(x=.42),
    col = "grey20"
  ) +
  facet_grid(
    cols = vars(n_items),
    rows = vars(sens_sigma),
    labeller = labeller(
      n_items = n_items_labels,
      sens_sigma = sens_sigma_labels
    )
  ) +
  labs(y = "Sample reliability estimate", x = "Simulation Sample Size") +
  guides(col=guide_legend(title="Estimator")) +
  ggplot2::theme_bw() +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom")
  ) + 
  coord_cartesian(ylim=c(0,.87))

ggsave(file.path("plots","3_study2_violinplot.png"), width = 6.2, height = 7)
ggsave(file.path("plots","3_study2_violinplot.pdf"), width = 6.2, height = 7)

##### Comparison to split half -------------------------------------------------
# devtools::install_github("psyteachr/introdataviz")
# library(introdataviz)

source("https://raw.githubusercontent.com/PsyTeachR/introdataviz/7763afad2cea8fd9fa98acf4e389071cad61e758/R/splitviolin.R")

results_table_long  %>%
  group_by(n_items, sens_sigma, sample_sizes) %>%       # aggregating over sample-sizes & sens_mean
  mutate(
    mean_true_score_model_score_cor2 = mean(ascots),
    sample_sizes = factor(sample_sizes),
    name = factor(name, levels = c("rmp", "sh"), labels = c("RMU", "Split-Half"))
  ) %>%
  ungroup() %>%
  arrange(name) %>%
  ggplot(
    aes(
      y = est, 
      x = sample_sizes,
      fill = name
      )
  ) +
  geom_split_violin( # sourced from above link!
    # aes(fill = name),
    width = .95, 
    # fill = "grey80",
    scale = "width",
    trim = TRUE, # If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
    # position = position_identity() 
  ) +
  annotate("rect", 
           xmin = .5, xmax = 3.5,  # Adjust these values to cover just the text area
           ymin = -0.65, ymax = -1.24,  # Adjust based on your text position
           fill = "white", alpha = 0.7) +
  geom_text(
    data = comparison_statistics %>% filter(sample_sizes==250),
    inherit.aes = FALSE,
    aes(
      y = ifelse(sens_sigma > 0 , -.7, -.7),
      # y=.8,
      x = factor(sample_sizes),
      label = paste0(
        "Relative Empirical Standard Error \n\n", 
        "Relative Root Mean Squared Error \n\n"
      )
    ),
    # vjust = ifelse(results_table_cleaned2[results_table_cleaned2$sample_sizes==500,]$sens_sigma > 0, 0, 1),
    vjust =1,
    hjust = .5,
    size = 2.2,
    # position = position_nudge(x=0),
    col = "grey20"
  ) +
  geom_text(
    data = comparison_statistics,
    inherit.aes = FALSE,
    aes(
      y = ifelse(sens_sigma > 0 , -.7, -.7),
      # y=.8,
      x = factor(sample_sizes),
      label = paste0(
        "\n", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.1f", relative_empse)), "%\n",
        "\n", gsub("^(-?)0\\.", "\\1\\.", sprintf("%.1f", relative_rmse)), "%\n"
      )
    ),
    vjust = ifelse(results_table_cleaned2$sens_sigma > 0, 1, 1),
    hjust = .5,
    size = 2.2,
    # position = position_nudge(x=0),
    col = "grey20"
  ) +
  facet_grid(
    cols = vars(n_items),
    rows = vars(sens_sigma),
    labeller = labeller(
      n_items = n_items_labels,
      sens_sigma = sens_sigma_labels
    )
  ) +
  labs(
    y = "Sample reliability estimate", 
    x = "Simulation Sample Size"
    ) +
  guides(
    fill=guide_legend(title="Estimator")
    ) +
  ggplot2::theme_bw() +
  theme(
    legend.position = "none",
    # legend.position = c(.95, .05),
    # legend.justification = c("right", "bottom")
  ) + 
  coord_cartesian(ylim = c(-1.2,.9))

ggsave(file.path("plots","3_study2_violinplot_comparison.png"), width = 6.2, height = 7)
ggsave(file.path("plots","3_study2_violinplot_comparison.pdf"), width = 6.2, height = 7)



# DEPRECIATED ------------------------------------------------------------------

