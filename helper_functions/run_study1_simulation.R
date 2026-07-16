run_study1_simulation = function(
    i,
    n,
    loadings,
    h_ci_calc = TRUE,
    additional_tests = FALSE,
    use_init = TRUE
){
  # browser()
  out = list()
  
  out[["settings"]] = list()
  out[["settings"]][["seed"]]     =  .Random.seed
  out[["settings"]][["n"]]        =  n
  out[["settings"]][["loadings"]] =  loadings
  n_items = length(loadings)
  
  error_variances = 1^2 - loadings^2
  weights         = loadings/error_variances
  intercepts = rnorm(n_items, mean = 0,  sd = 0)  # Set intercepts to 0!
  
  out[["settings"]][["intercepts"]] =  intercepts
  
  # creates test-retest data
  dat_t1t2 = sim_factor_stnd(
    debug       = T,
    n_rows      = n,
    std_loading = loadings,
    intercepts  = intercepts
  )
  
  dat_t1 = dat_t1t2[[1]]
  dat_t2 = dat_t1t2[[2]]
  
  dat_t1_scale = scale(dat_t1, center = TRUE, scale = TRUE) %>%
    data.frame()
  dat_t2_scale = scale(dat_t2, center = TRUE, scale = TRUE) %>%
    data.frame()
  
  dat_t1_long = dat_t1_scale %>%
    dplyr::select(-true_scores, -mean) %>%
    tibble::rowid_to_column(., var = "pps") %>%
    tidyr::pivot_longer(cols = starts_with("X")) %>%
    mutate(name = factor(name))
  
  dat_t2_long = dat_t2_scale %>%
    dplyr::select(-true_scores, -mean) %>%
    tibble::rowid_to_column(., var = "pps") %>%
    tidyr::pivot_longer(cols = starts_with("X")) %>%
    mutate(name = factor(name))
  
  regression_factor_score         = as.matrix(dat_t1[,paste0("X",1:n_items)]) %*% weights                 # See P3 : DOI: 10.1348/000711008X365676
  out[["true_score_cor"]]         = cor.test(regression_factor_score,dat_t1$true_scores)     
  out[["population_reliability"]] = sqrt(sum(loadings^2/error_variances)/(1+sum(loadings^2/error_variances)))^2 # See P3 : DOI: 10.1348/000711008X365676
  
  out[["alpha_reliability_t1"]] <- tryCatch({
    MBESS::ci.reliability(data=dplyr::select(dat_t1_scale, -true_scores, -mean), type = "alpha", interval.type = "ml")
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  out[["alpha_reliability_t2"]] <- tryCatch({
    MBESS::ci.reliability(data=dplyr::select(dat_t2_scale, -true_scores, -mean), type = "alpha", interval.type = "ml")
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  
  out[["h_reliability_t1"]] <- tryCatch({
    coef_h(data=dplyr::select(dat_t1_scale, -true_scores, -mean), ci_calc = h_ci_calc)
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  out[["h_reliability_t2"]] <- tryCatch({
    coef_h(data=dplyr::select(dat_t2_scale, -true_scores, -mean), ci_calc = h_ci_calc)
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  
  
  
  # Fit Model 
  
  dat_stan_t1 = list(
    lambda_sd_prior_sd = .05,
    lambda_mean_prior_sd = 0.90,
    n      = nrow(dat_t1_long),
    pps_n  = length(unique(dat_t1_long$pps)),
    item_n = length(unique(dat_t1_long$name)),
    item   = match(dat_t1_long$name, unique(dat_t1_long$name)),
    pps    = dat_t1_long$pps,
    y      = dat_t1_long$value,
    prior_PPC = 0
  )
  
  dat_stan_t2 = list(
    lambda_sd_prior_sd = .05,
    lambda_mean_prior_sd = 0.90,
    n      = nrow(dat_t2_long),
    pps_n  = length(unique(dat_t2_long$pps)),
    item_n = length(unique(dat_t2_long$name)),
    item   = match(dat_t2_long$name, unique(dat_t2_long$name)),
    pps    = dat_t2_long$pps,
    y      = dat_t2_long$value,
    prior_PPC = 0
  )
  
  if (use_init){                                                                # Initialization has a HUGE impact on small sample performance! 
    init_fun <- function() list(
      theta = rnorm(nrow(dat_t1), 0, 0.1),
      # lambda = rnorm(length(l),.5,0),
      lambda_raw = rnorm(length(loadings), .1, 0),
      sigma_add = rnorm(length(loadings), .01, 0),
      sigma     = rep(1, length(loadings)),
      lambda_sd = rnorm(1, .1, 0),
      lambda_mean = rnorm(1, 0, 0)
      # lambda_raw_1 = rnorm(1, .1, 0),
      # lambda_raw_rest = rnorm(length(loadings)-1,0,0)
    )
  }
  
  internal_results_t1 = mod$sample(
    init = switch(as.numeric(use_init)+1,NULL, init_fun),                       # ifeslse can't return NULL
    data = dat_stan_t1,
    seed = 123,
    chains = 2,
    parallel_chains = 1,
    refresh = 500, # print update every 500 iters
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = .98
  )
  
  internal_results_t2 = mod$sample(
    init = switch(as.numeric(use_init)+1,NULL, init_fun),                       # ifeslse can't return NULL
    data = dat_stan_t2,
    seed = 123,
    chains = 2,
    parallel_chains = 1,
    refresh = 500, # print update every 500 iters
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = .98
  )
  
  # Calculate coefficient H using posterior draws of mcmc model 
  loadings_df_t1 = data.frame(internal_results_t1$summary("lambda"))
  loadings_df_t2 = data.frame(internal_results_t2$summary("lambda"))
  
  # l_t1 = out[["mcmc_loadings"]] = loadings_df_t1$mean
  # l_t2 = out[["mcmc_loadings"]] = loadings_df_t2$mean
  
  out[["mcmc_coefh_t1"]] = internal_results_t1$draws(variables = "mcmc_coef_h", format = "data.frame") %>% ggdist::mean_hdci()
  out[["mcmc_coefh_t2"]] = internal_results_t2$draws(variables = "mcmc_coef_h", format = "data.frame") %>% ggdist::mean_hdci()
  
  # Diagnostics using cmdstanr
  diagnostics_t1 = internal_results_t1$diagnostic_summary()
  diagnostics_t2 = internal_results_t2$diagnostic_summary()
  
  # Extract divergences
  out[["diag_divergences_t1"]]        = sum(diagnostics_t1$num_divergent)
  out[["diag_divergences_t2"]]        = sum(diagnostics_t2$num_divergent)
  
  # Check HMC diagnostics using cmdstanr
  out[["diagnostics_divergences_t1"]] = sum(diagnostics_t1$num_divergent)
  out[["diagnostics_divergences_t2"]] = sum(diagnostics_t2$num_divergent)
  out[["diagnostics_treedepth_t1"]]   = sum(diagnostics_t1$num_max_treedepth)
  out[["diagnostics_treedepth_t2"]]   = sum(diagnostics_t2$num_max_treedepth)
  out[["diagnostics_ebfmi_t1"]]       = diagnostics_t1$ebfmi
  out[["diagnostics_ebfmi_t2"]]       = diagnostics_t2$ebfmi

  calc_rmu_t1 = internal_results_t1$draws("theta", format = "matrix") |>
    t() |>
    gbtoolbox::reliability()

  calc_rmu_t2 = internal_results_t2$draws("theta", format = "matrix") |>
    t() |>
    gbtoolbox::reliability()
  
  out[["rmu_est_t1"]] = calc_rmu_t1$hdci
  out[["rmu_est_t2"]] = calc_rmu_t2$hdci
  
  
  scores_t1 = internal_results_t1 %>%                                 # posteriors for theta for each subject
    tidybayes::spread_draws(theta[pps]) %>%
    group_by(pps) %>%
    summarise(theta_score = ggdist::mean_hdci(theta)) %>%
    data.frame()
  scores_t2 = internal_results_t2 %>%
    tidybayes::spread_draws(theta[pps]) %>%
    group_by(pps) %>%
    summarise(theta_score = ggdist::mean_hdci(theta)) %>%
    data.frame()
  
  out[["test_retest_reliability"]] = cor(scores_t1$theta_score$y, scores_t2$theta_score$y)
  
  # browser()

  if (additional_tests==TRUE){
    
    ci_contain_true_score_t1 = as.numeric((dat_t1$true_scores > scores_t1$theta_score$ymin) & (dat_t1$true_scores < scores_t1$theta_score$ymax))
    ci_contain_true_score_t2 = as.numeric((dat_t2$true_scores > scores_t2$theta_score$ymin) & (dat_t2$true_scores < scores_t2$theta_score$ymax))
    
    
    out[["true_score_coverage_t1"]] = length(which(ci_contain_true_score_t1==1))/length(ci_contain_true_score_t1)
    out[["true_score_coverage_t2"]] = length(which(ci_contain_true_score_t2==1))/length(ci_contain_true_score_t2)
    
    
    out[["true_score_model_score_cor_t1"]]  = cor.test(dat_t1$true_scores, scores_t1$theta_score$y)   
    out[["true_score_model_score_cor_t2"]]  = cor.test(dat_t2$true_scores, scores_t2$theta_score$y)   # note that dat_t1$true_scores == dat_t2$true_scores
    
    # factor_score = psych::fa(dplyr::select(dat_scale, -true_scores, -mean), nfactors = 1)
    # out[["true_score_factor_score_cor"]] = cor.test(dat$true_scores, factor_score$scores)
    
  }
  
  if ((i %% 5)==0){
    write.csv(data.frame(y=""), file.path("progress_new",paste0(i,".ignore")))
  }
  
  return(out)
}


