run_factor_sim_2 = function(
    i,
    n,
    n_items,
    loadings,
    h_ci_calc = TRUE,
    additional_tests = FALSE
){
  # browser()
  out = list()
  
  out[["settings"]] = list()
  out[["settings"]][["seed"]]     =  .Random.seed
  out[["settings"]][["n"]]        =  n
  out[["settings"]][["n_items"]]  =  n_items
  out[["settings"]][["loadings"]] =  loadings
  
  error_variances = 1^2 - loadings^2
  weights         = loadings/error_variances
  intercepts = rnorm(n_items, mean = 0,  sd = 1)
  
  out[["settings"]][["intercepts"]] =  intercepts
  
  dat = sim_factor_stnd(
    debug       = T,
    n_rows      = n,
    n_items     = n_items,
    std_loading = loadings,
    intercepts  = intercepts
  )
  
  dat_scale = scale(dat, center = TRUE, scale = TRUE) %>%
    data.frame()
  
  dat_long = dat_scale %>%
    dplyr::select(-true_scores, -mean) %>%
    tibble::rowid_to_column(., var = "pps") %>%
    tidyr::pivot_longer(cols = starts_with("X")) %>%
    mutate(name = factor(name))
  
  
  out[["regression_factor_score"]] =  as.matrix(dat[,paste0("X",1:n_items)]) %*% weights                 # See P3 : DOI: 10.1348/000711008X365676
  out[["true_score_cor"]]  = cor(out[["regression_factor_score"]],dat$true_scores)^2     
  out[["population_reliability"]]     = sqrt(sum(loadings^2/error_variances)/(1+sum(loadings^2/error_variances)))^2 # See P3 : DOI: 10.1348/000711008X365676
  
  out[["alpha_reliability"]] <- tryCatch({
    MBESS::ci.reliability(data=dplyr::select(dat_scale, -true_scores, -mean), type = "alpha", interval.type = "ml")
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  
  out[["omega_reliability"]] <- tryCatch({
    MBESS::ci.reliability(data=dplyr::select(dat_scale, -true_scores, -mean), type = "omega", interval.type = "mlr")
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  
  out[["h_reliability"]] <- tryCatch({
    coef_h(data=dplyr::select(dat_scale, -true_scores, -mean), ci_calc = h_ci_calc)
  }, error = function(e) {
    NULL  # Return NULL in case of an error without showing a message
  })
  
  # Fit Model 
  
  dat_stan = list(
    n = nrow(dat_long),
    pps_n = length(unique(dat_long$pps)),
    item_n = length(unique(dat_long$name)),
    item   = match(dat_long$name, unique(dat_long$name)),
    pps    = dat_long$pps,
    y = dat_long$value
  )
  
  internal_results = mod$sample(
    data = dat_stan,
    seed = 123,
    chains = 2,
    parallel_chains = 1,
    refresh = 500, # print update every 500 iters
    iter_warmup = 1500,
    iter_sampling = 3000,
    adapt_delta = .95
  )
  # Diagnostics using cmdstanr
  diagnostics = internal_results$diagnostic_summary()
  
  # Extract divergences
  out[["diag_divergences"]] = sum(diagnostics$num_divergent)
  
  # Check HMC diagnostics using cmdstanr
  out[["diagnostics_divergences"]] = sum(diagnostics$num_divergent)
  out[["diagnostics_treedepth"]] =  sum(diagnostics$num_max_treedepth)
  out[["diagnostics_ebfmi"]] = diagnostics$ebfmi

  # out[["model_results"]] = 
  out[["r_est"]] = calc_r_stan_m2(internal_results)
  
  if (additional_tests==TRUE){
    
    theta_scores = 
    internal_results %>%
      tidybayes::spread_draws(theta[pps]) %>%
      group_by(pps) %>%
      summarise(theta_score = ggdist::mean_hdci(theta)) %>% 
      data.frame()
    
    theta_scores = theta_scores$theta_score
    
    theta_scores$theta_pop = dat$true_scores
    
    theta_scores$ci_contain_true_score = as.numeric((theta_scores$theta_pop > theta_scores$ymin) & (theta_scores$theta_pop< theta_scores$ymax))
    
    out[["true_score_coverage"]] = length(which(theta_scores$ci_contain_true_score==1))/length(theta_scores$ci_contain_true_score)
    
    out[["true_score_model_score_cor2"]]  = cor(dat$true_scores, theta_scores$y)   # TODO - ADD CI here, and remove "2"
    
    factor_score = psych::fa(dplyr::select(dat_scale, -true_scores, -mean), nfactors = 1)
    
    out[["true_score_factor_score_cor2"]] = cor(dat$true_scores, factor_score$scores) # TODO - ADD CI here, and remove "2"
    
  }
  
  if ((i %% 5)==0){
    write.csv(data.frame(y=""), file.path("progress_new",paste0(i,".ignore")))
  }
  
  return(out)
}


