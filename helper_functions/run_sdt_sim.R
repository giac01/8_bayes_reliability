# rm(list =ls())
  # source(file.path("helper_functions","sim_sdt.R"))
  # source(file.path("helper_functions","calc_r_brms.R"))
  #
  # is_even <- function(x) {
  #   return(x %% 2 == 0)
  # }

  run_sdt_sim = function(
    i,
    sens_mean,
    sens_sigma,
    k_mean,
    k_sigma,
    n_items,
    n_pps,
    retest    = TRUE,
    splithalf = TRUE,
    n_trials_old = NULL,
    n_trials_new = NULL,
    save_results = FALSE,
    b_prior = NULL,
    sd_prior = NULL
  ){
  # browser()
  results = list()
  
  results[["settings"]] = list()
  results[["settings"]][["sens_mean"]]  = sens_mean
  results[["settings"]][["sens_sigma"]] = sens_sigma
  results[["settings"]][["k_mean"]]     = k_mean
  results[["settings"]][["k_sigma"]]    = k_sigma
  results[["settings"]][["n_items"]]    = n_items
  
  results[["settings"]][["n_pps"]]      = n_pps
  results[["settings"]][["retest"]]     = retest
  results[["settings"]][["splithalf"]]  = splithalf
  results[["settings"]][["n_trials_old"]] = n_trials_old
  results[["settings"]][["n_trials_new"]] = n_trials_new
  results[["settings"]][["save_results"]] = save_results
  results[["settings"]][["b_prior"]]      = b_prior
  results[["settings"]][["sd_prior"]]     = sd_prior
  
  
  if(is.null(n_trials_old)) n_trials_old = n_items
  if(is.null(n_trials_new)) n_trials_new = n_items
  
  sens_pps_direct = rnorm(n_pps, mean = sens_mean, sd = sens_sigma)
  k_pps_direct    = rnorm(n_pps, mean = k_mean,    sd = k_sigma)

  dat = sim_sdt_binomial(
    sens_pps_direct = sens_pps_direct,
    k_pps_direct    = k_pps_direct,
    n_trials_old = n_trials_old,
    n_trials_new = n_trials_new,
    n_pps        = n_pps
  )

  if (retest == TRUE) {
  dat_testretest = sim_sdt_binomial(
    sens_pps_direct = sens_pps_direct,
    k_pps_direct    = k_pps_direct,
    n_trials_old = n_trials_old,
    n_trials_new = n_trials_new,
    n_pps        = n_pps
  )
  }

  if (splithalf == TRUE){
    dat_splithalf = list()
    dat_splithalf[[1]] = sim_sdt_binomial(
      sens_pps_direct = sens_pps_direct,
      k_pps_direct    = k_pps_direct,
      n_trials_old = round(n_trials_old/2),
      n_trials_new = round(n_trials_new/2),
      n_pps        = n_pps
    )
    dat_splithalf[[1]]$data$session = "session 1"

    dat_splithalf[[2]] = sim_sdt_binomial(
      sens_pps_direct = sens_pps_direct,
      k_pps_direct    = k_pps_direct,
      n_trials_old = round(n_trials_old/2),
      n_trials_new = round(n_trials_new/2),
      n_pps        = n_pps
    )
    dat_splithalf[[2]]$data$session = "session 2"
    dat_splithalf = bind_rows(dat_splithalf[[1]]$data,dat_splithalf[[2]]$data)
  }

  if (FALSE){
  # hite rate
    k = -2
    1 - pnorm(k, mean = +sens_mean/2, sd = 1)
    1 - pnorm(k, mean = -sens_mean/2, sd = 1)
  }
  
  # SPLIT HALF CALCULATION WITH SIMPLE D-PRIME CALC

  if(splithalf == TRUE){
    dat_splithalf_wider =
    dat_splithalf %>%
      mutate(cond = ifelse(cond==0.5, "old", "new")) %>%
      pivot_wider(id_cols = c(pps, session), names_from = cond, values_from = c(y,n_trials)) %>%
      mutate(
        y_old = ifelse(y_old == n_trials_old, n_trials_old - 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == n_trials_new, n_trials_new - 1, y_new),
        y_old = ifelse(y_old == 0, 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == 0, 1, y_new),
             ) %>%
      mutate(d_prime = qnorm(y_old/n_trials_old) - qnorm(y_new/n_trials_new)) %>%
      pivot_wider(values_from = d_prime, names_from = session, id_cols = pps)

    results[["splithalf_cor"]] = cor.test(dat_splithalf_wider$`session 1`, dat_splithalf_wider$`session 2`)

    results[["splithalf_a"]] = MBESS::ci.reliability(
      data.frame(dat_splithalf_wider[,2:3]),
      type = "alpha",
      interval.type = "ml"
    )
  }
  
  # TEST-RETEST ANALYSIS WITH SIMPLE D-PRIME CALC

  if(retest == TRUE){

    dat$data$session = "session 1"
    dat_testretest$data$session = "session 2"

    dat_testrest_combined = bind_rows(dat$data, dat_testretest$data) %>%
      mutate(cond = ifelse(cond==0.5, "old", "new")) %>%
      pivot_wider(id_cols = c(pps, session), names_from = cond, values_from = c(y,n_trials)) %>%
      mutate(
        y_old = ifelse(y_old == n_trials_old, n_trials_old - 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == n_trials_new, n_trials_new - 1, y_new),
        y_old = ifelse(y_old == 0, 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == 0, 1, y_new),
      ) %>%
      mutate(d_prime = qnorm(y_old/n_trials_old) - qnorm(y_new/n_trials_new)) %>%
      pivot_wider(values_from = d_prime, names_from = session, id_cols = pps)

    results[["testretest_cor"]] = cor.test(dat_splithalf_wider$`session 1`, dat_splithalf_wider$`session 2`)
    
  }

  # Fit BRMS model 
  
  if (!is.null(b_prior) & !is.null(sd_prior)){
    print("using prior model")
    my_prior = c(
      set_prior(paste0("constant(",-1*k_mean,")"), class = "Intercept"),
      set_prior(paste0("constant(",k_sigma,")"), class = "sd", coef = "Intercept", group = "pps"),
      
      set_prior(paste0("constant(",b_prior,")"), class = "b",  coef = "cond"),
      set_prior(paste0("constant(",sd_prior,")"), class = "sd", coef = "cond", group = "pps")
      
    )
    internal_results = update(
      # chains      = 2,
      # cores       = 1,
      object      = sim_model,
      newdata     = dat$data,
      recompile   = TRUE,
      # control = list(adapt_delta = 0.9),
      prior = my_prior
    )
  }
  
  if (is.null(b_prior) | is.null(sd_prior)){
    internal_results = update(
      # chains      = 2,
      # cores       = 1,
      object      = sim_model,
      newdata     = dat$data,
      recompile   = FALSE,
      # control = list(adapt_delta = 0.9)
      # prior = brms::prior_string(ifelse((!is.null(b_prior) & !is.null(sd_prior)), (my_prior), NULL)[[1]])
    )
  }

  # tidybayes::get_variables(internal_results)

  mod0_sens_estimates =
    internal_results %>%
    tidybayes::spread_draws(r_pps[pps, beta]) %>%
    filter(beta == "cond") %>%                           # Since there's no intercept this shouldn't haveany effect
    group_by(pps, beta) %>%
    summarise(mean = mean(r_pps)) %>%
    pivot_wider(
      id_cols = pps,
      names_from = beta,
      values_from = mean
    )

  results[["cor_bayes_estimate_true"]] = cor.test(mod0_sens_estimates$cond, dat$sens_pps)
  
  rmp_calc = calc_r_brms_sdt(internal_results)
  
  results[["rmp"]] = rmp_calc$hdci
  results[["rmp_pd"]] = rmp_calc$pd
  
  results[["mean(sens_pps)"]] = mean(dat$sens_pps)
  
  if (save_results==TRUE){
    results[["brms_results"]] = internal_results
  }

  if (internal_results$algorithm != "meanfield" & internal_results$algorithm != "fullrank" ){
    # Check HMC diagnostics using cmdstanr
    results[["diagnostics_divergences"]]       = sum(as.numeric(rstan::get_divergent_iterations(internal_results$fit)))
    results[["diagnostics_treedepth"]]         = rstan::get_num_max_treedepth(internal_results$fit)
    results[["diagnostics_low_bfmi_chains"]]   = rstan::get_low_bfmi_chains(internal_results$fit)
    
  }
  
  if ((i %% 10)==0){
    write.csv(data.frame(y=""), file.path("progress_sdt",paste0(i,".ignore")))
  }

  return(results)
  }


