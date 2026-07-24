# rm(list =ls())
  # source(file.path("helper_functions","sim_sdt.R"))
  # source(file.path("helper_functions","calc_r_brms.R"))
  #
  # is_even <- function(x) {
  #   return(x %% 2 == 0)
  # }

  run_study2_simulation = function(
    i,
    sens_mean,
    sens_sigma,
    k_mean,
    k_sigma,
    n_items,
    n_pps,
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
  results[["settings"]][["seed"]]     =  .Random.seed # This was recently added!
  results[["settings"]][["sens_mean"]]  = sens_mean
  results[["settings"]][["sens_sigma"]] = sens_sigma
  results[["settings"]][["k_mean"]]     = k_mean
  results[["settings"]][["k_sigma"]]    = k_sigma
  results[["settings"]][["n_items"]]    = n_items

  results[["settings"]][["n_pps"]]      = n_pps
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

  # creates test-retest data: same underlying sensitivity/criterion per subject,
  # independent trial-level (binomial) outcomes at each occasion
  dat_t1 = sim_sdt_binomial(
    sens_pps_direct = sens_pps_direct,
    k_pps_direct    = k_pps_direct,
    n_trials_old = n_trials_old,
    n_trials_new = n_trials_new,
    n_pps        = n_pps
  )
  dat_t1$data$time = "t1"


  dat_t2 = sim_sdt_binomial(
    sens_pps_direct = sens_pps_direct,
    k_pps_direct    = k_pps_direct,
    n_trials_old = n_trials_old,
    n_trials_new = n_trials_new,
    n_pps        = n_pps
  )
  dat_t2$data$time = "t2"

  sim_split_halves = function(split1_label, split2_label){
    dat_split = list()
    dat_split[[1]] = sim_sdt_binomial(
      sens_pps_direct = sens_pps_direct,
      k_pps_direct    = k_pps_direct,
      n_trials_old = round(n_trials_old/2),
      n_trials_new = round(n_trials_new/2),
      n_pps        = n_pps
    )
    dat_split[[1]]$data$split = split1_label

    dat_split[[2]] = sim_sdt_binomial(
      sens_pps_direct = sens_pps_direct,
      k_pps_direct    = k_pps_direct,
      n_trials_old = round(n_trials_old/2),
      n_trials_new = round(n_trials_new/2),
      n_pps        = n_pps
    )
    dat_split[[2]]$data$split = split2_label

    bind_rows(dat_split[[1]]$data, dat_split[[2]]$data)
  }

  if (splithalf == TRUE){
    dat_time1_split = sim_split_halves("time1split1", "time1split2")
    dat_time2_split = sim_split_halves("time2split1", "time2split2")
  }

  if (FALSE){
  # hite rate
    k = -2
    1 - pnorm(k, mean = +sens_mean/2, sd = 1)
    1 - pnorm(k, mean = -sens_mean/2, sd = 1)
  }

  # SPLIT HALF CALCULATION WITH SIMPLE D-PRIME CALC (computed separately at t1 and t2)

  calc_splithalf = function(dat_split, split1_label, split2_label){
    dat_split_wide =
      dat_split %>%
      mutate(cond = ifelse(cond==0.5, "old", "new")) %>%
      pivot_wider(id_cols = c(pps, split), names_from = cond, values_from = c(y,n_trials)) %>%
      mutate(
        y_old = ifelse(y_old == n_trials_old, n_trials_old - 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == n_trials_new, n_trials_new - 1, y_new),
        y_old = ifelse(y_old == 0, 1, y_old),                            # Continuity Correction
        y_new = ifelse(y_new == 0, 1, y_new),
             ) %>%
      mutate(d_prime = qnorm(y_old/n_trials_old) - qnorm(y_new/n_trials_new)) %>%
      pivot_wider(values_from = d_prime, names_from = split, id_cols = pps)

    list(
      cor   = cor.test(dat_split_wide[[split1_label]], dat_split_wide[[split2_label]]),
      alpha = MBESS::ci.reliability(
        data.frame(dat_split_wide[,c(split1_label, split2_label)]),
        type = "alpha",
        interval.type = "ml"
      )
    )
  }

  if(splithalf == TRUE){
    splithalf_t1 = calc_splithalf(dat_time1_split, "time1split1", "time1split2")
    splithalf_t2 = calc_splithalf(dat_time2_split, "time2split1", "time2split2")

    results[["splithalf_cor_t1"]] = splithalf_t1$cor
    results[["splithalf_a_t1"]]   = splithalf_t1$alpha
    results[["splithalf_cor_t2"]] = splithalf_t2$cor
    results[["splithalf_a_t2"]]   = splithalf_t2$alpha
  }

  # TEST-RETEST ANALYSIS WITH SIMPLE D-PRIME CALC (classical benchmark, using observed
  # hit/false-alarm rates rather than the Bayesian model)

  dat_retest_wide = bind_rows(dat_t1$data, dat_t2$data) %>%
    mutate(cond = ifelse(cond==0.5, "old", "new")) %>%
    pivot_wider(id_cols = c(pps, time), names_from = cond, values_from = c(y,n_trials)) %>%
    mutate(
      y_old = ifelse(y_old == n_trials_old, n_trials_old - 1, y_old),                            # Continuity Correction
      y_new = ifelse(y_new == n_trials_new, n_trials_new - 1, y_new),
      y_old = ifelse(y_old == 0, 1, y_old),                            # Continuity Correction
      y_new = ifelse(y_new == 0, 1, y_new),
    ) %>%
    mutate(d_prime = qnorm(y_old/n_trials_old) - qnorm(y_new/n_trials_new)) %>%
    pivot_wider(values_from = d_prime, names_from = time, id_cols = pps)

  results[["testretest_cor_dprime"]] = cor.test(dat_retest_wide$t1, dat_retest_wide$t2)

  # Fit BRMS model to t1 and t2 data separately

  fit_sdt_model = function(newdata){
    if (!is.null(b_prior) & !is.null(sd_prior)){
      print("using prior model")
      my_prior = c(
        set_prior(paste0("constant(",-1*k_mean,")"), class = "Intercept"),
        set_prior(paste0("constant(",k_sigma,")"), class = "sd", coef = "Intercept", group = "pps"),

        set_prior(paste0("constant(",b_prior,")"), class = "b",  coef = "cond"),
        set_prior(paste0("constant(",sd_prior,")"), class = "sd", coef = "cond", group = "pps")
      )
      update(
        object      = sim_model,
        newdata     = newdata,
        recompile   = TRUE,
        prior = my_prior
      )
    } else {
      update(
        object      = sim_model,
        newdata     = newdata,
        recompile   = FALSE
      )
    }
  }

  internal_results_t1 = fit_sdt_model(dat_t1$data)
  internal_results_t2 = fit_sdt_model(dat_t2$data)

  # tidybayes::get_variables(internal_results_t1)

  # Posterior draws of the sensitivity ("cond") random effect per subject, reused
  # below for both the point estimate and the RMU draws matrix
  sens_draws_t1 =
    internal_results_t1 %>%
    tidybayes::spread_draws(r_pps[pps, beta]) %>%
    filter(beta == "cond")                              # Since there's no intercept this shouldn't haveany effect

  sens_draws_t2 =
    internal_results_t2 %>%
    tidybayes::spread_draws(r_pps[pps, beta]) %>%
    filter(beta == "cond")

  mod0_sens_estimates_t1 = sens_draws_t1 %>% group_by(pps) %>% summarise(sens = mean(r_pps))
  mod0_sens_estimates_t2 = sens_draws_t2 %>% group_by(pps) %>% summarise(sens = mean(r_pps))

  results[["cor_bayes_estimate_true_t1"]] = cor.test(mod0_sens_estimates_t1$sens, dat_t1$sens_pps)
  results[["cor_bayes_estimate_true_t2"]] = cor.test(mod0_sens_estimates_t2$sens, dat_t2$sens_pps)

  # Calculate RMU reliability from posterior draws of the "cond" random effect, using gbtoolbox::reliability()

  draws_wide_t1 = sens_draws_t1 %>%
    pivot_wider(id_cols = pps, values_from = r_pps, names_from = .draw) %>%
    ungroup() %>%
    dplyr::select(-pps)

  draws_wide_t2 = sens_draws_t2 %>%
    pivot_wider(id_cols = pps, values_from = r_pps, names_from = .draw) %>%
    ungroup() %>%
    dplyr::select(-pps)

  rmu_t1 = gbtoolbox::reliability(draws_wide_t1)
  rmu_t2 = gbtoolbox::reliability(draws_wide_t2)

  results[["rmu_est_t1"]] = rmu_t1$hdci
  results[["rmu_est_t2"]] = rmu_t2$hdci

  # Test-retest reliability: correlation between the Bayesian sensitivity estimates at t1 and t2
  results[["test_retest_reliability"]] = cor(mod0_sens_estimates_t1$sens, mod0_sens_estimates_t2$sens)

  results[["mean(sens_pps_t1)"]] = mean(dat_t1$sens_pps)
  results[["mean(sens_pps_t2)"]] = mean(dat_t2$sens_pps)
  

  if (save_results==TRUE){
    results[["brms_results_t1"]] = internal_results_t1
    results[["brms_results_t2"]] = internal_results_t2
  }

  if (internal_results_t1$algorithm != "meanfield" & internal_results_t1$algorithm != "fullrank" ){
    # Check HMC diagnostics using cmdstanr
    results[["diagnostics_divergences_t1"]]     = sum(as.numeric(rstan::get_divergent_iterations(internal_results_t1$fit)))
    results[["diagnostics_treedepth_t1"]]       = rstan::get_num_max_treedepth(internal_results_t1$fit)
    results[["diagnostics_low_bfmi_chains_t1"]] = rstan::get_low_bfmi_chains(internal_results_t1$fit)
  }

  if (internal_results_t2$algorithm != "meanfield" & internal_results_t2$algorithm != "fullrank" ){
    results[["diagnostics_divergences_t2"]]     = sum(as.numeric(rstan::get_divergent_iterations(internal_results_t2$fit)))
    results[["diagnostics_treedepth_t2"]]       = rstan::get_num_max_treedepth(internal_results_t2$fit)
    results[["diagnostics_low_bfmi_chains_t2"]] = rstan::get_low_bfmi_chains(internal_results_t2$fit)
  }

  if ((i %% 10)==0){
    write.csv(data.frame(y=""), file.path("progress_sdt",paste0(i,".ignore")))
  }

  return(results)
  }

