run_study3_simulation = function(
    i,
    n_pps,
    n_trials,
    init_beliefs,
    learning_rate_mean,
    learning_rate_sd,
    decision_noise_mean,
    decision_noise_sd,
    prob_real,
    reward_outcome,
    save_results = FALSE,
    additional_tests = TRUE,
    init_stan = "normal",
    temp_save_file_name = NULL
){

  if (is.null(i)) stop("i is null")
  if (is.null(n_pps)) stop("n_pps is null")
  if (is.null(n_trials)) stop("n_trials is null")
  if (is.null(init_beliefs)) stop("init_beliefs is null")
  if (is.null(learning_rate_mean)) stop("learning_rate_mean is null")
  if (is.null(learning_rate_sd)) stop("learning_rate_sd is null")
  if (is.null(decision_noise_mean)) stop("decision_noise_mean is null")
  if (is.null(decision_noise_sd)) stop("decision_noise_sd is null")
  if (is.null(prob_real)) stop("prob_real is null")
  if (length(prob_real)!=1) stop("prob_real length should equal 1 (probability of outcome 2)")
  if (is.null(reward_outcome)) stop("reward_outcome is null")
  if (is.null(save_results)) stop("save_results is null")  # Though this has a default value and typically wouldn't be null.

  prob_real = c(1-prob_real, prob_real)

  # browser()
  results = list()

  results[["settings"]] = list()
  results[["settings"]][["seed"]]     =  .Random.seed
  results[["settings"]][["n_pps"]]               = n_pps
  results[["settings"]][["n_trials"]]            = n_trials
  results[["settings"]][["init_beliefs"]]        = init_beliefs
  results[["settings"]][["learning_rate_mean"]]  = learning_rate_mean
  results[["settings"]][["learning_rate_sd"]]    = learning_rate_sd
  results[["settings"]][["decision_noise_mean"]] = decision_noise_mean
  results[["settings"]][["decision_noise_sd"]]   = decision_noise_sd
  results[["settings"]][["prob_real"]]           = prob_real
  results[["settings"]][["save_results"]]        = save_results

  # Simulate test-retest data: t1 draws the underlying learning_rate/decision_noise for each
  # subject; t2 reuses those same underlying values (via learning_rate_direct/decision_noise_direct)
  # but generates an independent sequence of trial-by-trial choices/outcomes.

  dat_t1 = sim_ri(
    n_pps               = n_pps,
    n_trials            = n_trials,
    init_beliefs        = init_beliefs,
    learning_rate_mean  = learning_rate_mean,
    learning_rate_sd    = learning_rate_sd,
    decision_noise_mean = decision_noise_mean,
    decision_noise_sd   = decision_noise_sd,
    prob_real           = prob_real,
    reward_outcome      = reward_outcome,
    save_beliefs        = FALSE
  )

  dat_t2 = sim_ri(
    n_pps                  = n_pps,
    n_trials               = n_trials,
    init_beliefs           = init_beliefs,
    learning_rate_mean     = learning_rate_mean,
    learning_rate_sd       = learning_rate_sd,
    decision_noise_mean    = decision_noise_mean,
    decision_noise_sd      = decision_noise_sd,
    prob_real              = prob_real,
    reward_outcome         = reward_outcome,
    learning_rate_direct   = dat_t1$learning_rate,
    decision_noise_direct  = dat_t1$decision_noise,
    save_beliefs           = FALSE
  )

  if (init_stan == "beta"){
    init_values_muphi <- function() {
      list(
        A_pop_mu = .5,
        A_pop_phi = 1,
        tau_unscaled_mu = .5,
        tau_unscaled_phi= 1,
        A = rep(.5, n_pps),
        tau_unscaled = rep(1/5, n_pps)
      )
    }
  }

  if (init_stan == "normal"){
    init_values_muphi <- function() {
      list(
        mu = c(0,.5),
        sigma = c(1,1),

        learning_rate_z = rep(0, n_pps),
        decision_noise_z = rep(0, n_pps)
      )
    }
  }

  # Fit the stan model separately to each occasion, and pull out everything we need from that
  # occasion's fit. Kept as a closure (rather than a fully separate helper file) since it relies
  # on several variables from the enclosing call (n_pps, additional_tests, init_values_muphi, i).

  fit_occasion = function(dat){

    stan_data = list(
      N = nrow(dat$outcome),
      T = ncol(dat$outcome),
      Tsubj = apply(dat$outcome, 1, length),
      choice = dat$choice,
      outcome = dat$outcome
    )

    time_a = Sys.time()

    internal_results = mod$sample(
      data = stan_data,
      init = init_values_muphi,

      seed = 123,
      chains = 2,
      parallel_chains = 1,
      refresh = 500,
      iter_warmup = 1000,
      iter_sampling = 1000,
      adapt_delta = ifelse(n_pps<=100,.98, .95)
    )

    time_b = Sys.time()

    out = list()
    out[["fit_time"]] = as.numeric(difftime(time_b, time_a, units = "mins"))

    out[["model_exists"]] = (length(internal_results$output_files())!=0)
    # additional tests use up a lot of memory so need to be disabled for population calculations with large N_sim

    if (!out[["model_exists"]]) {
      out[["cor_bayes_estimate_true"]] = 9999 # This is to distinguish it from NAs which indicate 0 correlation!
      out[["rmp_est"]]      = NA
      out[["rmp_pd"]]       = NA
      out[["mean_learning_rate"]] = NA
      out[["point_estimates"]] = NULL
      return(out)
    }

    # Posterior draws for the learning rate, as a (iterations*chains) x n_pps matrix - used for
    # both the per-subject point estimate (cheap, always computed, feeds the ground-truth
    # correlation check below and the test-retest reliability estimand) and the full reliability
    # calculation.
    learning_rate_draws = internal_results$draws(variables = "A", format = "matrix")

    learning_rate_estimates = data.frame(
      pps                = 1:n_pps,
      y                  = base::colMeans(learning_rate_draws),
      true_learning_rate = dat$learning_rate
    )

    out[["point_estimates"]] = learning_rate_estimates

    # Reliability of the learning-rate estimates using the full posterior draws matrix
    rmp_calc =  learning_rate_draws %>%
                t() %>%
                gbtoolbox::reliability()

    out[["rmp_est"]] = rmp_calc$hdci
    out[["rmp_pd"]]  = rmp_calc$pd

    out[["cor_bayes_estimate_true"]] = cor.test(learning_rate_estimates$y, learning_rate_estimates$true_learning_rate)

    out[["mean_learning_rate"]] = mean(learning_rate_estimates$y)

    if (additional_tests == TRUE) {

      learning_rate_estimates_ci = internal_results$draws(variables = "A") %>%
                                posterior::as_draws_df() %>%
                                select(-.chain, -.iteration, -.draw)

      learning_rate_estimates_ci = learning_rate_estimates_ci %>%
                                `colnames<-`(c(1:n_pps)) %>%
                                pivot_longer(cols = everything(),
                                             names_to = "pps") %>%
                                mutate(pps = as.numeric(pps)) %>%
                                group_by(pps) %>%
                                summarise(ggdist::mean_hdci(value, .width = .95))

      learning_rate_estimates_ci$true_learning_rate = dat$learning_rate

      learning_rate_estimates_ci$ci_contain_true_score = as.numeric(
        (learning_rate_estimates_ci$true_learning_rate>=learning_rate_estimates_ci$ymin) &
        (learning_rate_estimates_ci$true_learning_rate<=learning_rate_estimates_ci$ymax)
      )

      out[["avg_true_score_coverage"]] = length(which(learning_rate_estimates_ci$ci_contain_true_score==1))/length(which(learning_rate_estimates_ci$ci_contain_true_score<2))

      out[["stan_results_summary"]] = internal_results$summary() %>%
        slice(which(!grepl("^A\\[",.$variable))) %>%
        slice(which(!grepl("^tau_unscaled\\[",.$variable))) %>%
        slice(which(!grepl("^tau\\[",.$variable))) %>%
        slice(which(!grepl("_z",.$variable))) %>%
        slice(which(!grepl("learning_rate\\[",.$variable))) %>%
        slice(which(!grepl("^decision_noise\\[",.$variable)))
    }

    if (save_results){
      out[["stan_results"]] = internal_results
    }

    out[["model_name"]] = internal_results$metadata()$model_name

    # Diagnostics using cmdstanr
    diagnostics = internal_results$diagnostic_summary()

    out[["diag_divergences"]]      = sum(diagnostics$num_divergent)
    out[["diagnostics_treedepth"]] = sum(diagnostics$num_max_treedepth)
    out[["diagnostics_ebfmi"]]     = diagnostics$ebfmi

    out
  }

  fit_t1 = fit_occasion(dat_t1)
  fit_t2 = fit_occasion(dat_t2)

  for (field in names(fit_t1)){
    if (field %in% c("point_estimates")) next
    results[[paste0(field, "_t1")]] = fit_t1[[field]]
  }
  for (field in names(fit_t2)){
    if (field %in% c("point_estimates")) next
    results[[paste0(field, "_t2")]] = fit_t2[[field]]
  }

  # Test-retest reliability: correlation between the Bayesian learning-rate estimates at t1 and t2.
  # This is the primary reliability estimand (replacing the old ground-truth-based rmp).
  if (fit_t1[["model_exists"]] & fit_t2[["model_exists"]]) {
    results[["test_retest_reliability"]] = cor(fit_t1[["point_estimates"]]$y, fit_t2[["point_estimates"]]$y)
  } else {
    results[["test_retest_reliability"]] = NA
  }

  if ((i %% 5)==0){
    write.csv(data.frame(y=""), file.path("progress_ri",paste0(i,".ignore")))
  }

  if (!is.null(temp_save_file_name)){
    filename = paste0(temp_save_file_name,i,".rds")
    saveRDS(results, file = file.path("results","study3results",filename))
  }

  return(results)
}

