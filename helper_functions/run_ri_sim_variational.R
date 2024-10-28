run_ri_sim_variational = function(
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
    init_stan = "normal"
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
  results[["diagnostics"]]                       = list()
  
  dat = sim_ri( # remove warning from here!
    n_pps              = n_pps,
    n_trials           = n_trials,
    init_beliefs       = init_beliefs,
    learning_rate_mean = learning_rate_mean,
    learning_rate_sd   = learning_rate_sd,
    decision_noise_mean= decision_noise_mean,
    decision_noise_sd  = decision_noise_sd,
    prob_real          = prob_real,
    reward_outcome     = reward_outcome,
    save_beliefs       = FALSE
  )
  
  stan_data = list(
    N = nrow(dat$outcome),
    T = ncol(dat$outcome),
    Tsubj = apply(dat$outcome, 1, length),
    choice = dat$choice,
    outcome = dat$outcome
  )
  
  if (init_stan == "beta"){
    init_values_muphi <- function() {
      list(
        A_pop_mu = .5,
        A_pop_phi = 1,
        tau_unscaled_mu = .5,
        tau_unscaled_phi= 1,
        A = rep(.5, stan_data$N),
        tau_unscaled = rep(1/5, stan_data$N)
      )
    }
  }
  
  if (init_stan == "normal"){
    init_values_muphi <- function() {
      list(
        mu = c(0,.5),
        sigma = c(1,1),
  
        learning_rate_z = rep(0, stan_data$N),
        decision_noise_z = rep(0, stan_data$N)
      )
    }
  }
  
  time_a = Sys.time()
  
  internal_results = mod$variational(
    data = stan_data, 
    seed = 123,
    init = init_values_muphi,
    draws = 2000,
    # tol_rel_obj = .0001,
    # iter = 40000,
    
    tol_rel_obj = .0001, # increased by 1/10 (again)
    iter = 40000,
    algorithm = "meanfield",
    save_latent_dynamics = TRUE
  )
  
  time_b = Sys.time()
  
  results[["diagnostics"]][["fit_time"]] = as.numeric(difftime(time_b, time_a, units = "mins"))
  
   
  model_exists = (length(internal_results$output_files())!=0)
  # additional tests use up a lot of memory so need to be disabled for population calculations with large N_sim
   
  if (additional_tests == FALSE & model_exists) {

    learning_rate_estimates = internal_results$draws(variables = "A")
    # learning_rate_estimates = internal_results$draws(variables = "learning_rate")
    learning_rate_estimates = as.numeric(base::colMeans(learning_rate_estimates))
    learning_rate_estimates = data.frame(pps = 1:n_pps, 
                                         y   = learning_rate_estimates)
    
    learning_rate_estimates$true_learning_rate = dat$learning_rate
    
  }
  
  if (additional_tests == TRUE & model_exists) {

    learning_rate_estimates = internal_results$draws(variables = "A") %>% 
                              posterior::as_draws_df() %>% 
                              select(-.chain, -.iteration, -.draw)
    
    # CRASH HERE!
    learning_rate_estimates = learning_rate_estimates %>% 
                              `colnames<-`(c(1:n_pps)) %>%
                              pivot_longer(cols = everything(),
                                           names_to = "pps") %>% 
                              mutate(pps = as.numeric(pps)) %>%
                              group_by(pps) %>%
                              summarise(ggdist::mean_hdci(value, .width = .95)) 
                              
    learning_rate_estimates$true_learning_rate = dat$learning_rate
    
    learning_rate_estimates$ci_contain_true_score = as.numeric(
      (learning_rate_estimates$true_learning_rate>=learning_rate_estimates$ymin) & 
      (learning_rate_estimates$true_learning_rate<=learning_rate_estimates$ymax)
    )
  
    results[["avg_true_score_coverage"]] = length(which( learning_rate_estimates$ci_contain_true_score==1))/length(which( learning_rate_estimates$ci_contain_true_score<2))
    
    rmp_calc =  internal_results$draws(variables = "A", format = "matrix") %>%
                t() %>%
                reliability()
    
    results[["rmp"]] = rmp_calc$hdci
    results[["rmp_pd"]] = rmp_calc$pd
    
  }
  
  if (model_exists){
    results[["diagnostics"]][["model_name"]] = internal_results$metadata()$model_name

    x = read.csv(internal_results$latent_dynamics_files(), skip = 32)
    
    results[["diagnostics"]][["max_iter"]] = max(x$X..iter)
    
    # Diagnostics using cmdstanr
    

    results[["cor_bayes_estimate_true"]] = cor.test(learning_rate_estimates$y, learning_rate_estimates$true_learning_rate)
  
    results[["mean(learning_rate)"]] = mean(learning_rate_estimates$y)
  
  }
   
   if (!model_exists){
     
     results[["cor_bayes_estimate_true"]] = 9999 # This is to distinguish it from NAs which indicate 0 correlation! 
     
     results[["rmp"]] = NA
     results[["rmp_pd"]] = NA
     
     results[["mean(learning_rate)"]] = NA
     
   }
   
   if (model_exists & additional_tests==TRUE){
     results[["stan_results_summary"]] = internal_results$summary() %>%
       slice(which(!grepl("^A\\[",.$variable))) %>%
       slice(which(!grepl("^tau_unscaled\\[",.$variable))) %>%
       slice(which(!grepl("^tau\\[",.$variable))) %>%
       slice(which(!grepl("_z",.$variable))) %>%
       slice(which(!grepl("learning_rate\\[",.$variable))) %>%
       slice(which(!grepl("^decision_noise\\[",.$variable))) 
   }

  if (save_results & model_exists){
    results[["stan_results"]] = internal_results
  }

  if ((i %% 5)==0){
    write.csv(data.frame(y=""), file.path("progress_ri",paste0(i,".ignore")))
  }

  return(results)
}


