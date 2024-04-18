sim_ri = function(
  n_pps, 
  n_trials,
  init_beliefs,
  learning_rate_mean,
  learning_rate_sd,
  decision_noise_mean, 
  decision_noise_sd,
  reward_outcome, 
  prob_real,
  save_beliefs = TRUE
){
  # n_pps               = 500
  # n_trials            = 200
  # init_beliefs        = c(0, 0)
  # prob_real           = c(.2, .8)
  # learning_rate_mean  = .5
  # learning_rate_sd    = .1
  # decision_noise_mean = 1
  # decision_noise_sd   = .3

  
  if (learning_rate_sd==0){learning_rate = rep(learning_rate_mean, n_pps)} else{
    learning_rate  = g_rbeta(n_pps, learning_rate_mean, learning_rate_sd)
  }

  if (decision_noise_sd==0){decision_noise = rep(decision_noise_mean, n_pps)} else{
    decision_noise = g_rbeta(n_pps, mu = decision_noise_mean/5, sd = decision_noise_sd/5)*5
  }

  # Initialise outcome matrix, choice matrix, and beliefs matrix 
  outcome = matrix(ncol = n_trials, nrow = n_pps)   
  choice  = matrix(ncol = n_trials, nrow = n_pps)   
  ev      = matrix(ncol = n_trials, nrow = n_pps)
  beliefs_outcome_list = list()
  
  for (i in 1:n_pps){
    beliefs_outcome    = init_beliefs
    # prob_real          = c(.2, .8)
    beliefs_outcome_list[[i]] = list()
    
    for (j in 1:n_trials){
      choice_probability = g_softmax(decision_noise[i]*beliefs_outcome)    # probabilities of observed choices 
      
      choice[i,j]      = rbinom(n = 1, size = 1, prob = choice_probability[2]) + 1
      outcome[i,j]     = reward_outcome[rbinom(n = 1, size = 1, prob = prob_real[choice[i,j]])+1]
      
      prediction_error = outcome[i,j] - beliefs_outcome[choice[i,j]]
      
      beliefs_outcome[choice[i,j]]  = beliefs_outcome[choice[i,j]] + learning_rate[i]*prediction_error
      
      if (save_beliefs) {beliefs_outcome_list[[i]][[j]] = beliefs_outcome}
    }
    
  }
  
  # Covert data to long format for analysis readyness 
  
  choice_long = 
    choice %>%
    as_tibble() %>%
    rownames_to_column(var = "subjID") %>%
    pivot_longer(
      cols = starts_with("V"),
      values_to = "choice"
    )
  
  outcome_long = 
    outcome %>%
    as_tibble() %>%
    rownames_to_column(var = "subjID") %>%
    pivot_longer(
      cols = starts_with("V"),
      values_to = "outcome"
    )
  
  dat = full_join(choice_long, outcome_long, by = join_by(subjID, name))
  
  return(list(
    dat    = dat,
    choice = choice,
    outcome = outcome,
    learning_rate = learning_rate,
    decision_noise = decision_noise
    ))
}
