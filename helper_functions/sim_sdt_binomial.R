# rm(list = ls())
# # set.seed(10)
# k_mean       = 0
# # m1_mean      = -.5
# # m2_mean      = .2
# # m_sigma      = 1
# sens_mean    = 3
# sens_sigma  = 0
# k_sigma      = 1/4
# n_trials_old = 100
# n_trials_new = 50
# n_pps        = 10
# 
sim_sdt_binomial = function(
    
    sens_pps_direct = NULL,  # Enter specific values of sensitivity and k here
    k_pps_direct    = NULL,
  
    sens_mean  = 1,
    sens_sigma = 1/4,
    
    k_mean     = 0,
    k_sigma    = 0 ,
    
    n_trials_old,
    n_trials_new,
    n_pps
){
  # browser()
  
  sens_pps = rnorm(n_pps, mean = sens_mean, sd = sens_sigma)
  k_pps    = rnorm(n_pps, mean = k_mean,    sd = k_sigma)
  
  if (!is.null(sens_pps_direct) | !is.null(k_pps_direct)){
    sens_pps = sens_pps_direct
    k_pps    = k_pps_direct
  }
  
  m1_pps = - sens_pps/2
  m2_pps = + sens_pps/2
  
  p_falsealarm = 1 - pnorm(k_pps, mean = m1_pps, sd = 1)
  p_hit        = 1 - pnorm(k_pps, mean = m2_pps, sd = 1)
  
  # true_sens = qnorm(p_hit) - qnorm(p_falsealarm)
  
  new_trials = rbinom(n_pps, size = n_trials_new, prob = p_falsealarm)
  old_trials = rbinom(n_pps, size = n_trials_old, prob = p_hit)
  
  
  trials = data.frame(pps = 1:n_pps, old = old_trials, new = new_trials) %>% 
    pivot_longer(cols = c(old, new),
                 names_to = "cond",
                 values_to = "y"
                 ) %>% 
    mutate(n_trials = ifelse(cond=="old",n_trials_old,n_trials_new)) %>%
    mutate(cond = ifelse(cond == "old", 1/2, -1/2))
  
  return(list(data     = trials,
              p_hit    = p_hit,
              p_falsealarm  = p_falsealarm,
              sens_pps = sens_pps,
              k_pps    = k_pps,
              m1_pps   = m1_pps, 
              m2_pps   = m2_pps
  ))
  
}

