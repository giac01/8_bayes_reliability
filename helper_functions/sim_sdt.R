# rm(list = ls())
# # set.seed(10)
# k_mean       = 0
# # m1_mean      = -.5
# # m2_mean      = .2
# # m_sigma      = 1
# sens_mean    = 1
# sens_sigma  = 1/4
# k_sigma      = 1/4
# n_trials_old = 10
# n_trials_new = 10
# n_pps        = 50

sim_sdt = function(
    k_mean     = 0,
    
    sens_mean  = 1,
    sens_sigma = 1/4,
    
    # m1_mean,
    # m2_mean,
    # m_sigma = 1,
    
    k_sigma,
    n_trials_old,
    n_trials_new,
    n_pps
  ){

  print("this function has errors and needs updating DO NOT USE! See sim_sdt_binomial")
# m1_pps = rnorm(n_pps, m1_mean, m_sigma^2)
# m2_pps = rnorm(n_pps, m2_mean, m_sigma^2)
  sens_pps= rnorm(n_pps, mean = sens_mean, sd = sens_sigma)
  k_pps  = rnorm(n_pps, k_mean, k_sigma^2)
  
  m1_pps = k_pps - sens_pps/2
  m2_pps = k_pps + sens_pps/2

  p_hit  = 1 - pnorm(k_pps, mean = m2_pps, sd = 1)
  p_fa   = 1 - pnorm(k_pps, mean = m1_pps, sd = 1)
  
  # true_sens = qnorm(p_hit) - qnorm(p_fa)
  
  old_trials = sapply(1:n_pps, function(i)
    rbinom(n_trials_old, size = 1 , prob = p_hit[i])
    ) %>%
    # t() %>%
    data.frame() %>%
    mutate(cond = 1)
  
  new_trials = sapply(1:n_pps, function(i)
    rbinom(n_trials_new, size = 1 , prob = p_fa[i])
  ) %>%
    # t() %>%
    data.frame() %>%
    mutate(cond = 0)
  
  trials = rbind.data.frame(old_trials, new_trials) %>%
    rowid_to_column(var = "trial") %>%
    pivot_longer(cols = starts_with("x")) %>% 
    mutate(pps = as.numeric(gsub("X","", name))) %>% 
    select(-name) %>%
    arrange(pps)

return(list(data = trials,
            p_hit = p_hit,
            p_fa  = p_fa,
            sens_pps = sens_pps,
            k_pps     = k_pps
            ))

}


