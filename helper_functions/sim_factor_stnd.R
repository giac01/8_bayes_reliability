# Simulate data by specifying the standardised loading - measurement error set to 1 - lambda^2

sim_factor_stnd = function(
    n_rows,              # SCALAR: Sample Size
    std_loading,         # SCALAR OR VECTOR: Standardized Factor Loading
    intercepts,
    debug = FALSE
    
){

  n_items = length(std_loading)
  
  if (is.null(intercepts)){
    intercepts  = rep(0, n_items)
  }
  
  error_variance = 1^2 - std_loading^2        # Set the error variance to 1-loading^2 (because obs variance =1)
  
  true_scores = rnorm(n_rows)
  dat_t1         = sapply(1:n_items, function(i) intercepts[i] + std_loading[i]*true_scores + rnorm(n_rows, sd = sqrt(error_variance[i]))) # Note the sd(true_score) and sd(error) must be kept at 1 for the loading calculation to make sense!
  dat_t1         = data.frame(true_scores,dat_t1) 
  
  dat_t2         = sapply(1:n_items, function(i) intercepts[i] + std_loading[i]*true_scores + rnorm(n_rows, sd = sqrt(error_variance[i]))) # Note the sd(true_score) and sd(error) must be kept at 1 for the loading calculation to make sense!
  dat_t2         = data.frame(true_scores,dat_t2) 
  
  if (debug){
    cat("Error Variance: \n")
    print(error_variance)
    
    cat("std_loading: \n")
    print(std_loading)
    
    dat_t1$mean = dat_t1 %>%
      select(starts_with("X")) %>%
      apply(.,1,mean)
    
    dat_t2$mean = dat_t2 %>%
      select(starts_with("X")) %>%
      apply(.,1,mean)
    
    cat("\nCor(t, x):\n")
    print(round(cor(dat_t1), digits = 2))
    
    cat("\nCor(t, x) SQAURED :\n")
    print(round(cor(dat_t1)^2, digits = 2))
    
    cat("\nCov(t, x):\n")
    print(round(cov(dat_t1), digits = 2))
    
    
  }
  
  return(list(dat_t1,dat_t2))
}
