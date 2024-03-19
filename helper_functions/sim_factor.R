
sim_factor_dat_unstandardised = function(
    n_rows,              # SCALAR: Sample Size
    loading,         # SCALAR OR VECTOR: Standardized Factor Loading
    n_items,             # SCALARL Number of items
    true_var = 1, 
    err_var = 1,
    debug = FALSE
    
){
  
  if (length(loading)==1 & n_items > 1){
    loading = rep(loading, n_items)
  }

  true_scores = rnorm(n_rows, mean = 0, sd = sqrt(true_var))
  dat         = sapply(1:n_items, function(i) loading[i]*true_scores + rnorm(n_rows, sd = sqrt(err_var))) # Note the sd(true_score) and sd(error) must be kept at 1 for the loading calculation to make sense!
  colnames(dat) = paste0("X",1:ncol(dat))
  dat         = cbind.data.frame(true_scores,dat) 
  
  if (debug){
    
    cat("\nTrue Score Variance: ",true_var, "\n")
    
    cat("\nError Variance: ", err_var, "\n")
    
    cat("\nloading: \n")
    print(loading)
    
    dat$mean = dat %>%
               select(starts_with("X")) %>%
               apply(.,1,mean)
    
    cat("\nCor(t, x):\n")
    print(round(cor(dat), digits = 2))
    
    cat("\nCor(t, x) SQAURED :\n")
    print(round(cor(dat)^2, digits = 2))
    
    cat("\nCov(t, x):\n")
    print(round(cov(dat), digits = 2))
    
  }
  
  return(dat)
}




