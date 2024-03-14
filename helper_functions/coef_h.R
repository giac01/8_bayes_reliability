# Coefficient H and bootstrap confidence interval 
# 
# datahead(d) = dat_scale %>%
#   select(-true_scores, -mean)

coef_h = function(
    data, 
    iter = 1000,
    ci_calc = TRUE
    ){
 famod = psych::fa(data, nfactors = 1)
 l = as.numeric(famod$loadings)
 r = (1+(sum(l^2/(1-l^2))^-1))^-1   # See mcneish thanks alpha paper for formula

 ci = NULL
 
 if (ci_calc == TRUE){
   r_boot = sapply(1:iter, function(x){
     # Use tryCatch to handle errors
     famod_boot = tryCatch({
       psych::fa(data[sample(x=nrow(data), size=nrow(data), replace = TRUE),], nfactors = 1)
     }, error = function(e) return(NA)) # Return NA on error
     
     # Check if famod_boot is not NA (meaning fa() was successful)
     if(!identical(NA, famod_boot)){
       l = as.numeric(famod_boot$loadings)
       r = (1+(sum(l^2/(1-l^2))^-1))^-1
       return(r)
     } else {
       return(NA) # Return NA if fa() failed
     }
   }
   )
 }
 
 ci = quantile(r_boot, probs = c(.025, .975), type = 6, na.rm = TRUE)
 
 return(list(r = r, ci = ci))
}
 
