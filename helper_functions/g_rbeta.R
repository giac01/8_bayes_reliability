
# Function to generate beta distributed data using mean and sd parameterisation 

g_rbeta = function(n, mu, sd){
  if (sd^2 >= (mu*(1-mu))){stop("REQUIREMENT: sd^2 < (mu*(1-mu))")}
  
  alpha = mu*((mu*(1-mu))/sd^2-1)
  beta  = (1-mu)*((mu*(1-mu)/sd^2) - 1)
  
  if (!all.equal(mu, alpha/(alpha+beta))) {warning("mu not correct")}
  if (!all.equal(sd^2, ((alpha*beta) / ( ((alpha+beta)^2) * (alpha+beta+1))))  ) {warning("sd not correct")}
 
  return(rbeta(n, alpha, beta))
  
}