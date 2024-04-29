
# Function to generate beta distributed data using mean and sd parameterisation 

g_normaluniform = function(n, mu, sd){
  return(pnorm(qnorm(mu) + rnorm(n, mean = 0, sd = sd)  ))
}


