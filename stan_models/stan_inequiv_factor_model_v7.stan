// Congeneric model with hyperparameters for loadings and positive first loading
data {
  int<lower=0> n;
  int<lower=0> pps_n;
  int<lower=0> item_n;
  array[n] int<lower=1, upper=item_n> item;
  array[n] int<lower=1, upper=pps_n> pps;
  vector[n] y;
  real<lower=0> lambda_sd_prior_sd;
}
parameters {
  vector[pps_n] theta;                       // true scores 
  real<lower=0> lambda_raw_1;                // first loading (constrained positive)
  vector[item_n - 1] lambda_raw_rest;        // rest of the loadings
  vector<lower=0, upper=1>[item_n] sigma_add;
  real<lower=0> lambda_sd;                   // sd for loadings
  real<lower=0> lambda_mean;                          // mean for loadings
}
transformed parameters {
  vector[item_n] lambda_raw;
  vector<lower=-1, upper=1>[item_n] lambda;
  
  lambda_raw = append_row(lambda_raw_1, lambda_raw_rest);
  lambda = inv_logit(lambda_raw * lambda_sd + lambda_mean) * 2 - 1;  // Transform to (-1, 1)
}
model {
  // Priors
  theta ~ normal(0, 1);
  sigma_add ~ beta(1, 5);
  
  // Hyperpriors
  lambda_sd ~ normal(0, lambda_sd_prior_sd);
  lambda_mean ~ normal(0, 1);
  
  // Priors for loadings
  lambda_raw_1 ~    normal(0, 1);  
  lambda_raw_rest ~ normal(0, 1);  
  
  // Likelihood
  vector[n] mu = lambda[item] .* theta[pps];
  vector[n] sigma = sqrt(1 - square(lambda[item])) + sigma_add[item];
  y ~ normal(mu, sigma);
}
