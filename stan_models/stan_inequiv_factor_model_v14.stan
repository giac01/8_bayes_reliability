// Congeneric model with hyperparameters for loadings and positive first loading
data {
  int<lower=0> n;
  int<lower=0> pps_n;
  int<lower=0> item_n;
  array[n] int<lower=1, upper=item_n> item;
  array[n] int<lower=1, upper=pps_n> pps;
  vector[n] y;
  real<lower=0> lambda_sd_prior_sd;
  real<lower=0> lambda_mean_prior_sd;
  int<lower=0, upper=1> prior_PPC;
}
parameters {
  vector[pps_n] theta;                       // true scores 
  vector[item_n] lambda_raw;                // first loading (constrained positive)
  vector<lower=0, upper=1>[item_n] sigma_add;
  real<lower=0> lambda_sd;                   // sd for loadings
  real lambda_mean;                          // mean for loadings
}
transformed parameters {
  vector<lower=0, upper=1>[item_n] lambda;
  
  lambda = inv_logit(lambda_raw * lambda_sd + lambda_mean);  
}
model {
  // Priors
  theta ~ normal(0, 1);
  sigma_add ~ beta(1, 5);
  
  // Hyperpriors
  lambda_sd ~ logistic(0, lambda_sd_prior_sd);
  lambda_mean ~ logistic(0, lambda_mean_prior_sd);
  
  // Priors for loadings
  lambda_raw ~ logistic(0, 1);
  
  // Likelihood
  if (prior_PPC == 0) {  // Only evaluate likelihood when not doing prior predictive
    vector[n] mu = lambda[item] .* theta[pps];
    vector[n] sigma = sqrt(1 - square(lambda[item])) + sigma_add[item];
    y ~ normal(mu, sigma);
  }
}
generated quantities {
  // Arrays to store prior predictive draws
  vector[n] y_prior;
  vector[n] mu_prior;
  vector[n] sigma_prior;
  real mcmc_coef_h;
  
  mcmc_coef_h = pow(1 + pow(sum(square(lambda) ./ (1 - square(lambda))), -1), -1);

  // Generate prior predictive draws
  if (prior_PPC == 1) {
    mu_prior = lambda[item] .* theta[pps];
    sigma_prior = sqrt(1 - square(lambda[item])) + sigma_add[item];

    for (i in 1:n) {
      y_prior[i] = normal_rng(mu_prior[i], sigma_prior[i]);
    }
  } else {
    // Fill with missing values when not doing prior predictive
    for (i in 1:n) {
      y_prior[i] = not_a_number();
      mu_prior[i] = not_a_number();
      sigma_prior[i] = not_a_number();
    }
  }
}
