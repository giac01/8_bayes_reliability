// Congeneric model with hyperparameters for loadings and positive loadings.
// v15 changes vs v14:
//  - prior-predictive containers are size 0 unless prior_PPC == 1 (much smaller output)
//  - mu / sigma computed once in transformed parameters, shared by model block and GQ
//  - adds mcmc_coef_h_adj: coefficient H using the model-implied residual sd
//    (which includes sigma_add), alongside the v14 formula for backward comparison
//  - std_normal() for theta, vectorised normal_rng, sum-based H expression
data {
  int<lower=1> n;
  int<lower=1> pps_n;
  int<lower=1> item_n;
  array[n] int<lower=1, upper=item_n> item;
  array[n] int<lower=1, upper=pps_n> pps;
  vector[n] y;
  real<lower=0> lambda_sd_prior_sd;
  real<lower=0> lambda_mean_prior_sd;
  int<lower=0, upper=1> prior_PPC;
}
parameters {
  vector[pps_n] theta;                        // true scores
  vector[item_n] lambda_raw;                  // non-centred raw loadings
  vector<lower=0, upper=1>[item_n] sigma_add; // extra residual sd beyond sqrt(1 - lambda^2)
  real<lower=0> lambda_sd;                    // sd for loadings (half-logistic prior)
  real lambda_mean;                           // mean for loadings
}
transformed parameters {
  vector<lower=0, upper=1>[item_n] lambda = inv_logit(lambda_raw * lambda_sd + lambda_mean);
  vector<lower=0>[item_n] sigma_item = sqrt(1 - square(lambda)) + sigma_add;
}
model {
  // Priors
  theta ~ std_normal();
  sigma_add ~ beta(1, 5);

  // Hyperpriors
  lambda_sd ~ logistic(0, lambda_sd_prior_sd);
  lambda_mean ~ logistic(0, lambda_mean_prior_sd);

  // Priors for loadings
  lambda_raw ~ logistic(0, 1);

  // Likelihood
  if (prior_PPC == 0) {  // Only evaluate likelihood when not doing prior predictive
    y ~ normal(lambda[item] .* theta[pps], sigma_item[item]);
  }
}
generated quantities {
  // Prior predictive draws: size 0 (nothing stored) unless prior_PPC == 1
  vector[prior_PPC == 1 ? n : 0] y_prior;
  vector[prior_PPC == 1 ? n : 0] mu_prior;
  vector[prior_PPC == 1 ? n : 0] sigma_prior;
  real mcmc_coef_h;      // v14 formula: assumes residual variance = 1 - lambda^2
  real mcmc_coef_h_adj;  // uses the model-implied residual sd (includes sigma_add)

  {
    real ss = sum(square(lambda) ./ (1 - square(lambda)));
    mcmc_coef_h = ss / (1 + ss);
    real ss_adj = sum(square(lambda) ./ square(sigma_item));
    mcmc_coef_h_adj = ss_adj / (1 + ss_adj);
  }

  if (prior_PPC == 1) {
    mu_prior = lambda[item] .* theta[pps];
    sigma_prior = sigma_item[item];
    y_prior = to_vector(normal_rng(mu_prior, sigma_prior));
  }
}
