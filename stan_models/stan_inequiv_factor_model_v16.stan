// Congeneric model with hyperparameters for loadings and positive loadings.
// v16 changes vs v15:
//  - replaces the sqrt(1 - lambda^2) + sigma_add residual parameterisation with a
//    free per-item residual sd (sigma ~ half-normal(0, 1)). In v14/v15 lambda and
//    sigma_add jointly determined the residual sd and could trade off against each
//    other; here each item has one loading and one residual sd, so the decomposition
//    is identified directly by the data (total item variance ~ 1 after standardising).
//  - lambda is the raw loading; the standardised loading lambda_std is computed in
//    generated quantities as lambda / sqrt(lambda^2 + sigma^2).
//  - single mcmc_coef_h, computed from the model-implied signal-to-noise
//    lambda^2 / sigma^2 (identical to using lambda_std in the v14 formula).
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
  vector[pps_n] theta;             // true scores (variance fixed to 1 by the prior)
  vector[item_n] lambda_raw;       // non-centred raw loadings
  vector<lower=0>[item_n] sigma;   // free residual sd per item
  real<lower=0> lambda_sd;         // sd for loadings (half-logistic prior)
  real lambda_mean;                // mean for loadings
}
transformed parameters {
  vector<lower=0, upper=1>[item_n] lambda = inv_logit(lambda_raw * lambda_sd + lambda_mean);
}
model {
  // Priors
  theta ~ std_normal();
  sigma ~ normal(0, 1);            // half-normal; residual sd is < 1 for standardised items

  // Hyperpriors
  lambda_sd ~ logistic(0, lambda_sd_prior_sd);
  lambda_mean ~ logistic(0, lambda_mean_prior_sd);

  // Priors for loadings
  lambda_raw ~ logistic(0, 1);

  // Likelihood
  if (prior_PPC == 0) {  // Only evaluate likelihood when not doing prior predictive
    y ~ normal(lambda[item] .* theta[pps], sigma[item]);
  }
}
generated quantities {
  // Standardised loadings: theta has variance 1, so implied item variance = lambda^2 + sigma^2
  vector<lower=0, upper=1>[item_n] lambda_std = lambda ./ sqrt(square(lambda) + square(sigma));
  real mcmc_coef_h;

  // Prior predictive draws: size 0 (nothing stored) unless prior_PPC == 1
  vector[prior_PPC == 1 ? n : 0] y_prior;
  vector[prior_PPC == 1 ? n : 0] mu_prior;
  vector[prior_PPC == 1 ? n : 0] sigma_prior;

  {
    real ss = sum(square(lambda) ./ square(sigma));
    mcmc_coef_h = ss / (1 + ss);
  }

  if (prior_PPC == 1) {
    mu_prior = lambda[item] .* theta[pps];
    sigma_prior = sigma[item];
    y_prior = to_vector(normal_rng(mu_prior, sigma_prior));
  }
}
