// Standardised congeneric model with hyperparameters for loadings and positive loadings.
// v17 = v14 without sigma_add: items are standardised before fitting (variance = 1),
// so the residual sd is fully determined by the loading: sigma = sqrt(1 - lambda^2).
// One parameter per item; lambda IS the standardised loading, and the coefficient H
// formula is exactly consistent with the likelihood.
// Keeps v15's efficiency changes (shared sigma_item, size-0 prior-predictive draws).
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
  real<lower=0> lambda_sd;         // sd for loadings (half-logistic prior)
  real lambda_mean;                // mean for loadings
}
transformed parameters {
  vector[item_n] lambda_logit = lambda_raw * lambda_sd + lambda_mean;
  vector<lower=0, upper=1>[item_n] lambda = inv_logit(lambda_logit);
  // sqrt(1 - lambda^2) computed as sqrt((1 - lambda) * (1 + lambda)), with
  // 1 - lambda = inv_logit(-lambda_logit): stays positive even when lambda
  // rounds to 1 in double precision, so the likelihood never sees scale = 0
  vector<lower=0>[item_n] sigma_item = sqrt(inv_logit(-lambda_logit) .* (1 + lambda));
}
model {
  // Priors
  theta ~ std_normal();

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
  real mcmc_coef_h;

  // Prior predictive draws: size 0 (nothing stored) unless prior_PPC == 1
  vector[prior_PPC == 1 ? n : 0] y_prior;
  vector[prior_PPC == 1 ? n : 0] mu_prior;
  vector[prior_PPC == 1 ? n : 0] sigma_prior;

  {
    real ss = sum(square(lambda) ./ (1 - square(lambda)));
    mcmc_coef_h = ss / (1 + ss);
  }

  if (prior_PPC == 1) {
    mu_prior = lambda[item] .* theta[pps];
    sigma_prior = sigma_item[item];
    y_prior = to_vector(normal_rng(mu_prior, sigma_prior));
  }
}
