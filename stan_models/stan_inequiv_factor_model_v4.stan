// Tau-inequivalent model 
data {
  int<lower=0> n;
  int<lower=0> pps_n;
  int<lower=0> item_n;
  array[n] int<lower=1, upper=item_n> item;
  array[n] int<lower=1, upper=pps_n> pps;
  vector[n] y;
}

parameters {
  vector[pps_n] theta;                      // true scores 
  vector<lower=0, upper=1>[item_n] lambda;  // loadings
  vector<lower=0,  upper=1>[item_n] sigma_add;
}

model {
  theta ~ normal(0, 1);
  sigma_add ~ beta(1, 5);
  vector[n] mu = lambda[item] .* theta[pps];           // Thanks to bob carpenter for this refactoring suggestion https://discourse.mc-stan.org/t/regression-common-factor-model-in-stan/34277
  vector[n] sigma = sqrt(1 - square(lambda[item])) + sigma_add[item];
  y ~ normal(mu, sigma);
}
