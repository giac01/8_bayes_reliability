// Tau-equivalent model 
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
  real<lower=0, upper=1> lambda;  // loadings
  real<lower=0, upper=1> sigma;
}

model {
  theta ~ normal(0, 1);
  vector[n] mu = lambda .* theta[pps];           // Thanks to bob carpenter for this refactoring suggestion https://discourse.mc-stan.org/t/regression-common-factor-model-in-stan/34277
  y ~ normal(mu, sigma);
}
