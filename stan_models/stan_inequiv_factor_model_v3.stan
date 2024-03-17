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
  vector[item_n] lambda;  // loadings
  vector<lower=0>[item_n] sigma;
}

model {
  sigma ~ student_t(3,0,5);
  lambda[1] ~ uniform(0,1);
  theta ~ normal(0, 1);
  vector[n] mu = lambda[item] .* theta[pps];           // Thanks to bob carpenter for this refactoring suggestion https://discourse.mc-stan.org/t/regression-common-factor-model-in-stan/34277
  y ~ normal(mu, sigma[item]);
}