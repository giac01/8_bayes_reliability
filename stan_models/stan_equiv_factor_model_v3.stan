// Tau-equivalent model 
data {
  int<lower=0> n;                             // Number of rows in dataset (pps_n*item_n)
  int<lower=0> pps_n;                         // Number of participants 
  int<lower=0> item_n;                        // Number of items completed per participant (assuming all completed the same)
  array[n] int<lower=1, upper=item_n> item;   // Indicator variable for item 
  array[n] int<lower=1, upper=pps_n> pps;     // Indicator variable for participant
  vector[n] y;                                // Outcome
}

parameters {
  vector[pps_n] theta;                        // true scores 
  real<lower=0, upper=1> lambda;              // loadings
  real<lower=0, upper=1> sigma_add;// Variance inflation factor

}

model {
  theta ~ normal(0, 1);
  sigma_add ~ beta(1, 5);
  vector[n] mu = lambda .* theta[pps];           // Thanks to bob carpenter for this refactoring suggestion https://discourse.mc-stan.org/t/regression-common-factor-model-in-stan/34277
  y ~ normal(mu, sqrt(1 - square(lambda)) + sigma_add);
}
