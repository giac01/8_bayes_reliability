data {
  int<lower=0> n;
  int<lower=0> pps_n;
  int<lower=0> item_n;
  int<lower=1, upper=item_n> item[n];
  int<lower=0, upper=pps_n> pps[n];
  vector[n] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  // vector[n] mu;
  vector[pps_n] theta;                      # true scores 
  vector[item_n] item_intercept;            # Item intercepts 
  vector<lower=0, upper=1>[item_n] lambda;  # loadings
}

// transformed parameters {
  // vector<lower=0, upper=1>[n] sigma;  // actual group-level effects
  //  for (i in 1:n){
  //   sigma[i] = 1 - lambda[item[i]]^2;
  //  }

// }
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[n] mu;
  theta ~ normal(0, 1);

  for (i in 1:n){
    mu[i] = item_intercept[item[i]] + lambda[item[i]] * theta[pps[i]];
    y[i] ~ normal(mu[i], sqrt(1 - lambda[item[i]]^2));
  }
  
  // target += normal_lpdf(y | mu , sigma);

}




