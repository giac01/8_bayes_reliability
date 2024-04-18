// V2:Removed generated quantities
// V3: beta alpha & beta parameterisation
// V4: beta mean and sample size parameterization 
// V5: Change lower limits of parameters
// V6: Moved back to logistic parametisation, clarifed some terms

// Adapted from: https://github.com/cran/hBayesDM/blob/master/inst/stan_files/bandit2arm_delta.stan
// Specific version i've used: https://github.com/cran/hBayesDM/commit/c9bce7666a0d540dbdae68f13abbb38a3d8c410d

data {
  int<lower=1> N;                                    // Number of subjects
  int<lower=1> T;                                    // Number of trials 
  array[N] int<lower=1, upper=T> Tsubj;              // Number of trials completed by each subject 
  array[N, T] int<lower=1, upper=2> choice;          // I'm not sure why the original script allows a choice of -1 - odd. Either 1 or 2 makes sense
  array[N, T] int outcome;
}

transformed data {
  vector[2] init_beliefs;  // initial two values for Expected Values (EVs)
  init_beliefs = rep_vector(0.0, 2);
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu;             // Group average learning rate [1] and decision noise [2]
  vector<lower=0>[2] sigma;    // Group SDs for learning rate [1] and decision noise [2]

  // Standardised Subject-Level Effects
  vector[N] learning_rate_z;    
  vector[N] decision_noise_z;   
}

transformed parameters {
  // Unstandardised Subject-Level Effects
  vector<lower=0, upper=1>[N] learning_rate;
  vector<lower=0, upper=5>[N] decision_noise;
  
  learning_rate  = inv_logit(mu[1] + sigma[1]*learning_rate_z);    
  decision_noise = 5 * inv_logit(mu[2] + sigma[2]*decision_noise_z);
}

model {
  // Hyperparameters
  mu    ~ logistic(0, 1);               // mu_z[1] -> average pop learning rate 
  sigma ~ logistic(0, 0.2);             // standard deviations in individual diff in learning rate and decision noise

  // individual parameters
  learning_rate_z  ~ logistic(0, 1);              // Individual differences in learning rate (standardized scale)
  decision_noise_z ~ logistic(0, 1);              // Individual differences in decision noise / temperature (standardized scale)

  // subject loop and trial loop
  for (i in 1:N) {                                                // iterate over subjects (N = sample size)
    vector[2] ev;                                                 // expected value
    real PE;                                                      // prediction error

    ev = init_beliefs;                                                   // Starting expected values

    for (t in 1:(Tsubj[i])) {
      // compute action probabilities
      choice[i, t] ~ categorical_logit(decision_noise[i] * ev);

      // prediction error
      PE = outcome[i, t] - ev[choice[i, t]];

      // value updating (learning)
      ev[choice[i, t]] += learning_rate[i] * PE;
    }
  }
}

generated quantities {
  real mean_pps_learning_rate  = mean(learning_rate);
  real sd_pps_learning_rate    = sd(learning_rate);
  real mean_pps_decision_noise = mean(decision_noise);
  real sd_pps_decision_noise   = sd(decision_noise);

  real mean_dist_learning_rate  = Phi_approx(mu[1]);
  real mean_dist_decision_noise = 5 * Phi_approx(mu[2]);
  
  vector[N] A = learning_rate;   // This is just because my R code assumes learning_rate is called A (like the original stan caode called it!)
}
