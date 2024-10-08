
// Removed generated quantities
// beta parameterisation

// Adapted from: https://github.com/cran/hBayesDM/blob/master/inst/stan_files/bandit2arm_delta.stan
// Specific version i've used: https://github.com/cran/hBayesDM/commit/c9bce7666a0d540dbdae68f13abbb38a3d8c410d

data {
  int<lower=1> N;                                    // Number of subjects
  int<lower=1> T;                                    // Number of trials 
  array[N] int<lower=1, upper=T> Tsubj;
  array[N, T] int<lower=1, upper=2> choice;        // I'm not sure why the original script allows a choice of -1 - odd. Either 1 or 2 makes sense
  array[N, T] int outcome;
}
transformed data {
  vector[2] initV;  // initial two values for Expected Values (EVs)
  initV = rep_vector(0.0, 2);
}
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  real<lower=0> A_pop_alpha;
  real<lower=0> A_pop_beta;
  
  real<lower=0> tau_unscaled_alpha;
  real<lower=0> tau_unscaled_beta;
  
  // Subject-level raw parameters 
  vector<lower=0, upper=1>[N] A;    // learning rate
  vector<lower=0, upper=1>[N] tau_unscaled;  // inverse temperature / 5
}

transformed parameters {
  vector[N] tau = 5 * tau_unscaled;  // Scale up by 5
}

model {
  // Hyperparameters
  A  ~ beta(A_pop_alpha, A_pop_beta);              // mu_pr[1] -> average pop learning rate 
  tau_unscaled ~ beta(tau_unscaled_alpha, tau_unscaled_beta);     // standard deviations in individual diff in learning rate and decision noise
  
  // subject loop and trial loop
  for (i in 1:N) {                                                // iterate over subjects (N = sample size)
    vector[2] ev;                                                 // expected value
    real PE;                                                      // prediction error

    ev = initV;                                                   // Starting expected values

    for (t in 1:(Tsubj[i])) {
      // compute action probabilities
      choice[i, t] ~ categorical_logit(tau[i] * ev);

      // prediction error
      PE = outcome[i, t] - ev[choice[i, t]];

      // value updating (learning)
      ev[choice[i, t]] += A[i] * PE;
    }
  }
}

generated quantities {
  real mean_A;
  real sd_A;
  real mean_tau_unscaled;
  real sd_tau_unscaled;
  real mean_tau;
  real sd_tau;

  // Calculating mean and standard deviation for A
  mean_A = A_pop_alpha / (A_pop_alpha + A_pop_beta);
  sd_A = sqrt((A_pop_alpha * A_pop_beta) / (pow(A_pop_alpha + A_pop_beta, 2) * (A_pop_alpha + A_pop_beta + 1)));

  // Calculating mean and standard deviation for tau_unscaled
  mean_tau_unscaled = tau_unscaled_alpha / (tau_unscaled_alpha + tau_unscaled_beta);
  sd_tau_unscaled = sqrt((tau_unscaled_alpha * tau_unscaled_beta) / (pow(tau_unscaled_alpha + tau_unscaled_beta, 2) * (tau_unscaled_alpha + tau_unscaled_beta + 1)));
  
  // Calculating mean and standard deviation for tau
  mean_tau = mean_tau_unscaled * 5;
  sd_tau = sd_tau_unscaled * 5;
}

