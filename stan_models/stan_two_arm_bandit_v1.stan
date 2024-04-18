
// Adapted from: https://github.com/cran/hBayesDM/blob/master/inst/stan_files/bandit2arm_delta.stan
// Specific version i've used: https://github.com/cran/hBayesDM/commit/c9bce7666a0d540dbdae68f13abbb38a3d8c410d

data {
  int<lower=1> N;                                    // Number of subjects
  int<lower=1> T;                                    // Number of trials 
  array[N] int<lower=1, upper=T> Tsubj;
  array[N, T] int<lower=1, upper=2> choice;        // I'm not sure why the original script allows a choice of -1 - odd. Either 1 or 2 makes sense
  array[N, T] int<lower=-1, upper=2> outcome;
}
transformed data {
  vector[2] initV;  // initial two values for Expected Values (EVs)
  initV = rep_vector(0.0, 2);
}
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu_pr;
  vector<lower=0>[2] sigma;

  // Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    // learning rate
  vector[N] tau_pr;  // inverse temperature
}
transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] A;
  vector<lower=0, upper=5>[N] tau;

  for (i in 1:N) {                                                // Setting up hierarchial effects
    // By using the normal CDF (phi_approx) 
    A[i]   = Phi_approx(mu_pr[1]  + sigma[1]  * A_pr[i]);         // Learning rate for pps i 
    tau[i] = Phi_approx(mu_pr[2]  + sigma[2] * tau_pr[i]) * 5;    // Decision noise e for pps i  - *5 allows the tau parameter to go up to 5!
  }
}
model {
  // Hyperparameters
  mu_pr  ~ normal(0, 1);              // mu_pr[1] -> average pop learning rate 
  sigma ~ normal(0, 0.2);             // standard deviations in individual diff in learning rate and decision noise

  // individual parameters
  A_pr   ~ normal(0, 1);              // Individual differences in learning rate (standardized scale)
  tau_pr ~ normal(0, 1);              // Individual differences in decision noise / temperature (standardized scale)

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
  // For group level parameters
  real<lower=0, upper=1> mu_A;
  real<lower=0, upper=5> mu_tau;

  // For log likelihood calculation
  vector[N] log_lik;

  // For posterior predictive check
  matrix[N, T] y_pred;

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_A   = Phi_approx(mu_pr[1]);
  mu_tau = Phi_approx(mu_pr[2]) * 5;

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[2] ev; // expected value
      real PE;      // prediction error

      // Initialize values
      ev = initV;

      log_lik[i] = 0;

      for (t in 1:(Tsubj[i])) {
        // compute log likelihood of current trial
        log_lik[i] += categorical_logit_lpmf(choice[i, t] | tau[i] * ev);

        // generate posterior prediction for current trial
        y_pred[i, t] = categorical_rng(softmax(tau[i] * ev));

        // prediction error
        PE = outcome[i, t] - ev[choice[i, t]];

        // value updating (learning)
        ev[choice[i, t]] += A[i] * PE;
      }
    }
  }
}
