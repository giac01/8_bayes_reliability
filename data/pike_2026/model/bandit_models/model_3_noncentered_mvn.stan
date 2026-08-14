/* RL 4arm-bandit model 3 : 'RL -- \tau + \alpha_{plty} + \alpha_{rwd}' (non-centered, multivariate)
*  original author: Vincent Valton
*  original file: model_3.stan
*  model_m4: Full per rwd type (alpha_Rwd=free, tau=free, alpha_Plt=free)
*
*  Same data/likelihood as model_3_noncentered.stan, but replaces the three independent
*  univariate non-centered priors (lrR_raw/lrP_raw/tau_raw ~ std_normal(), each scaled by its
*  own sigma[k]) with a single multivariate non-centered prior over the 3 per-subject parameters,
*  using a Cholesky-factored population covariance (mu/sigma/L_Omega). This lets the population
*  lrR/lrP/tau correlation structure help pin down weakly-identified subjects.
*
*  Motivation: diagnostics on model_3_noncentered.stan showed genuine multimodality (not a
*  funnel -- zero divergences) in several subjects' (lrP_raw, tau_raw) posteriors -- e.g. subject
*  25 in the t2 fit has two disconnected posterior modes trading off "moderate lrP + noisy
*  choice" against "weak lrP + more deterministic choice" (mcmc_pairs shows two separated blobs).
*  Modeling the lrR/lrP/tau correlation explicitly gives well-identified subjects a chance to
*  pull ambiguous ones toward the population-consistent combination instead of each subject's
*  bimodality being resolved independently per chain. See the diagnostics section of
*  02_pike_2026_bandit.qmd.
*/

data {
     int<lower=1> N; 				//Number of subjects (strictly positive int)
     int<lower=1> T;  				//Number of trials (strictly positive int)
     array[N] int<lower=1, upper=T>Tsubj; 		//Number of trials per subject (1D array of ints) — contains the max number of trials per subject
     int<lower=2> No; 				//Number of choice options in total (int) — set to 4
     int<lower=2> Nopt;				//Number of choice options per trial (int) — set to 4

     matrix[N,T] rwd;		//Matrix of reals containing the reward received on a given trial (1 or 0) — (rows: participants, columns : trials)
     matrix[N,T] plt;		//Matrix of reals containing the penalty received on a given trial (-1 or 0) — (rows: participants, columns : trials)
     vector[No] Vinits;		//Vector or reals containing the initial q-values (set to [0, 0, 0, 0] for now);

     array[No,No-1] int <lower=1,upper=No> unchosen; // Preset matrix that maps lists unchosen options from chosen one — set to [2, 3, 4; 1, 3, 4; 1, 2, 4; 1, 2, 3]
     array[N,T] int <lower=1,upper=No> choice; 		 // Array of ints containing the choice made for each trial and participant (i.e. option chosen out of 4) — (rows: participants, columns: trials)
}

transformed data {

     vector[No] initV;
     initV = Vinits;
}

parameters {

     vector[3] mu;                     // population means (probit scale): [1] = lrR, [2] = lrP, [3] = tau
     vector<lower=0>[3] sigma;         // population SDs
     cholesky_factor_corr[3] L_Omega;  // Cholesky factor of the population lrR/lrP/tau correlation matrix

     matrix[3, N] z;                   // standard-normal raw scores, pre-correlation

}

transformed parameters {
     vector<lower=0, upper=1>[N] lrR;
     vector<lower=0, upper=1>[N] lrP;
     vector<lower=0, upper=6>[N] tau;
     vector<lower=0>[N] inv_temp;

     // non-centered multivariate normal: column i ~ MVN(0, diag(sigma) * Omega * diag(sigma))
     matrix[3, N] indiv_raw = diag_pre_multiply(sigma, L_Omega) * z;

     for (i in 1:N) {
       lrR[i] = Phi_approx(mu[1] + indiv_raw[1, i]);
       lrP[i] = Phi_approx(mu[2] + indiv_raw[2, i]);
       tau[i] = Phi_approx(mu[3] + indiv_raw[3, i]) * 6;
     }
     inv_temp = 1 ./ tau;
}

model {

     mu      ~ normal(0, 1);
     sigma   ~ normal(0, 1);
     L_Omega ~ lkj_corr_cholesky(2);

     to_vector(z) ~ std_normal();


     for (i in 1:N) {
             vector[No] v_rwd;
             vector[No] v_plt;
             vector[No] v;
             real peR;
             real peP;

             v = initV;
             v_rwd = initV;
             v_plt = initV;

             for (t in 1:(Tsubj[i])) {
             		choice[i,t] ~ categorical_logit( inv_temp[i] * v );
                       		peR = rwd[i,t] - v_rwd[choice[i,t]];
                          peP = -abs(plt[i,t]) - v_plt[choice[i,t]];

                          v_rwd[choice[i,t]]= v_rwd[choice[i,t]] + lrR[i] * peR;
                          v_plt[choice[i,t]]= v_plt[choice[i,t]] + lrP[i] * peP;

                       		v[choice[i,t]] = v_rwd[choice[i,t]] + v_plt[choice[i,t]];
             }
     }
}
generated quantities {
      vector [N] log_lik;
      corr_matrix[3] Omega = multiply_lower_tri_self_transpose(L_Omega); // recovered lrR/lrP/tau population correlation matrix

        for (i in 1:N) {
                  vector[No] v_rwd;
                  vector[No] v_plt;
                  vector[No] v;
                  real peR;
                  real peP;

                  v = initV;
                  v_rwd = initV;
                  v_plt = initV;
                  log_lik[i] = 0;

                  for (t in 1:(Tsubj[i])) {
                    log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] | inv_temp[i] * v );
                              peR = rwd[i,t] - v_rwd[choice[i,t]];
                              peP = -abs(plt[i,t]) - v_plt[choice[i,t]];

                              v_rwd[choice[i,t]]= v_rwd[choice[i,t]] + lrR[i] * peR;
                              v_plt[choice[i,t]]= v_plt[choice[i,t]] + lrP[i] * peP;

                              v[choice[i,t]] = v_rwd[choice[i,t]] + v_plt[choice[i,t]];
                  }
        }
}
