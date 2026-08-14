/* RL 4arm-bandit model 3 : 'RL -- \tau + \alpha_{plty} + \alpha_{rwd}' (non-centered)
*  original author: Vincent Valton
*  original file: model_3.stan
*  model_m4: Full per rwd type (alpha_Rwd=free, tau=free, alpha_Plt=free)
*
*  Reparameterized non-centered (mu/sigma + Phi_approx), matching the style already used in
*  model/rbias/model_3.stan. The original model_3.stan draws lrR/lrP/tau directly from
*  Beta/Gamma hyperpriors (a centered parameterization), which produced a funnel-geometry
*  convergence problem on this bandit data: rhat up to 1.74, ESS as low as ~6, and low E-BFMI
*  concentrated on lrP/tau/inv_temp/log_lik plus every group-level hyperparameter (see
*  diagnostics in 02_pike_2026.qmd). This version keeps the same data block, likelihood, and
*  effective prior shape, just reparameterized to sample better.
*
*  The non-centered reparameterization alone removed the funnel (zero divergences) but left
*  genuine multimodality in a handful of weakly-identified subjects' (lrP, tau): rhat up to 2.64,
*  ESS as low as ~5, concentrated in those subjects' lrP_raw/tau_raw plus the downstream
*  mu[2]/sigma[2] population parameters -- not fixable via adapt_delta/warmup since there were no
*  divergences to begin with. sigma's prior was tightened from cauchy(0, 2.5) to normal(0, 0.2)
*  (matching hBayesDM's bandit4arm_4par in 04_pike_2026_bandit4arm_4par.qmd, which converges
*  cleanly) to shrink those subjects harder toward the population mean and collapse the
*  bimodality. See the diagnostics section of 02_pike_2026_bandit.qmd for details.
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

     vector[3] mu;              // population means (probit scale): [1] = lrR, [2] = lrP, [3] = tau
     vector<lower=0>[3] sigma;  // population SDs

     vector[N] lrR_raw;
     vector[N] lrP_raw;
     vector[N] ivT_raw;

}

transformed parameters {
     vector<lower=0, upper=1>[N] lrR;
     vector<lower=0, upper=1>[N] lrP;
     vector<lower=0, upper=6>[N] ivT;

     for (i in 1:N) {
       lrR[i] = Phi_approx(mu[1] + sigma[1] * lrR_raw[i]);
       lrP[i] = Phi_approx(mu[2] + sigma[2] * lrP_raw[i]);
       ivT[i] = Phi_approx(mu[3] + sigma[3] * ivT_raw[i]) * 6;
     }
}

model {

     mu    ~ normal(0, 1);
     sigma ~ normal(0, 0.2);

     lrR_raw ~ std_normal();
     lrP_raw ~ std_normal();
     ivT_raw ~ std_normal();


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
             		choice[i,t] ~ categorical_logit( ivT[i] * v );
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
                    log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] | ivT[i] * v );
                              peR = rwd[i,t] - v_rwd[choice[i,t]];
                              peP = -abs(plt[i,t]) - v_plt[choice[i,t]];

                              v_rwd[choice[i,t]]= v_rwd[choice[i,t]] + lrR[i] * peR;
                              v_plt[choice[i,t]]= v_plt[choice[i,t]] + lrP[i] * peP;

                              v[choice[i,t]] = v_rwd[choice[i,t]] + v_plt[choice[i,t]];
                  }
        }
}
