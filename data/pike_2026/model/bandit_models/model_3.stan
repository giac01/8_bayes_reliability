/* RL 4arm-bandit model 3 : 'RL -- \tau + \alpha_{plty} + \alpha_{rwd}'
*  author: Vincent Valton
*  email: vincent.valton@ucl.ac.uk
*  model_m4: Full per rwd type (alpha_Rwd=free, tau=free, alpha_Plt=free)
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

     real<lower=0> a_lrR;
     real<lower=0> b_lrR;
     real<lower=0> a_lrP;
     real<lower=0> b_lrP;
     real<lower=0> k_tau;
     real<lower=0, upper=20> theta_tau;

     vector<lower=0, upper=1>[N] lrR;
     vector<lower=0, upper=1>[N] lrP;
     vector<lower=0, upper=6>[N] tau;

}

transformed parameters {
     vector<lower=0>[N] inv_temp;

     inv_temp = 1 ./ tau;
}

model {

     a_lrR ~ normal(1,5);
     b_lrR ~ normal(1,5);

     a_lrP ~ normal(1.2,5);
     b_lrP ~ normal(1.2,5);

     k_tau ~ normal(0.8,20);
     theta_tau ~ normal(1,20);

     lrR  ~ beta(a_lrR,b_lrR);
     lrP  ~ beta(a_lrP,b_lrP);
     tau ~ gamma(k_tau,theta_tau);


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
