/* RL 4arm-bandit model 5 : 'RL (Full) -- \tau + \alpha_Pos{Chosen} + \alpha_Neg{Chosen} + \alpha_Pos{Unchosen} + \alpha_Neg{Unchosen}'
*  author: Vincent Valton
*  email: vincent.valton@ucl.ac.uk
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

     real<lower=0> a_lrPosC;
     real<lower=0> b_lrPosC;
     real<lower=0> a_lrPosU;
     real<lower=0> b_lrPosU;
     real<lower=0> a_lrNegC;
     real<lower=0> b_lrNegC;
     real<lower=0> a_lrNegU;
     real<lower=0> b_lrNegU;
     real<lower=0> k_tau;
     real<lower=0, upper=20> theta_tau;

     vector<lower=0, upper=1>[N] lrPosC;
     vector<lower=0, upper=1>[N] lrPosU;
     vector<lower=0, upper=1>[N] lrNegC;
     vector<lower=0, upper=1>[N] lrNegU;
     vector<lower=0>[N] tau;

}

transformed parameters {
     vector<lower=0>[N] inv_temp;

     inv_temp = 1 ./ tau;
}

model {
     a_lrPosC ~ normal(1,5);
     b_lrPosC ~ normal(1,5);

     a_lrPosU ~ normal(1,5);
     b_lrPosU ~ normal(1,5);

     a_lrNegC ~ normal(1,5);
     b_lrNegC ~ normal(1,5);

     a_lrNegU ~ normal(1,5);
     b_lrNegU ~ normal(1,5);

     k_tau ~ normal(0.8,20);
     theta_tau ~ normal(1,20);

     lrPosC  ~ beta(a_lrPosC,b_lrPosC);
     lrPosU  ~ beta(a_lrPosU,b_lrPosU);
     lrNegC  ~ beta(a_lrNegC,b_lrNegC);
     lrNegU  ~ beta(a_lrNegU,b_lrNegU);
     tau ~ gamma(k_tau,theta_tau);

     for (i in 1:N) {
             vector[No] v_rwd;
             vector[No] v_plt;
             vector[No] v;
             vector[No] peR;
             vector[No] peP;

             v = initV;
             v_rwd = initV;
             v_plt = initV;

             for (t in 1:(Tsubj[i])) {
             		choice[i,t] ~ categorical_logit( inv_temp[i] * v );
                          // Calculate PE for chosen option
                          peR[choice[i,t]] = rwd[i,t] - v_rwd[choice[i,t]];
                          peP[choice[i,t]] = -abs(plt[i,t]) - v_plt[choice[i,t]];

                          // Update values for chosen option based on sign of PE
                          if (peR[choice[i,t]] > 0) { //Positive PE use lrPos for Chosen
                                v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + lrPosC[i] * peR[choice[i,t]];
                          }
                          else { //Negative PE use lrNeg for Unchosen
                                v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + lrNegC[i] * peR[choice[i,t]];
                          }
                          if (peP[choice[i,t]] > 0) { //Positive PE use lrPos for Unchosen
                                v_plt[choice[i,t]] = v_plt[choice[i,t]] + lrPosC[i] * peP[choice[i,t]];
                          }
                          else { //Negative PE use lrNeg for Unchosen
                                v_plt[choice[i,t]] = v_plt[choice[i,t]] + lrNegC[i] * peP[choice[i,t]];
                          }

                          // Calculate PE for all unchosen options & update values
                          for (i_unchosen in 1:(No-1)) {
                                peR[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_rwd[unchosen[choice[i,t],i_unchosen]];
                                peP[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_plt[unchosen[choice[i,t],i_unchosen]];

                                //update corresponding v_rwd & v_plt
                                if (peR[unchosen[choice[i,t],i_unchosen]] > 0) { //Positive PE use lrPos for Unchosen
                                      v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + lrPosU[i] * peR[unchosen[choice[i,t],i_unchosen]];
                                }
                                else { //Negative PE use lrNeg for Unchosen
                                      v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + lrNegU[i] * peR[unchosen[choice[i,t],i_unchosen]];
                                }
                                if (peP[unchosen[choice[i,t],i_unchosen]] > 0) { //Positive PE use lrPos for Unchosen
                                      v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + lrPosU[i] * peP[unchosen[choice[i,t],i_unchosen]];
                                }
                                else { //Negative PE use lrNeg for Unchosen
                                      v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + lrNegU[i] * peP[unchosen[choice[i,t],i_unchosen]];
                                }
                          }

                        // update value of all options (not just chosen)
                        v = v_rwd + v_plt;
             }
     }
}
generated quantities {
      vector [N] log_lik;

        for (i in 1:N) {
                  vector[No] v_rwd;
                  vector[No] v_plt;
                  vector[No] v;
                  vector[No] peR;
                  vector[No] peP;

                  v = initV;
                  v_rwd = initV;
                  v_plt = initV;
                  log_lik[i] = 0;

                  for (t in 1:(Tsubj[i])) {
                    log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] | inv_temp[i] * v );
                            // Calculate PE for chosen option
                            peR[choice[i,t]] = rwd[i,t] - v_rwd[choice[i,t]];
                            peP[choice[i,t]] = -abs(plt[i,t]) - v_plt[choice[i,t]];

                            // Update values for chosen option based on sign of PE
                            if (peR[choice[i,t]] > 0) { //Positive PE use lrPos for Chosen
                                  v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + lrPosC[i] * peR[choice[i,t]];
                            }
                            else { //Negative PE use lrNeg for Unchosen
                                  v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + lrNegC[i] * peR[choice[i,t]];
                            }
                            if (peP[choice[i,t]] > 0) { //Positive PE use lrPos for Unchosen
                                  v_plt[choice[i,t]] = v_plt[choice[i,t]] + lrPosC[i] * peP[choice[i,t]];
                            }
                            else { //Negative PE use lrNeg for Unchosen
                                  v_plt[choice[i,t]] = v_plt[choice[i,t]] + lrNegC[i] * peP[choice[i,t]];
                            }

                            // Calculate PE for all unchosen options & update values
                            for (i_unchosen in 1:(No-1)) {
                                  peR[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_rwd[unchosen[choice[i,t],i_unchosen]];
                                  peP[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_plt[unchosen[choice[i,t],i_unchosen]];

                                  //update corresponding v_rwd & v_plt
                                  if (peR[unchosen[choice[i,t],i_unchosen]] > 0) { //Positive PE use lrPos for Unchosen
                                          v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + lrPosU[i] * peR[unchosen[choice[i,t],i_unchosen]];
                                  }
                                  else { //Negative PE use lrNeg for Unchosen
                                          v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + lrNegU[i] * peR[unchosen[choice[i,t],i_unchosen]];
                                  }
                                  if (peP[unchosen[choice[i,t],i_unchosen]] > 0) { //Positive PE use lrPos for Unchosen
                                          v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + lrPosU[i] * peP[unchosen[choice[i,t],i_unchosen]];
                                  }
                                  else { //Negative PE use lrNeg for Unchosen
                                          v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + lrNegU[i] * peP[unchosen[choice[i,t],i_unchosen]];
                                  }
                            }

                    // update value of all options (not just chosen)
                    v = v_rwd + v_plt;
                }
        }
}
