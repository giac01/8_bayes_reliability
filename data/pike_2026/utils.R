bandit_datalist<-function(data){
  # data block from model
  # data {
  #   int<lower=1> N; 				//Number of subjects (strictly positive int)
  #   int<lower=1> T;  				//Number of trials (strictly positive int)
  #   int<lower=1, upper=T> Tsubj[N]; 		//Number of trials per subject (1D array of ints) — contains the max number of trials per subject
  #   int<lower=2> No; 				//Number of choice options in total (int) — set to 4
  #   int<lower=2> Nopt;				//Number of choice options per trial (int) — set to 4
  #   
  #   matrix[N,T] rwd;		//Matrix of reals containing the reward received on a given trial (1 or 0) — (rows: participants, columns : trials)
  #   matrix[N,T] plt;		//Matrix of reals containing the penalty received on a given trial (-1 or 0) — (rows: participants, columns : trials)
  #   vector[No] Vinits;		//Vector or reals containing the initial q-values (set to [0, 0, 0, 0] for now);
  #   
  #   int<lower=1,upper=No> unchosen[No,No-1]; // Preset matrix that maps lists unchosen options from chosen one — set to [2, 3, 4; 1, 3, 4; 1, 2, 4; 1, 2, 3]
  #   int<lower=1,upper=No> choice[N,T]; 		 // Array of ints containing the choice made for each trial and participant (i.e. option chosen out of 4) — (rows: participants, columns: trials)
  # }
  # 
  
  #remove any repeated trials (removes first instance)
  bandit_dups_idx<-which(duplicated(data%>%select(id,trial_nr),fromLast=TRUE))
  if (length(bandit_dups_idx)>0){
    data<-data[-c(bandit_dups_idx),]
  }
  
  Tsubj=data%>%
    group_by(id)%>%
    summarise(Tsubj=(max(trial_nr)+1))%>%
    mutate(id=NULL)
  
  rwd=data%>%
    select(is_gain, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = is_gain)%>%
    mutate(id=NULL)
  
  plt=data%>%
    select(is_loss, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = is_loss)%>%
    mutate(id=NULL)
  
  choice=data%>%
    select(response, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = response)%>%
    mutate(id=NULL)
  
  out<-list(N=length(unique(data$id)),
            T=max(data$trial_nr+1),
            Tsubj=Tsubj$Tsubj,
            No=4,
            Nopt=4,
            rwd=rwd,
            plt=plt,
            Vinits=c(0.5,0.5,0.5,0.5),
            unchosen=array(data=c(2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3),dim = c(4,3)),
            choice=choice)
  
  return(out)
}

gamble_datalist<-function(data){
  # data block from model
  # data {
  #   int<lower=1> N;// Number of subjects (strictly positive int)
  #   int<lower=1> T; // Max number of trials (strictly positive int)
  #   array [N] int <lower=1, upper=T> Tsubj; // Max number of trials per participants (1D array of ints)
  #   array [N, T] int<lower=-1, upper=1> gamble; // 2D array of ints containing whether participant gambled or not (1, 0 respectively) on a given trial — (Row: participant, column: Trial)
  #   array [N, T] int cert; // 2D Array of reals containing value for the sure option for each participant and trial — (Row: participant, columns: trials)
  #   array [N, T] real<lower=0> gain; // 2D Array of reals containing value for the gain in the gamble for each participant and trial — (Row: participant, columns: trials)
  #   array [N, T] real<lower=0> loss; // 2D Array of reals containing value for the loss in the gamble for each participant and trial — (Row: participant, columns: trials)
  #   
  # }
  
  #remove any repeated trials (removes second instance)
  gamble_dups_idx<-which(duplicated(data%>%select(id,trial_nr),fromLast=TRUE))
  if (length(gamble_dups_idx)>0){
    data<-data[-c(gamble_dups_idx),]
  }  
  
  data<-data%>%
    filter(!is.na(trial_nr))
  
  Tsubj=data%>%
    group_by(id)%>%
    summarise(Tsubj=max(trial_nr,na.rm=TRUE)+1)%>%
    mutate(id=NULL)
  
  choice=data%>%
    select(chose_risky, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = chose_risky)%>%
    mutate(id=NULL)
  
  cert=data%>%
    select(safe, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = safe)%>%
    mutate(id=NULL)
  
  gain=data%>%
    select(risky_gain, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = risky_gain)%>%
    mutate(id=NULL)
  
  loss=data%>%
    select(risky_loss, id, trial_nr)%>%
    mutate(risky_loss = -1*risky_loss)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = risky_loss)%>%
    mutate(id=NULL)


  out<-list(N=length(unique(data$id)),
            T=max(data$trial_nr,na.rm=TRUE)+1,
            Tsubj=Tsubj$Tsubj,
            gamble=choice,
            cert=cert,
            gain=gain,
            loss=loss)
  
  return(out)
}

effort_datalist<-function(data){
  # model data block
  # data{
  #   int<lower=1> Ns; // number of subjects (strictly positive int)
  #   int<lower=0> Nx; // maximum number of trials (int)
  #   int<lower=1> Ni; // number of predictors (ignore for now and set to 1)
  #   int<lower=0,upper=1> y[Ns,Nx]; // Responses (accept/refuse = 1 or 0) — 2D array of ints (rows: participant, columns: trials)
  #   
  #   matrix<lower=0>[Ns,Nx] x_rwd; // Matrix of reals containing the reward level for each participant and trial — (rows: participant, column: trial)
  #   matrix<lower=0>[Ns,Nx] x_eff;   // Matrix of reals containing the effort level for each participant and trial — (rows: participant, column: trial)
  # }
  
  data<-data%>%
    filter(phase=='main')
  
  #remove any repeated trials (removes first instance)
  effort_dups_idx<-which(duplicated(data%>%select(id,trial_nr),fromLast=TRUE))
  if (length(effort_dups_idx)>0){
    data<-data[-c(effort_dups_idx),]
  }
  
  response=data%>%
    select(accepted, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = accepted)%>%
    mutate(id=NULL)
  
  x_rwd=data%>%
    select(reward, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = reward)%>%
    mutate(id=NULL)
  
  x_eff=data%>%
    select(difficulty, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = difficulty)%>%
    mutate(id=NULL)
  
  
  out<-list(Ns=length(unique(data$id)),
            Nx=max(data$trial_nr+1),
            Ni=1,
            y=response,
            x_rwd=x_rwd,
            x_eff=x_eff)
  
  return(out)
}


gershman_preproc<-function(data){
  gershman_data = data %>%
    mutate(opt_left = ifelse(opt_left=='r', 'Risky', 'Safe'),
           opt_right = ifelse(opt_right=='r', 'Risky', 'Safe'),
           choice = ifelse(response=='left', opt_left, opt_right),
           chose_risky = ifelse(choice=='Risky', 1, 0))
  
  gershman_data = gershman_data %>%
    mutate(value_difference = value_right - value_left, 
           chose_right = as.numeric(response=='right'),
           condition = paste(opt_left, opt_right))
  
  gershman_data = gershman_data %>%
    mutate(trial_nr = trial_nr+1)%>%
    mutate(trial_nr = block_nr*10 + trial_nr)%>%
    arrange(id, trial_nr) %>%
    group_by(id) %>%
    mutate(
      n_trials = n(),
      score = cumsum(reward),
      prev_reward = lag(reward),
      change = ifelse(response == lag(response), 0, 1),
      mean_rt = mean(rt, na.rm=T)) %>%
    ungroup()
  
  #remove any repeated trials (removes first instance)
  gershman_dups_idx<-which(duplicated(gershman_data%>%select(id,trial_nr),fromLast=TRUE))
  if (length(gershman_dups_idx)>0){
    gershman_data<-gershman_data[-c(gershman_dups_idx),]
  }
  
  # gershman_data = gershman_data %>% select(-all_of(names(latents)))
  latents = kalman_filter(gershman_data, q_initial = 100^2, q_safe = .001, q_risky = 16^2)
  # Transformations (see Gershman papers)
  gershman_data = cbind(gershman_data, latents)
  out = 'gershman_data_processed.csv'
  write.csv(gershman_data, out, row.names = F)
  
  gershman_data<-gershman_data%>%
    mutate_at(vars(starts_with('kalman')), scale)
  
}

gershman_datalist<-function(data){
  # data block stan model
  # data {
  #   int<lower=1> N; 				            //Number of subjects (strictly positive int)
  #   int<lower=1> T;  				          //Number of trials (strictly positive int)
  #   int<lower=1, upper=T> Tsubj[N]; 		//Number of trials per subject (1D array of ints) — contains the max number of trials per subject
  #   
  #   // V, RU and VTU need to be transformed: mutate_at(vars(starts_with('kalman')), scale) %>% # z transform
  #   matrix[N,T] V;		  //Matrix of z-transformed reals containing the Kalman Value Difference (i.e. V) on that trial — (rows: participants, columns : trials)
  #   matrix[N,T] RU;		//Matrix of z-transformed reals containing the Kalman Sigma Difference (i.e. RU) on that trial — (rows: participants, columns : trials)
  #   matrix[N,T] VTU;		//Matrix of z-transformed reals containing the Kalman Value Difference (i.e. V/TU) on that trial — (rows: participants, columns : trials)
  #   
  #   int choice[N,T];   // Array of ints containing the choice made for each trial and participant (i.e. option chosen out of 2 : 0 or 1) — (rows: participants, columns: trials)
  # }
  
  if (sum((str_detect(names(data),'kalman')))<1){
    data<-gershman_preproc(data)
  }
  
  Tsubj=data%>%
    group_by(id)%>%
    summarise(Tsubj=max(trial_nr))%>%
    mutate(id=NULL)
  
  V=data%>%
    select(kalman_value_difference, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = kalman_value_difference)%>%
    mutate(id=NULL)
  
  RU=data%>%
    select(kalman_sigma_difference, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = kalman_sigma_difference)%>%
    mutate(id=NULL)
  
  VTU=data%>%
    select(kalman_weighted_value_difference, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = kalman_weighted_value_difference)%>%
    mutate(id=NULL)
  
  choice=data%>%
    mutate(choice = 1-chose_right) %>%
    select(choice, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = choice)%>%
    mutate(id=NULL)
  
  
  out<-list(N=length(unique(data$id)),
            T=max(data$trial_nr),
            Tsubj=Tsubj$Tsubj,
            V=V,
            RU=RU,
            VTU=VTU,
            choice=choice)
  
  return(out)
}

rewardbias_datalist<-function(data){
  # data block from model
  # data {
  #   int<lower=1> N; 				//Number of subjects (strictly positive int)
  #   int<lower=1> T;  				//Number of trials (strictly positive int)
  #   int<lower=1> levels;     //Number of levels of congruence: set to 5
  #   
  #   int<lower=1,upper=2> choice[N,T]; 		 // Array of ints containing the choice made for each trial and participant (i.e. whether they chose left or right) — (rows: participants, columns: trials)
  #   int<lower=0,upper=1> accuracy[N,T]; //For whether they actually responded correctly (even if unrewarded)
  #   int<lower=-1,upper=1> rwd[N,T];		//Matrix of integers containing the reward received on a given trial (1 or 0) — (rows: participants, columns : trials)
  #   int<lower=1,upper=levels> congruence[N,T]; //The congruence of the stimuli: should be integers from 1 to levels
  #   
  #   matrix[2,levels] Vinits;		//Matrix of reals containing the initial q-values for left and right for each congruence level - not used in this model;
  #}
  
  #remove any repeated trials (removes first instance)
  rbias_dups_idx<-which(duplicated(data%>%select(id,trial_nr),fromLast=TRUE))
  if (length(rbias_dups_idx)>0){
    data<-data[-c(rbias_dups_idx),]
  }
  
  choice=data%>%
    select(said_right, id, trial_nr)%>%
    mutate(said_right = ifelse(said_right==1, 2, 1)) %>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = said_right)%>%
    mutate(id=NULL)
  
  accuracy=data%>%
    select(accuracy, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = accuracy)%>%
    mutate(id=NULL)
  
  rwd=data%>%
    select(reward, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = reward)%>%
    mutate(id=NULL)
  
  congruence=data%>%
    mutate(coherence = case_when(
      coherence==0.8 & target_right==0 ~ 1,
      coherence==0.6 & target_right==0 ~ 2,
      coherence==0.5 ~ 3, 
      coherence==0.6 & target_right==1 ~ 4,
      coherence==0.8 & target_right==1 ~ 5))%>%
    select(coherence, id, trial_nr)%>%
    pivot_wider(id_cols = id, names_from = trial_nr,values_from = coherence)%>%
    mutate(id=NULL)
  

  
  out<-list(N=length(unique(data$id)),
            T=max(data$trial_nr+1),
            levels=5,
            choice=choice,
            accuracy=accuracy,
            rwd=rwd,
            congruence=congruence,
            Vinits=matrix(c(rep(0,10)),nrow = 2))
  
  return(out)
  
  
}

kalman_filter = function(df, q_initial = 100, q_safe = .0001, q_risky = 16){
  ## s1 and s2 are variances, not SDs.
  N = nrow(df)
  Q = zeros(N, 2) + q_safe
  # Initial gains
  Q[df$opt_left == 'Risky', 1] = q_risky
  Q[df$opt_right == 'Risky', 2] = q_risky
  
  M = zeros(N, 2)
  S = zeros(N, 2)
  
  chose_right = ifelse(df$response == 'right', 2, 1)
  
  for(i in 1:N){
    # initialization at the start of each block
    if(i == 1 || (df$block_nr[i] != df$block_nr[i-1])){
      m = c(0, 0);      # posterior mean
      s = c(q_initial, q_initial);  # posterior variance
    }
    
    choice = chose_right[i]
    reward = df$reward[i]
    
    # store latents
    M[i,] = m
    S[i,] = s
    
    # update
    k = s[choice] / (s[choice] + Q[i, choice])    # Kalman gain
    err = reward - m[choice];            # prediction error
    m[choice] = m[choice] + k*err;       # posterior mean
    s[choice] = s[choice] - k*s[choice];      # posterior variance
    
  }
  latents = data.frame(
    m1 = M[,1], m2 = M[,2],
    s1= S[,1], s2 = S[,2]) %>%
    mutate(kalman_value_difference = m2 - m1,
           kalman_sigma_difference = s2 - s1,
           kalman_total_uncertainty = sqrt(s1 + s2),
           kalman_weighted_value_difference = kalman_value_difference / kalman_total_uncertainty)
  return(latents)
}

zeros = function(nrow, ncol){
  matrix(0, nrow, ncol)
}

binomial_smooth = function(link='probit', ...){
  geom_smooth(method='glm', method.args = list(family=binomial(link=link)), ...)
}

lgnd = function(x, y){
  theme(legend.position=c(x, y), legend.justification = c(x, y))
}

no_legend = function(){
  theme(legend.position = 'none')
}

mkdir = function(dir){
  dir.create(dir, showWarnings = F, recursive = T)
}

tilt_x_ticks = function(angle=45, vjust=1, hjust=1){
  theme(axis.text.x = element_text(angle=angle, vjust=vjust, hjust=hjust))
}

integrated_bic<-function(ll,ntrials,nparam) {
  -2*ll+nparam*log(ntrials)
}

softmax <- function(x) {
  exp_x <- exp(x - max(x))  # for numerical stability
  return(exp_x / sum(exp_x))
}

pow <- function(x,n){
  return(x^n)
}
