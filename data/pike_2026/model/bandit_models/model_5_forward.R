bandit_model5<-function(task,params){

  N=nrow(params)
  unchosen=matrix(data=c(2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3), nrow=4)
  choice=matrix(data=NA,nrow=N,ncol=nrow(task[task$id==1,]))
  
  #remove duplicate trials
  bandit_dups_idx<-which(duplicated(task%>%select(id,trial_nr),fromLast=TRUE))
  if (length(bandit_dups_idx)>0){
    task<-task[-c(bandit_dups_idx),]
  }
  for (i in 1:N) {
    
    No=4 
    initV=c(0.5,0.5,0.5,0.5)
    
    v = initV
    v_rwd = initV
    v_plt = initV
    peR = c(0,0,0,0)
    peP = c(0,0,0,0)
    
    
    task_temp = task[task$id==i,]
    params_temp = params[params$id==i,]
    
    invtemp=1/params_temp$bandit_tau
  
    
    for (t in 1:nrow(task_temp)) {
      if (nrow(task_temp)<1){
        break
      }
      choice[i,t] = sample(1:4, 1, prob=softmax(invtemp * v ))
      peR[choice[i,t]] = task_temp$is_gain[t] - v_rwd[choice[i,t]]
      peP[choice[i,t]] = -abs(task_temp$is_loss[t]) - v_plt[choice[i,t]]
      
      if (peR[choice[i,t]] > 0) { 
        v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + params_temp$bandit_lrPosC * peR[choice[i,t]];
      }
      else { 
        v_rwd[choice[i,t]] = v_rwd[choice[i,t]] + params_temp$bandit_lrNegC * peR[choice[i,t]];
      }
      if (peP[choice[i,t]] > 0) { 
        v_plt[choice[i,t]] = v_plt[choice[i,t]] + params_temp$bandit_lrPosC * peP[choice[i,t]];
      }
      else { 
        v_plt[choice[i,t]] = v_plt[choice[i,t]] + params_temp$bandit_lrNegC * peP[choice[i,t]];
      }
      
      for (i_unchosen in 1:(No-1)) {
        peR[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_rwd[unchosen[choice[i,t],i_unchosen]];
        peP[unchosen[choice[i,t],i_unchosen]] = 0.0 - v_plt[unchosen[choice[i,t],i_unchosen]];
        
        if (peR[unchosen[choice[i,t],i_unchosen]] > 0) { 
          v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + params_temp$bandit_lrPosU * peR[unchosen[choice[i,t],i_unchosen]];
        }
        else { 
          v_rwd[unchosen[choice[i,t],i_unchosen]] = v_rwd[unchosen[choice[i,t],i_unchosen]] + params_temp$bandit_lrNegU * peR[unchosen[choice[i,t],i_unchosen]];
        }
        if (peP[unchosen[choice[i,t],i_unchosen]] > 0) { 
          v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + params_temp$bandit_lrPosU * peP[unchosen[choice[i,t],i_unchosen]];
        }
        else { 
          v_plt[unchosen[choice[i,t],i_unchosen]] = v_plt[unchosen[choice[i,t],i_unchosen]] + params_temp$bandit_lrNegU * peP[unchosen[choice[i,t],i_unchosen]];
        }
      }
      
      v = v_rwd + v_plt;
    } 
    task$response[task$id==i]=choice[i,]
  }
  return(task)
}