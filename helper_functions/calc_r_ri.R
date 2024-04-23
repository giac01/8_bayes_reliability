
# Added probability of direction (pd)

calc_r_ri = function(
    input_model
){
  
  suppressWarnings({
    draws_wide = input_model$draws() %>%
      posterior::as_draws_df() %>%
      select(starts_with("A[")) %>%
      t() %>%
      data.frame() 
  })
  
  col_select = sample(1:ncol(draws_wide), replace = F)
  draws_wide_1 = draws_wide[col_select[1:(length(col_select)/2)]]  
  draws_wide_2 = draws_wide[col_select[(length(col_select)/2+1):length(col_select)]] 
  
  cors = sapply(1:length(draws_wide_1), function(i) cor(draws_wide_1[,i],draws_wide_2[,i]))
  
  hdci = ggdist::mean_hdci(cors, .width = 0.95)
  
  pd = bayestestR::p_direction(cors)$pd
  
  return(list(hdci = hdci, pd = pd))
}

