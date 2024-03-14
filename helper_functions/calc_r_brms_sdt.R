calc_r_brms_sdt = function(
    input_model
){
  # browser()
  # tidybayes::get_variables(input_model)
  
  draws = input_model %>%
    tidybayes::spread_draws(r_pps[pps,beta])
  
  if (!identical(names(table(draws$beta)),c("cond"))) warning("Failed data check")
  # if (identical(names(table(draws$beta)) %in% c("Intercept"))) warning("Failed data check")
  
  
  draws_wide = draws %>%
    # select(-starts_with(".")) %>%
    filter(beta == "cond") %>%
    pivot_wider(id_cols = pps, values_from = r_pps, names_from = .draw) %>%
    ungroup() %>%
    dplyr::select(-pps)
  
  col_select = sample(1:ncol(draws_wide), replace = F)
  draws_wide_1 = draws_wide[col_select[1:(length(col_select)/2)]]  
  draws_wide_2 = draws_wide[col_select[(length(col_select)/2+1):length(col_select)]] 
  
  cors = sapply(1:length(draws_wide_1), function(i) cor(draws_wide_1[,i],draws_wide_2[,i]))
  
  cors_hcdi = ggdist::mean_hdci(cors)
  
  return(cors_hcdi)
}