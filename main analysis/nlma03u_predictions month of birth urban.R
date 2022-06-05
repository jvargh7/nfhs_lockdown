# Marginal date of birth trends --------------
require(lubridate)
source("functions/delta_method.R")


pred_overlaps <- map_dfr(1:nrow(overlaps_months), function(i){
  print(i);
  e_df = analytic_sample %>% 
    dplyr::select(-ends_with("_d"),-e_interaction) %>% 
    bind_cols(overlaps_months[i,-1]) %>% 
    # CHECK: Does it contrast correctly? ---------------
  mutate(e_interaction = factor(e_interaction,levels=c(1:19)))
  
  
  pred_stunting = delta_method(glm_stunting,pred_df=e_df)
  pred_underweight = delta_method(glm_underweight,pred_df=e_df)
  pred_wasting = delta_method(glm_wasting,pred_df=e_df)
  
  
  
  data.frame(row = i,
             mean_pred_e_stunting = pred_stunting[["mean"]],
             mean_pred_e_underweight = pred_underweight[["mean"]],
             mean_pred_e_wasting = pred_wasting[["mean"]],
             se_pred_e_stunting = pred_stunting[["se"]],
             se_pred_e_underweight = pred_underweight[["se"]],
             se_pred_e_wasting = pred_wasting[["se"]]
  ) %>% 
    return(.)
})

bind_cols(overlaps_months,pred_overlaps) %>% 
  write_csv(.,"main analysis/nlma03u_prediction for unique months and year for urban.csv")
