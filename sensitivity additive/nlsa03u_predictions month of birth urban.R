# Marginal date of birth trends --------------
require(lubridate)
source("models/delta_method.R")
overlaps <- readRDS("data/overlaps.RDS")  %>% 
  dplyr::select(dates,ends_with("_d")) %>% 
  mutate(month = month(dates),
         year = year(dates)) %>% 
  distinct_at(vars(month,year,ends_with("_d")),.keep_all = TRUE)

pred_overlaps <- map_dfr(1:nrow(overlaps), function(i){
  print(i);
  e_df = analytic_sample %>% 
    dplyr::select(-ends_with("_d")) %>% 
    bind_cols(overlaps[i,-1])
  
  
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

bind_cols(overlaps,pred_overlaps) %>% 
  write_csv(.,"models/nlwb02_prediction for unique months and year for urban.csv")
