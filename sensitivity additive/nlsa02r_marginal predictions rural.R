# Marginal standardization -------------

exposures <- c("e1_p1_d","e1_p2_d","e1_p3_d","e1_p4_d","e1_p5_d","e2_p1_d","e2_p2_d","e2_p3_d","e2_p4_d","e2_p5_d")

marginal_predictions <- map_dfr(exposures,function(e){
  print(e);
  e0_df = analytic_sample %>% 
    mutate_at(vars(one_of(e)),function(x) case_when(TRUE ~ 0));
  e1_df = analytic_sample %>% 
    mutate_at(vars(one_of(e)),function(x) case_when(TRUE ~ 1));
  
  
  pred_e0_stunting = predict(glm_stunting,newdata=e0_df,type="response") 
  mean_pred_e0_stunting = Hmisc::wtd.mean(pred_e0_stunting,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e0_stunting = sqrt(Hmisc::wtd.var(pred_e0_stunting,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  pred_e1_stunting = predict(glm_stunting,newdata=e1_df,type="response");
  mean_pred_e1_stunting = Hmisc::wtd.mean(pred_e1_stunting,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e1_stunting = sqrt(Hmisc::wtd.var(pred_e1_stunting,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  pred_e0_underweight = predict(glm_underweight,newdata=e0_df,type="response");
  mean_pred_e0_underweight = Hmisc::wtd.mean(pred_e0_underweight,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e0_underweight = sqrt(Hmisc::wtd.var(pred_e0_underweight,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  pred_e1_underweight = predict(glm_underweight,newdata=e1_df,type="response");
  mean_pred_e1_underweight = Hmisc::wtd.mean(pred_e1_underweight,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e1_underweight = sqrt(Hmisc::wtd.var(pred_e1_underweight,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  pred_e0_wasting = predict(glm_wasting,newdata=e0_df,type="response");
  mean_pred_e0_wasting = Hmisc::wtd.mean(pred_e0_wasting,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e0_wasting = sqrt(Hmisc::wtd.var(pred_e0_wasting,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  pred_e1_wasting = predict(glm_wasting,newdata=e1_df,type="response");
  mean_pred_e1_wasting = Hmisc::wtd.mean(pred_e1_wasting,weights = e0_df$combined_sampleweight,normwt = TRUE);
  sd_pred_e1_wasting = sqrt(Hmisc::wtd.var(pred_e1_wasting,weights = e0_df$combined_sampleweight,normwt = TRUE));
  
  data.frame(exposure = e,
             mean_pred_e0_stunting = mean_pred_e0_stunting,
             mean_pred_e1_stunting = mean_pred_e1_stunting,
             mean_pred_e0_underweight = mean_pred_e0_underweight,
             mean_pred_e1_underweight = mean_pred_e1_underweight,
             mean_pred_e0_wasting = mean_pred_e0_wasting,
             mean_pred_e1_wasting = mean_pred_e1_wasting,
             marginal_stunting = (mean_pred_e0_stunting - mean_pred_e1_stunting),
             marginal_underweight = (mean_pred_e0_underweight - mean_pred_e1_underweight),
             marginal_wasting = (mean_pred_e0_wasting - mean_pred_e1_wasting),
             sd_pred_e0_stunting = sd_pred_e0_stunting,
             sd_pred_e1_stunting = sd_pred_e1_stunting,
             sd_pred_e0_underweight = sd_pred_e0_underweight,
             sd_pred_e1_underweight = sd_pred_e1_underweight,
             sd_pred_e0_wasting = sd_pred_e0_wasting,
             sd_pred_e1_wasting = sd_pred_e1_wasting,
             sd_marginal_stunting = sqrt(sd_pred_e0_stunting^2 + sd_pred_e1_stunting^2),
             sd_marginal_underweight = sqrt(sd_pred_e0_underweight^2 + sd_pred_e1_underweight^2),
             sd_marginal_wasting = sqrt(sd_pred_e0_wasting^2 + sd_pred_e1_wasting^2)) %>% 
    return(.)
  
  
})

write_csv(marginal_predictions,paste0("sensitivity additive/nlsa02r_marginal predictions from poisson for rural.csv"))
