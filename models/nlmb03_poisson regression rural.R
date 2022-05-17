require(survey)
require(splines)

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  dplyr::filter(m_rural == 1) %>% 
  mutate(sdistri = factor(sdistri))

analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

# Stunting -----------

glm_stunting <- svyglm(c_stunting ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                         e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + c_male + 
                         m_caste + m_religion + m_age + m_education + factor(sdistri) + factor(c_month),design = analytic_survey,
                       family = poisson())

glm_underweight <- svyglm(c_underweight ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                            e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + c_male + 
                            m_caste + m_religion + m_age + m_education + factor(sdistri) + factor(c_month),design = analytic_survey,
                          family = poisson())

glm_wasting <- svyglm(c_wasting ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                        e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + c_male + 
                        m_caste + m_religion + m_age + m_education + factor(sdistri) + factor(c_month),design = analytic_survey,
                      family = poisson())

summary_poisson = bind_rows(
  broom::tidy(glm_stunting) %>% mutate(outcome = "Stunting"),
  broom::tidy(glm_underweight) %>% mutate(outcome = "Underweight"),
  broom::tidy(glm_wasting) %>% mutate(outcome = "Wasting")) %>% 
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::filter(str_detect(term,"^e(1|2)")) 

summary_poisson %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"paper/nlmb03_poisson outcomes x economic shocks in rural.csv")

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

write_csv(marginal_predictions,paste0("paper/nlmb03_marginal predictions from poisson for rural.csv"))

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
  write_csv(.,"paper/nlmb03_prediction for unique months and year for rural.csv")

