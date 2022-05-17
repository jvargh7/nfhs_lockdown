require(survey)
require(splines)

overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  dplyr::filter(m_rural == 1)


analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

glm_stunting <- svyglm(c_stunting ~ ns(c_age,df=4) + factor(e_interaction) + nfhs5 + 
                         m_wealthq + m_caste + m_wealthq + m_religion + m_age + m_education + m_alcohol + m_smoking + factor(v024_nfhs5) + factor(c_month),design = analytic_survey,
                       family = poisson())

glm_underweight <- svyglm(c_underweight ~ ns(c_age,df=4) + factor(e_interaction) + nfhs5 + 
                            m_wealthq + m_caste + m_wealthq + m_religion + m_age + m_education + m_alcohol + m_smoking + factor(v024_nfhs5) + factor(c_month),design = analytic_survey,
                          family = poisson())

glm_wasting <- svyglm(c_wasting ~ ns(c_age,df=4) + factor(e_interaction) + nfhs5 + 
                        m_wealthq + m_caste + m_wealthq + m_religion + m_age + m_education + m_alcohol + m_smoking + factor(v024_nfhs5) + factor(c_month),design = analytic_survey,
                      family = poisson())


# Stunting -----------


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
  dplyr::filter(str_detect(term,"factor\\(e"))

summary_poisson %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"models with sequential interactions/nlsb03_poisson outcomes x economic shocks in rural.csv")

e_interaction_level <- c(2:19)

marginal_predictions <- map_dfr(e_interaction_level,function(e){
  print(e);
  e0_df = analytic_sample %>% 
    mutate_at(vars(e_interaction),function(x) case_when(TRUE ~ 1));
  e1_df = analytic_sample %>% 
    mutate_at(vars(e_interaction),function(x) case_when(TRUE ~ e));
  
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

write_csv(marginal_predictions,paste0("models with sequential interactions/nlsb03_marginal predictions from poisson for rural.csv"))

# Marginal date of birth trends --------------
require(lubridate)
source("models/delta_method.R")
overlaps <- readRDS("data/overlaps.RDS")  %>% 
  dplyr::select(dates,ends_with("_d")) %>% 
  mutate(month = month(dates),
         year = year(dates)) %>% 
  distinct_at(vars(month,year,ends_with("_d")),.keep_all = TRUE) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11]))

pred_overlaps <- map_dfr(1:nrow(overlaps), function(i){
  print(i);
  e_df = analytic_sample %>% 
    dplyr::select(-ends_with("_d"),-e_interaction) %>% 
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
  write_csv(.,"models with sequential interactions/nlsb03_prediction for unique months and year for rural.csv")
