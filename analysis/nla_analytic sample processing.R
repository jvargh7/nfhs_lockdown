analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  mutate(exposure_estimate = case_when(is.na(exposure_estimate) ~ 0,
                                       TRUE ~ exposure_estimate)) %>% 
  
  mutate(p1_estimate = case_when(p1_n < 90 ~ 0,
                                 TRUE ~ p1_estimate),
         p2_estimate = case_when(p2_n < 90 ~ 0,
                                 TRUE ~ p2_estimate),
         p3_estimate = case_when(p3_n < 90 ~ 0,
                                 TRUE ~ p3_estimate),
         p4_estimate = case_when(p4_n < 90 ~ 0,
                                 TRUE ~ p4_estimate),
         p5_estimate = case_when(p5_n < 90 ~ 0,
                                 TRUE ~ p5_estimate)
         )




analytic_svy <- analytic_sample %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
