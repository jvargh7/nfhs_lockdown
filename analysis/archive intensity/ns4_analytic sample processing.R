source("analysis/nla_analytic sample processing.R")
rm(analytic_svy,analytic2_svy)

spline_svy = analytic_sample %>% 
  mutate(exposure_estimate_gt10 = case_when(exposure_estimate >= 1.0 ~ exposure_estimate - 1.0,
                                            TRUE ~ 0),
         exposure_estimate_gt20 = case_when(exposure_estimate >= 2.0 ~ exposure_estimate - 2.0,
                                            TRUE ~ 0),
         exposure_estimate_gt30 = case_when(exposure_estimate >= 3.0 ~ exposure_estimate - 3.0,
                                            TRUE ~ 0)
         ) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

