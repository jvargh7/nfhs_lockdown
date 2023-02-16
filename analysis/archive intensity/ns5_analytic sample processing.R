source("analysis/nla_analytic sample processing.R")
rm(analytic_svy,analytic2_svy)

# Varible 'b4': 1: Male, 2: Female

conceived_svy <- analytic_sample %>% 
  dplyr::filter((c_dob - 38.5*7) < lockdown_start) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

conceived2_svy <- analytic_sample %>%
  dplyr::filter(age_le24 ==1 | age_ge36 == 1,((c_dob - 38.5*7) < lockdown_start)) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
