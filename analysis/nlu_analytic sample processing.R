u1_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G1",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

u2_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G2",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


u3_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G3",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


u4_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G4",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


u5_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G5",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


u6_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G6",m_rural == 0) %>%  
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


u7_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G7",m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
