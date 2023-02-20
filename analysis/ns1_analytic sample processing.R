source("analysis/nlg_analytic sample processing.R")


f1_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G1",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

f2_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G2",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


f3_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G3",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


f4_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G4",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


f5_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G5",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


f6_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G6",m_rural == 1,c_male == 0) %>%  
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


f7_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G7",m_rural == 1,c_male == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

# Male ----------
m1_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G1",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

m2_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G2",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


m3_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G3",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


m4_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G4",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


m5_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G5",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


m6_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G6",m_rural == 1,c_male == 1) %>%  
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


m7_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G7",m_rural == 1,c_male == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
