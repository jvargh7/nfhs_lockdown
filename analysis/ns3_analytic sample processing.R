
source("analysis/nlg_analytic sample processing.R")

cluster_covariates <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5 cluster covariates.RDS")) %>% 
  dplyr::select(v001,icds,bpl,bpl_card)


c1_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G1",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

c2_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G2",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


c3_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G3",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


c4_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G4",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


c5_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G5",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


c6_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G6",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>%  
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


c7_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G7",m_rural == 1) %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

