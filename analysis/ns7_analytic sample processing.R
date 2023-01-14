source("analysis/nla_analytic sample processing.R")
rm(analytic_svy,analytic2_svy)

cluster_covariates <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5 cluster covariates.RDS")) %>% 
  dplyr::select(v001,icds,bpl,bpl_card)
# Varible 'b4': 1: Male, 2: Female

cluster_svy <- analytic_sample %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

cluster2_svy <- analytic_sample %>% 
  left_join(cluster_covariates,
            by=c("v001"="v001")) %>%
  dplyr::filter(age_le24 ==1 | age_ge36 == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")