analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  # Per 10pp change for statistical model
  mutate_at(vars(ends_with("estimate")),~ case_when(is.na(.) ~ 0,
                                       TRUE ~ .*-(1/10))) %>% 
  # Per 30day change for statistical model
  mutate_at(vars(ends_with("gt20")),~case_when(is.na(.) ~ 0,
                                                TRUE ~ (./30))) %>% 
  # Variables for statistical models only -----------
  mutate(c_age3mo = cut(c_age,breaks = seq(0,60,by=3),include.lowest = TRUE,right=FALSE)) %>% 
  mutate(phase2 = case_when(phase == 2 ~ 1,
                            TRUE ~ 0),
         age_le24 = case_when(c_age <= 24 ~ 1,
                              TRUE ~ 0),
         age_ge36 = case_when(c_age >= 36 ~ 1,
                              TRUE ~ 0)
         ) %>% 
  mutate(phase2_age_le24 = phase2*age_le24,
         exposure_estimate_age_le24 = exposure_estimate*age_le24,
         phase2_age_ge36 = phase2*age_ge36,
         exposure_estimate_categories = cut_interval(exposure_estimate,n=10)
         
         ) %>% 
    mutate(across(.cols=one_of("exposure_estimate","exposure_gt20","m_eduyr","m_height","m_age",
                               "p1_estimate","p2_estimate","p3_estimate","p4_estimate"),
                  .fns = function(x) scale(x,center=TRUE,scale=FALSE),.names="{.col}_centered")) %>% 
  mutate_at(vars(ends_with("lcases"),ends_with("acases")),~case_when(is.na(.) ~ 0,
                                                                     TRUE ~ .))
  

analytic_sample %>% 
  imap_dfr(.,function(v,name){
    data.frame(variable = name,
               observations = sum(!is.na(v)))
    
  }) %>% 
  writexl::write_xlsx(.,"analysis/analytic_sample variables.xlsx")

df = analytic_sample %>% 
  group_by(sdist) %>% 
  tally() 



analytic_svy <- analytic_sample %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

analytic2_svy <- analytic_sample %>%
  dplyr::filter(age_le24 ==1 | age_ge36 == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


# rural_svy <- analytic_sample %>% 
#   dplyr::filter(m_rural == 1) %>% 
#   as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
# 
# urban_svy <- analytic_sample %>% 
#   dplyr::filter(m_rural == 0) %>% 
#   as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
