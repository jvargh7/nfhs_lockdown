analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  mutate_at(vars(ends_with("estimate")),~ case_when(is.na(.) ~ 0,
                                       TRUE ~ .))



analytic_svy <- analytic_sample %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
