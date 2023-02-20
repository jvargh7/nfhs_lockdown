source(".Rprofile")

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  # Per 10pp change for statistical model
  mutate_at(vars(ends_with("cumulative")),~ case_when(is.na(.) ~ 0,
                                                    TRUE ~ .*-(1/10000))) %>% 
  # Variables for statistical models only -----------
mutate(c_age2mo = cut(c_age,breaks = seq(0,60,by=2),include.lowest = TRUE,right=FALSE) %>% as.character(.)) %>% 
  mutate(phase2 = case_when(phase == 2 ~ 1,
                            TRUE ~ 0),
         
         group = case_when(c_age %in% c(0:6) ~ "G1",
                           c_age %in% c(7:12) ~ "G2",
                           c_age %in% c(13:18) ~ "G3",
                           c_age %in% c(19:24) ~ "G4",
                           c_age %in% c(25:36) ~ "G5",
                           c_age %in% c(37:48) ~ "G6",
                           c_age %in% c(49:60) ~ "G7")
  ) %>% 
  mutate_at(vars(ends_with("lcases"),ends_with("acases")),~case_when(is.na(.) ~ 0,
                                                                     TRUE ~ .))

analytic_sample %>% 
  imap_dfr(.,function(v,name){
    data.frame(variable = name,
               observations = sum(!is.na(v)))
    
  }) %>% 
  writexl::write_xlsx(.,"analysis/analytic_sample variables missingness.xlsx")

source("analysis/nlg_district development.R")

analytic_sample_wdistrict <- analytic_sample %>% 
  left_join(may_zones,
            by=c("sdist"="sdist_nfhs5")) %>% 
  left_join(district_characteristics,
            by=c("sdist"="sdist")) 



g1_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G1") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

g2_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G2") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


g3_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G3") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


g4_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G4") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


g5_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G5") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


g6_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G6") %>%  
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


g7_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(group == "G7") %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
