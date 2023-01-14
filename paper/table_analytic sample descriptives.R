# source("analysis/nla_analytic sample processing.R")

descriptive_svy <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer") %>% 
  # Per 10pp change
  mutate_at(vars(ends_with("estimate")),~ case_when(is.na(.) ~ 0,
                                                    TRUE ~ .*-(1))) %>% 
  # Per 30day change
  mutate_at(vars(ends_with("gt20")),~case_when(is.na(.) ~ 0,
                                               TRUE ~ .))


source("C:/code/external/functions/survey/svysummary.R")

c_vars = c("c_haz","c_waz","c_whz","c_bmiz","m_age","m_height","m_bmi","m_eduyr",
           "exposure_estimate","p1_estimate","p2_estimate","p3_estimate","p4_estimate",
           "exposure_gt20","p1_gt20","p2_gt20","p3_gt20","p4_gt20")
p_vars = c("c_stunting","c_underweight","c_wasting","c_overweight")
g_vars = c("m_caste","hh_wealthqur","hh_religion","m_education","m_weightstatus")

id_vars = list(c("m_rural","phase"),
               c("phase"))

# Total -------

table_output <- map_dfr(id_vars,
                        
                        function(i_v){
                          
                          n5_sy <- svysummary(descriptive_svy,
                                              c_vars = c_vars,
                                              p_vars = p_vars,
                                              g_vars = g_vars,
                                              id_vars = i_v
                          ) %>% 
                            mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                            mutate(est_ci = paste0(estimate," (",
                                                   lci,", ",uci,")"));
                          
                          # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                          n5_ct <- analytic_sample %>% 
                            group_by_at(vars(one_of(i_v))) %>%
                            summarize_at(vars(one_of(c(
                              c_vars,
                              p_vars,
                              g_vars
                            ))),
                            list(n = ~sum(!is.na(.)))) %>% 
                            pivot_longer(names_to="variable",values_to="n",
                                         cols=-one_of(i_v)
                            ) %>% 
                            mutate(variable = str_replace(variable,"_n$",""));
                          
                          n5_out <- left_join(n5_sy,
                                              n5_ct,
                                              by=c(
                                                i_v[i_v!=""],
                                                "variable"));
                          
                          return(n5_out)
                          
                          
                        }
                        
                        )


table_output %>% 
  mutate(m_rural = case_when(is.na(m_rural) ~ "Total",
                             m_rural == 1 ~ "Rural",
                             m_rural == 0 ~ "Urban"),
         phase = case_when(phase == 1 ~ "2019-20",
                           phase == 2 ~ "2020-21")
         ) %>% 
  dplyr::select(m_rural,phase,variable,group,est_ci) %>% 
  pivot_wider(names_from=c("m_rural","phase"),values_from="est_ci") %>% 
  
  write_csv(.,"paper/table_analytic sample descriptives.csv")





