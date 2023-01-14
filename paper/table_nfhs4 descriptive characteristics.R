dist5 <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  distinct(phase,sdist)

phase1 = dist5 %>% dplyr::filter(phase == 1) %>% dplyr::select(sdist) %>% pull()
phase2 = dist5 %>% dplyr::filter(phase == 2) %>% dplyr::select(sdist) %>% pull()


psu4 <- read_csv("C:/code/external/nfhs4_on_map2020/data/psu_on_mapnfhs5.csv") %>% 
  mutate(phase_in_nfhs5 = case_when(REGCODE %in% phase1 & REGCODE %in% phase2 ~ "Mixed",
                           REGCODE %in% phase1 ~ "Phase 1",
                           REGCODE %in% phase2 ~ "Phase 2",
                           TRUE ~ "Excluded"))


nfhs4_descriptive_df = readRDS(paste0(path_lockdown_folder,"/working/nfhs4 child.RDS")) %>% 
  dplyr::mutate(included = case_when(v024_nfhs5 %in% v024_nfhs5_14states ~ "Included",
                                     TRUE ~ "Excluded")) %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz) & !is.na(m_height)) %>% 
  left_join(psu4 %>% 
              dplyr::select(DHSCLUST,phase_in_nfhs5),by=c("v001"="DHSCLUST"))

nfhs4_descriptive_svy = nfhs4_descriptive_df %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

source("C:/code/external/functions/survey/svysummary.R")

c_vars = c("c_haz","c_waz","c_whz","c_bmiz","m_age","m_height","m_bmi","m_eduyr")
p_vars = c("c_stunting","c_underweight","c_wasting","c_overweight","m_rural")
g_vars = c("m_caste","hh_wealthqur","hh_religion","m_education","m_weightstatus")

id_vars = list(c("m_rural","included"),
               c("phase_in_nfhs5"),
               c("included"))

table_output <- map_dfr(id_vars,
                        
                        function(i_v){
                          
                          n5_sy <- svysummary(nfhs4_descriptive_svy,
                                              c_vars = c_vars,
                                              p_vars = p_vars,
                                              g_vars = g_vars,
                                              id_vars = i_v
                          ) %>% 
                            mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                            mutate(est_ci = paste0(estimate," (",
                                                   lci,", ",uci,")"));
                          
                          # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                          n5_ct <- nfhs4_descriptive_df %>% 
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
  dplyr::filter(is.na(phase_in_nfhs5)) %>% 
  mutate(m_rural = case_when(is.na(m_rural) ~ "Total",
                             m_rural == 1 ~ "Rural",
                             m_rural == 0 ~ "Urban")
  ) %>% 
  dplyr::select(m_rural,included,variable,group,est_ci) %>% 
  pivot_wider(names_from=c("m_rural","included"),values_from="est_ci") %>% 
  
  write_csv(.,"paper/table_nfhs4 analytic sample descriptives.csv")



table_output %>% 
  dplyr::filter(!is.na(phase_in_nfhs5)) %>% 
  dplyr::select(phase_in_nfhs5,variable,group,est_ci) %>% 
  pivot_wider(names_from=c("phase_in_nfhs5"),values_from="est_ci") %>% 
  
  write_csv(.,"paper/table_nfhs4 analytic sample descriptives based on districts covered.csv")




