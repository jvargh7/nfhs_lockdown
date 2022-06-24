excluded_anthro <- readRDS(paste0(path_lockdown_folder,"/working/excluded_anthro.RDS"))
require(srvyr)

ea_svydesign <- excluded_anthro %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>%
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(
    phase = factor(phase,levels=c(0,1,2),labels=c("NFHS4","Pre-lockdown","Post-lockdown"))) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = sampleweight,
                   variance = "YG",pps = "brewer")

continuous_variables <- c("c_haz","c_waz","c_whz","m_age","m_height","m_bmi")
ea_continuous <- ea_svydesign %>% 
  group_by(phase) %>% 
  summarize_at(vars(one_of(continuous_variables)),
               .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(cols=-one_of("phase"),names_to=c("variable","est"),names_sep ="_mean") %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking",
                     "m_caste","m_wealthq","m_religion","m_education","m_rural",
                     "m_weightstatus")
ea_groups <- map_dfr(group_variables,
                        function(g){
                          
                          ea_svydesign %>% 
                            group_by_at(vars(phase,g)) %>% 
                            summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                            rename(group = g) %>% 
                            mutate(variable = g)
                          
                        }) %>% 
  dplyr::filter(!is.na(proportion)) 

ea_counts <- excluded_anthro %>% 
  mutate(m_rural = case_when(m_rural == 1 ~ "yes",
                             m_rural == 0 ~ "no",
                             TRUE ~ "no")) %>% 
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(
    phase = factor(phase,levels=c(0,1,2),labels=c("NFHS4","Pre-lockdown","Post-lockdown"))) %>%  
  group_by(phase) %>% 
  summarize_at(vars(continuous_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of("phase")) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


bind_rows(ea_continuous,
          ea_groups %>% 
            mutate_if(is.numeric,~.*100)) %>% 
  left_join(ea_counts,
            by=c("variable","phase")) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  dplyr::select(variable,phase,value,group) %>% 
  pivot_wider(names_from=phase,values_from=value) %>% 
  write_csv(.,"paper/table_excluded anthro descriptives.csv")
