
excluded_states <- readRDS(paste0(path_lockdown_folder,"/working/excluded_states.RDS"))
require(srvyr)

es_svydesign <- excluded_states %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = sampleweight,
                   variance = "YG",pps = "brewer")

continuous_variables <- c("c_haz","c_waz","c_whz","m_age","m_height","m_bmi")
es_continuous <- es_svydesign %>% 
  group_by(nfhs5) %>% 
  summarize_at(vars(one_of(continuous_variables)),
               .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=-nfhs5) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking",
                     "m_caste","m_wealthq","m_religion","m_education","m_rural",
                     "m_weightstatus")
es_groups <- map_dfr(group_variables,
                        function(g){
                          
                          es_svydesign %>% 
                            group_by_at(vars(one_of("nfhs5",g))) %>% 
                            summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                            rename(group = g) %>% 
                            mutate(variable = g)
                          
                        }) %>% 
  dplyr::filter(!is.na(proportion)) 

es_counts <- excluded_states %>% 
  mutate(m_rural = case_when(m_rural == 1 ~ "yes",
                             m_rural == 0 ~ "no",
                             TRUE ~ "no")) %>% 
  group_by(nfhs5) %>% 
  summarize_at(vars(continuous_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-nfhs5) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


bind_rows(es_continuous,
          es_groups %>% 
            mutate_if(is.numeric,~.*100)) %>% 
  left_join(es_counts,
            by=c("variable","nfhs5")) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  dplyr::select(nfhs5,variable,value,group) %>% 
  arrange(nfhs5) %>% 
  pivot_wider(names_from=nfhs5,values_from=value) %>% 
  write_csv(.,"paper/table_excluded states descriptives.csv")
