nfhs5_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5_exposure.RDS")) %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz))
require(srvyr)

nfhs5_svydesign <- nfhs5_exposure %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest")),
         phase = factor(phase,levels=c(1,2),labels=c("Pre-lockdown","Post-lockdown"))) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024,nest=TRUE,weights = sampleweight,
                   variance = "YG",pps = "brewer")

continuous_variables <- c("c_haz","c_waz","c_whz","m_bmi")
nfhs5_continuous <- nfhs5_svydesign %>% 
  group_by(phase) %>% 
  summarize_at(vars(one_of(continuous_variables)),
               .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(cols=-one_of("phase"),names_to=c("variable","est"),names_sep ="_mean") %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural",
                     "m_caste","m_wealthq","m_religion","m_education",
                     "m_weightstatus")
nfhs5_groups <- map_dfr(group_variables,
                        function(g){
                          
                          nfhs5_svydesign %>% 
                            group_by_at(vars(phase,g)) %>% 
                            summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                            rename(group = g) %>% 
                            mutate(variable = g)
                          
                        }) %>% 
  dplyr::filter(!is.na(proportion)) 

nfhs5_counts <- nfhs5_exposure %>% 
  mutate(phase = factor(phase,levels=c(1,2),labels=c("Pre-lockdown","Post-lockdown"))) %>% 
  group_by(phase) %>% 
  summarize_at(vars(continuous_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of("phase")) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


bind_rows(nfhs5_continuous,
          nfhs5_groups %>% 
            mutate_if(is.numeric,~.*100)) %>% 
  left_join(nfhs5_counts,
            by=c("variable","phase")) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  dplyr::select(variable,phase,value,group) %>% 
  pivot_wider(names_from=phase,values_from=value) %>% 
  write_csv(.,"paper/table_nfhs5 descriptives.csv")
