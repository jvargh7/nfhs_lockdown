
excluded_sample <- readRDS(paste0(path_lockdown_folder,"/working/excluded_sample.RDS"))

require(srvyr)

excluded_svydesign <- excluded_sample %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>% 
  mutate(
    phase = factor(phase,levels=c(1,2),labels=c("Pre-lockdown","Post-lockdown"))) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

continuous_variables <- c("c_haz","c_waz","c_whz","m_bmi")
excluded_continuous <- excluded_svydesign %>% 
  group_by(group) %>% 
  summarize_at(vars(one_of(continuous_variables)),
               .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(cols=-one_of("group"),names_to=c("variable","est"),names_sep ="_mean") %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural",
                     "m_caste","m_wealthq","m_religion","m_education",
                     "m_weightstatus")
excluded_groups <- map_dfr(group_variables,
                        function(g){
                          
                          excluded_svydesign %>% 
                            group_by_at(vars(group,g)) %>% 
                            summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                            rename(category = g) %>% 
                            mutate(variable = g)
                          
                        }) %>% 
  dplyr::filter(!is.na(proportion)) 

excluded_counts <- excluded_sample %>% 
  mutate(phase = factor(phase,levels=c(1,2),labels=c("Pre-lockdown","Post-lockdown"))) %>% 
  group_by(group) %>% 
  summarize_at(vars(continuous_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of("group")) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


bind_rows(excluded_continuous,
          excluded_groups %>% 
            mutate_if(is.numeric,~.*100)) %>% 
  left_join(excluded_counts,
            by=c("variable","group")) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  dplyr::select(variable,group,value,category) %>% 
  pivot_wider(names_from=group,values_from=value) %>% 
  write_csv(.,"sensitivity/sens01_excluded descriptives.csv")
