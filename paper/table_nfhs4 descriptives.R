nfhs4_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs4_exposure.RDS"))  %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz))
require(srvyr)

nfhs4_svydesign <- nfhs4_exposure %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024,nest=TRUE,weights = sampleweight,
                   variance = "YG",pps = "brewer")

continuous_variables <- c("c_haz","c_waz","c_whz","m_bmi")
nfhs4_continuous <- nfhs4_svydesign %>% 
  summarize_at(vars(one_of(continuous_variables)),
               .funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural",
                     "m_caste","m_wealthq","m_religion","m_education",
                     "m_weightstatus")
nfhs4_groups <- map_dfr(group_variables,
                                 function(g){
                                   
                                   nfhs4_svydesign %>% 
                                     group_by_at(g) %>% 
                                     summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                                     rename(group = g) %>% 
                                     mutate(variable = g)
                                   
                                 }) %>% 
  dplyr::filter(!is.na(proportion)) 

nfhs4_counts <- nfhs4_exposure %>% 
  summarize_at(vars(continuous_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


bind_rows(nfhs4_continuous,
          nfhs4_groups %>% 
            mutate_if(is.numeric,~.*100)) %>% 
  left_join(nfhs4_counts,
            by=c("variable")) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  write_csv(.,"paper/table_nfhs4 descriptives.csv")
