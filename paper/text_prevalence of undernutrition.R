analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS"))
require(srvyr)

as_svydesign <- analytic_sample %>% 
  mutate_at(vars("c_stunting","c_underweight","c_wasting","c_overweight","m_alcohol","m_smoking","m_rural"),
            function(x) case_when(x == 1 ~ "yes",
                                  x == 0 ~ "no",
                                  TRUE ~ "no")) %>%
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(
    phase = factor(phase,levels=c(0,1,2),labels=c("NFHS4","Pre-lockdown","Post-lockdown"))) %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

group_variables <- c("c_stunting","c_underweight","c_wasting","c_overweight","m_rural","m_weightstatus")
as_groups <- map_dfr(group_variables,
                     function(g){
                       
                       as_svydesign %>% 
                         group_by_at(vars(phase,g)) %>% 
                         summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                         rename(group = g) %>% 
                         mutate(variable = g)
                       
                     }) %>% 
  dplyr::filter(!is.na(proportion)) 

as_groups %>% 
  dplyr::filter(group == "yes"|group == "Overweight or Obese") %>% 
  mutate_if(is.numeric,~.*100) %>% 
  mutate(value = paste0(round(proportion,1)," (",
                        round(proportion_low,1),", ",
                        round(proportion_upp,1),")")) %>% 
  dplyr::select(variable,group,phase,value) %>% 
  pivot_wider(names_from=phase,values_from=value) 
