require(survey)
require(splines)

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

analytic_sample %>% 
  mutate(month = month(c_month)) %>% 
  group_by(month,nfhs5,phase,sum_e2_d) %>% 
  tally() %>% 
  View()
