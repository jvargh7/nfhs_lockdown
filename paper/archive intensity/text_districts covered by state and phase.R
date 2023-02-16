district_coverage <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  group_by(phase,nfhs5_state,sdist) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(sdist) %>% 
  mutate(prop = n/sum(n)) 

state_coverage = district_coverage %>% 
  # dplyr::filter(n = max(n)) %>% 
  group_by(phase,nfhs5_state) %>% 
  summarize(n_dist = sum(prop > 0))

phase_coverage <- district_coverage %>% 
  group_by(sdist) %>% 
  mutate(type = case_when(n() == 1 ~ "Completed",
                          n()>1 ~ "Partial")) %>% 
  ungroup() %>% 
  group_by(phase,type) %>% 
  summarize(sum_n = n())
