analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

# Study population (unweighted) -----------
analytic_sample %>% 
       dplyr::filter(phase == 2) %>% 
       dplyr::select(v024_nfhs5) %>% 
       pull() %>% 
  unique(.) %>% 
  as.numeric(.)


analytic_sample %>% 
  group_by(nfhs5,phase) %>% 
  dplyr::summarise(min_date = min(c_dob),
                   max_date = max(c_dob),
                   n = n())



# Sample exposed during sensitive period (unweighted) ------------


dates <- analytic_sample %>% 
  dplyr::select(c_dob,phase,ends_with("_d")) %>%
  pivot_longer(cols=-one_of("c_dob","phase"),names_to="exp_period",values_to="value") %>% 
  dplyr::filter(value!=0) %>% 
  group_by(exp_period,phase) %>% 
  dplyr::summarize(min = min(c_dob),
                   max = max(c_dob),
                   n = sum(value) %>% as.character(.)) %>% 
  mutate(daterange = paste0(format(min,"%d-%m-%Y")," to ",format(max,"%d-%m-%Y"))) %>% 
  dplyr::select(exp_period,phase,daterange,n) %>%
  mutate(phase = case_when(is.na(phase) ~ "N4",
                           TRUE ~ paste0("N5 Phase",phase))) %>% 
  pivot_longer(cols=-one_of("exp_period","phase"),names_to="var",values_to="val") %>% 
  pivot_wider(names_from = exp_period,values_from=val)

dates %>% 
  write_csv(.,"paper/table_counts by period of exposure.csv")

