source("main analysis/nlma_analytic sample.R")

analytic_sample <- nlma_analytic_sample

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


dates_exp <- analytic_sample %>% 
  dplyr::filter(e_interaction > 1) %>% 
  dplyr::select(c_dob,phase,ends_with("_d"),-contains("sum")) %>%
  pivot_longer(cols=-one_of("c_dob","phase"),names_to="exp_period",values_to="value") %>% 
  dplyr::filter(value!=0) %>% 
  group_by(exp_period,phase) %>% 
  dplyr::summarize(min = min(c_dob),
                   max = max(c_dob),
                   n = sum(value) %>% as.character(.)) 

dates_unexp1 <- analytic_sample %>% 
  
  dplyr::filter(is.na(phase)|(e1_p1_d == 0 & e1_p2_d == 0 & e1_p3_d == 0 & e1_p4_d == 0 & e1_p5_d == 0)) %>% 
  group_by(phase) %>% 
  summarize(min = min(c_dob),
            max = max(c_dob),
            n = n() %>% as.character(.)) %>% 
  mutate(exp_period = "unexp_e1")

dates_unexp2 <- analytic_sample %>% 
  dplyr::filter(e2_p1_d == 0, e2_p2_d == 0, e2_p3_d == 0, e2_p4_d == 0, e2_p5_d == 0) %>% 
  group_by(phase) %>% 
  summarize(min = min(c_dob),
            max = max(c_dob),
            n = n() %>% as.character(.)) %>% 
  mutate(exp_period = "unexp_e2")

dates <- bind_rows(dates_exp,
          dates_unexp1,
          dates_unexp2) %>% 
  mutate(daterange = paste0(format(min,"%d-%m-%Y")," to ",format(max,"%d-%m-%Y"))) %>% 
  dplyr::select(exp_period,phase,daterange,n) %>%
  mutate(phase = case_when(is.na(phase) ~ "N4",
                           TRUE ~ paste0("N5 Phase",phase))) %>% 
  pivot_longer(cols=-one_of("exp_period","phase"),names_to="var",values_to="val") %>% 
  pivot_wider(names_from = exp_period,values_from=val)





dates %>% 
  write_csv(.,"paper/table_counts by period of exposure.csv")

