
overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

summary_e_interaction <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  group_by(e_interaction) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))


readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  dplyr::filter(e_interaction == 1) %>% 
  group_by(year(c_dob)) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))
  


nlsa_outcomes <- left_join(read_csv("sensitivity additive/nlsa01u_summary poisson regression urban.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Urban_",.)),
                             read_csv("sensitivity additive/nlsa01r_summary poisson regression rural.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Rural_",.)),
                             by = "term") %>% 
  arrange(term)




write_csv(nlsa_outcomes,("paper/nlsa_coefficients.csv"))




                             