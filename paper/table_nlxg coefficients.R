require(lubridate)
overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

summary_e_interaction <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,c_male == 0) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction)) %>% 
  group_by(e_interaction) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))


nlxg_outcomes <- left_join(read_csv("sensitivity boys/nlxg01u_summary poisson girls urban.csv") %>% 
                             rename_at(vars(Stunting,Underweight,Wasting),~paste0("Urban_",.)),
                           read_csv("sensitivity boys/nlxg01r_summary poisson girls rural.csv") %>% 
                             rename_at(vars(Stunting,Underweight,Wasting),~paste0("Rural_",.)),
                           by = "term") %>% 
  mutate(term = as.numeric(str_replace(term,"factor\\(e_interaction\\)",""))) %>% 
  left_join(summary_e_interaction,
            by=c("term" = "e_interaction")) %>% 
  mutate(term2 = factor(term,labels=paste0("Table Order ",sprintf("%02d",c(10:2,19:11)))) %>% as.character(.)) %>% 
  arrange(term2)




write_csv(nlxg_outcomes,("paper/nlxg_coefficients.csv"))




