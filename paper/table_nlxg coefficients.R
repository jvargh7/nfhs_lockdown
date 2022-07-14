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

summary_e_interaction_region <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,c_male == 0) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  group_by(e_interaction,year_categories,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n()) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  arrange(m_rural,term2)

write_csv(summary_e_interaction_region,("paper/summary girls urban rural counts by exposure.csv"))


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




