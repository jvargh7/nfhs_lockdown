require(lubridate)
overlaps_unique <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_overlaps_unique.RDS")) 

summary_e_interaction <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>% 
  group_by(e_interaction) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))

summary_e_interaction_region <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2018) ~ "2018",
                                     year(c_dob) %in% c(2017) & month(c_dob) %in% c(11:12) ~ "2018",
                                     year(c_dob) %in% c(2017) & month(c_dob) %in% c(3) ~ "2017",
                                     year(c_dob) %in% c(2016) ~ "2016",
                                     year(c_dob) %in% c(2015) & month(c_dob) %in% c(4:12) ~ "2015 P2",
                                     year(c_dob) %in% c(2015) & month(c_dob) %in% c(1:3) ~ "2010-15 P1",
                                     TRUE ~ "2010-15 P1")) %>% 
  group_by(e_interaction,year_categories,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n()) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:15),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,6:2,15:7)))) %>% as.character(.)) %>% 
  arrange(m_rural,term2)

write_csv(summary_e_interaction_region,("paper/nlse urban rural counts by exposure.csv"))


readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>%
   dplyr::filter(e_interaction == 1) %>%
  group_by(year(c_dob)) %>%
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))



nlse_outcomes <- left_join(read_csv("sensitivity 120d/nlse01u_summary poisson regression urban.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Urban_",.)),
                             read_csv("sensitivity 120d/nlse01r_summary poisson regression rural.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Rural_",.)),
                             by = "term") %>% 
  mutate(term = as.numeric(str_replace(term,"factor\\(e_interaction\\)",""))) %>% 
  left_join(summary_e_interaction,
            by=c("term" = "e_interaction")) %>% 
  mutate(term2 = factor(term,labels=paste0("Table Order ",sprintf("%02d",c(6:2,15:7)))) %>% as.character(.)) %>% 
  arrange(term2)




write_csv(nlse_outcomes,("paper/nlse_coefficients.csv"))




