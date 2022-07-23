require(lubridate)
overlaps_unique <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_overlaps_unique.RDS")) 

summary_e_interaction <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>% 
  group_by(e_interaction) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))



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




