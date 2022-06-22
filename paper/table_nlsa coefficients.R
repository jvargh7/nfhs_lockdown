

nlsa_outcomes <- left_join(read_csv("sensitivity additive/nlsa01u_summary poisson regression urban.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Urban_",.)),
                             read_csv("sensitivity additive/nlsa01r_summary poisson regression rural.csv") %>% 
                               rename_at(vars(Stunting,Underweight,Wasting),~paste0("Rural_",.)),
                             by = "term") %>% 
  arrange(term)




write_csv(nlsa_outcomes,("paper/nlsa_coefficients.csv"))




                             