source("functions/gentable_coefs_contrasts.R")


gentable_coefs_contrasts("ns1") %>% 
  write_csv("paper/table_ns1 anthropometric z-scores.csv")

gentable_coefs_contrasts("ns2") %>% 
  write_csv("paper/table_ns2 boys undernutrition.csv")

gentable_coefs_contrasts("ns3") %>% 
  write_csv("paper/table_ns3 girls undernutrition.csv")

gentable_coefs_contrasts("ns5") %>% 
  write_csv("paper/table_ns5 conceived before lockdown.csv")


gentable_coefs_contrasts("ns6") %>% 
  write_csv("paper/table_ns6 district adjustment.csv")

gentable_coefs_contrasts("ns7") %>% 
  write_csv("paper/table_ns7 cluster adjustment.csv")

gentable_coefs_contrasts("ns8") %>% 
  write_csv("paper/table_ns8 born before Delta.csv")
