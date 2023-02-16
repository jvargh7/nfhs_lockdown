
tabB1 = read_csv("analysis/nla02_contrasts of sensitive age.csv") %>% 
  rename_at(vars(one_of("Coef")),~"RR") %>% 
  dplyr::select(term,outcome,RR) %>% 
  dplyr::filter(term %in% df1_terms) %>% 
  pivot_wider(names_from=outcome,values_from=RR) %>% 
  mutate(term = factor(term,levels=df1_terms,labels=names(df1_terms))) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))



tabB2 = read_csv("analysis/nla02_coefficients of sensitive age.csv") %>% 
  rename_at(vars(one_of("Coef")),~"RR") %>% 
  dplyr::filter(model == "m2c") %>% 
  dplyr::filter(term %in% df2_terms) %>% 
  dplyr::select(term,outcome,RR) %>% 
  mutate(term = factor(term,levels=df2_terms,labels=names(df2_terms))) %>% 
  pivot_wider(names_from=outcome,values_from=RR) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))


df3 = read_csv("analysis/nla02_coefficients of sensitive age.csv") %>% 
  dplyr::filter(model %in% c("m2a","m2b")) %>% 
  dplyr::select(term,outcome,p.value) %>% 
  dplyr::filter(term %in% df3_terms) 

df4 = read_csv("analysis/nla02_coefficients of sensitive age.csv") %>% 
  dplyr::filter(model == "m2c") %>% 
  dplyr::filter(term %in% df4_terms) %>% 
  dplyr::select(term,outcome,p.value) 


p_adj = p.adjust(c(df3$p.value,df4$p.value),method = "BH")


df3adj = df3 %>% 
  dplyr::select(-p.value) %>% 
  mutate(p.adj = p_adj[1:nrow(df3)]) %>% 
  pivot_wider(names_from=outcome,values_from=p.adj) %>% 
  mutate(term = factor(term,levels=df3_terms,labels=names(df3_terms))) %>% 
  
  rename_at(vars(starts_with("c_")),~paste0(.,"_p"))



df4adj = df4 %>% 
  dplyr::select(-p.value) %>% 
  mutate(p.adj = p_adj[(1+nrow(df3)):length(p_adj)]) %>% 
  mutate(term = factor(term,levels=df4_terms,labels=names(df4_terms))) %>% 
  
  pivot_wider(names_from=outcome,values_from=p.adj) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_p"))

bind_rows(
  df1 %>% 
    left_join(df3adj,
              by="term"),
  
  df2 %>% 
    left_join(df4adj,
              by="term")) %>% 
  mutate_at(vars(ends_with("_p")),~case_when(is.na(.)~ "",
                                             TRUE ~ as.character(round(.,3)))) %>% 
  dplyr::select(term,contains("stunting"),contains("underweight"),contains("wasting")) %>% 
  
  
  write_csv(.,"paper/table_sensitive age intervals for mobility restriction.csv")

