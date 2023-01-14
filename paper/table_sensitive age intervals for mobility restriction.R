
include_main_effects = FALSE

df1_terms = c("phase2 at age_le24=0",
              "phase2 at age_le24=1",
              "Interaction of phase2:age_le24",
              "exposure_estimate_centered at age_le24=0",
              "exposure_estimate_centered at age_le24=1",
              "Interaction of exposure_estimate_centered:age_le24")
df3_terms = c("phase2",
  "phase2:age_le24",
  "exposure_estimate_centered",
  "exposure_estimate_centered:age_le24")

names(df1_terms) = c("Phase 2 for 36-59","Phase 2 for 00-24","Phase 2 36-59 vs 00-24",
                     "Average depth in 36-59","Average depth in 00-24","Average depth 36-59 vs 00-24")
names(df3_terms) = c("Phase 2 for 36-59","Phase 2 36-59 vs 00-24","Average depth in 36-59","Average depth 36-59 vs 00-24")

if(!include_main_effects){
  df3_terms = df3_terms[c(2,4)]
  # df1_terms = df1_terms[c(3,6)]
}

df2_terms = c("phase2","p1_estimate_centered","p2_estimate_centered","p3_estimate_centered")
names(df2_terms) = c("Phase 2","Average depth in Gestation","Average depth in 00-06","Average depth in 07-24")

df4_terms = df2_terms

df1 = read_csv("analysis/nla02_contrasts of sensitive age.csv") %>% 
  rename_at(vars(one_of("Coef")),~"RR") %>% 
  dplyr::select(term,outcome,RR) %>% 
  dplyr::filter(term %in% df1_terms) %>% 
  pivot_wider(names_from=outcome,values_from=RR) %>% 
  mutate(term = factor(term,levels=df1_terms,labels=names(df1_terms))) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))



df2 = read_csv("analysis/nla02_coefficients of sensitive age.csv") %>% 
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

