source("functions/exp_T.R")
source("C:/code/external/functions/nhst/se_from_pvalue.R")
source("C:/code/external/functions/preprocessing/round_d.R")
e_flag = TRUE
include_main_effects = FALSE
prefix = "nla"
tabB1_terms = c("phase2 at age_le24=0",
              "phase2 at age_le24=1",
              "Interaction of phase2:age_le24",
              "exposure_estimate_centered at age_le24=0",
              "exposure_estimate_centered at age_le24=1",
              "Interaction of exposure_estimate_centered:age_le24")
tabB3_terms = c("phase2",
  "phase2:age_le24",
  "exposure_estimate_centered",
  "exposure_estimate_centered:age_le24")

tabB2_terms = c("phase2","p1_estimate_centered","p2_estimate_centered","p3_estimate_centered")
names(tabB2_terms) = c("Phase 2","Average depth in Gestation","Average depth in 00-06","Average depth in 07-24")
tabB4_terms = tabB2_terms

names(tabB1_terms) = c("Phase 2 for 36-59","Phase 2 for 00-24","Phase 2 36-59 vs 00-24",
                     "Average depth in 36-59","Average depth in 00-24","Average depth 36-59 vs 00-24")
names(tabB3_terms) = c("Phase 2 for 36-59","Phase 2 36-59 vs 00-24","Average depth in 36-59","Average depth 36-59 vs 00-24")

tabB3_terms_final = tabB3_terms
if(!include_main_effects){
  tabB3_terms_final = na.omit(tabB3_terms[c(2,4)])
  # df1_terms = df1_terms[c(3,6)]
}


coef02 <- read_csv(paste0("analysis/",prefix,"02_coefficients of sensitive age.csv"))
contrasts02 <- read_csv(paste0("analysis/",prefix,"02_contrasts of sensitive age.csv"))


# Coefficient extraction ----------
tabB2 = coef02 %>% 
  rename_at(vars(one_of("Coef")),~"RR") %>% 
  dplyr::filter(model == "m2c") %>% 
  dplyr::filter(term %in% tabB2_terms)

tabB1 = contrasts02 %>% 
  rename_at(vars(one_of("Coef")),~"RR") %>% 
  rename(estimate = Estimate,
         std.error = SE)

# P-value extraction -------- 
tabB3 = coef02 %>% 
  dplyr::filter(model %in% c("m2a","m2b")) %>% 
  dplyr::select(term,outcome,p.value) %>% 
  dplyr::filter(term %in% tabB3_terms_final) 

tabB4 = coef02 %>% 
  dplyr::filter(model == "m2c") %>% 
  dplyr::filter(term %in% tabB4_terms) %>% 
  dplyr::select(term,outcome,p.value) 

p_adj = p.adjust(c(tabB3$p.value,tabB4$p.value),method = "BH")


# p-value adjustment --------  
tabB3adj = tabB3 %>% 
  dplyr::select(-p.value) %>% 
  mutate(p.adj = p_adj[1:nrow(tabB3)])  %>% 
  mutate(term = factor(term,levels=tabB3_terms_final,labels=names(tabB3_terms_final)))

tabB4adj = tabB4 %>% 
  dplyr::select(-p.value) %>% 
  mutate(p.adj = p_adj[(1+nrow(tabB3)):(nrow(tabB3)+nrow(tabB4))])

# Standard error adjustment
tabB2adj = tabB2 %>% 
  left_join(tabB4adj,by=c("term","outcome")) %>% 
  mutate(std.error.adj = se_from_pvalue(estimate,p.adj,std.error))%>% 
  mutate(L.adj = estimate - 1.96*std.error.adj,
         U.adj = estimate + 1.96*std.error.adj) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L.adj,e_flag),2),", ",
                         round_d(exp_T(U.adj,e_flag),2),")"),
         lci.adj = exp_T(L.adj,e_flag),
         uci.adj = exp_T(U.adj,e_flag))


tabB1adj = tabB1 %>% 
  mutate(term = factor(term,levels=tabB1_terms,labels=names(tabB1_terms))) %>% 
  left_join(tabB3adj,by=c("term","outcome")) %>% 
  mutate(std.error.adj = se_from_pvalue(estimate,p.adj,std.error))%>% 
  mutate(L.adj = estimate - 1.96*std.error.adj,
         U.adj = estimate + 1.96*std.error.adj) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L.adj,e_flag),2),", ",
                         round_d(exp_T(U.adj,e_flag),2),")"),
         lci.adj = exp_T(L.adj,e_flag),
         uci.adj = exp_T(U.adj,e_flag))

# Reshaping p-value tables
tabB3adj = tabB3adj %>% 
  pivot_wider(names_from=outcome,values_from=p.adj) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_p")) %>% 
  mutate_at(vars(ends_with("_p")),~as.character(round(.,3)))

tabB4adj = tabB4adj  %>% 
  mutate(term = factor(term,levels=tabB4_terms,labels=names(tabB4_terms))) %>% 
  pivot_wider(names_from=outcome,values_from=p.adj) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_p")) %>% 
  mutate_at(vars(ends_with("_p")),~as.character(round(.,3)))

# Coefficients -------
tabB2_final = tabB2adj %>% 
  dplyr::select(term,outcome,RR.adj) %>% 
  mutate(term = factor(term,levels=tabB2_terms,labels=names(tabB2_terms))) %>% 
  
  pivot_wider(names_from=outcome,values_from=RR.adj) %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))

tabB1_final = tabB1adj  %>% 
  dplyr::select(term,outcome,RR.adj) %>% 
  pivot_wider(names_from=outcome,values_from=RR.adj)  %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))

out = bind_rows(

  tabB1_final %>% 
    left_join(tabB3adj,
              by="term"),
  
  tabB2_final %>% 
    left_join(tabB4adj,
              by="term")) %>% 
  mutate_at(vars(ends_with("_p")),~case_when(is.na(.)~ "",
                                             TRUE ~ .)) %>% 
  dplyr::select(term,matches("(stunting|haz)"),matches("(underweight|waz)"),matches("(wasting|whz)"),everything())

write_csv(out,"paper/table_sensitive age intervals for mobility restriction.csv")
