m1a_terms = c("exposure_estimate_centered","exposure_gt20_centered")

read_csv("analysis/nla01_coefficients of exposure_gt20 as modifier.csv") %>% 
  dplyr::filter(term %in% m1a_terms,model == "m1a") %>% 
  dplyr::select(term,outcome,RR) %>% 
  pivot_wider(names_from=outcome,values_from=RR) %>% 
  write_csv(.,"paper/table_association of exposure to mobility restrictions.csv")

# read_csv("analysis/nla01_LRT of exposure_gt20 as modifier.csv")
