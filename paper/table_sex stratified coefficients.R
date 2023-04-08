source(".Rprofile")
source("functions/exp_T.R")
source("C:/code/external/functions/nhst/se_from_pvalue.R")
source("C:/code/external/functions/preprocessing/round_d.R")
f_coefs = read_csv("analysis/ns1f01_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

f_coefs$p.adj = p.adjust(f_coefs$p.value,method="BH")


m_coefs = read_csv("analysis/ns1m01_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

m_coefs$p.adj = p.adjust(m_coefs$p.value,method="BH")


e_flag = FALSE

table_coefs = bind_rows(f_coefs %>% mutate(region = "Female"),
                        m_coefs %>% mutate(region = "Male")) %>% 
  mutate(std.error.adj = se_from_pvalue(estimate,p.adj,std.error))%>% 
  mutate(L.adj = estimate - 1.96*std.error.adj,
         U.adj = estimate + 1.96*std.error.adj) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L.adj,e_flag),2),", ",
                         round_d(exp_T(U.adj,e_flag),2),")"),
         lci.adj = exp_T(L.adj,e_flag),
         uci.adj = exp_T(U.adj,e_flag))  %>% 
  mutate(inv_variance = 1/(std.error.adj^2))


summarized_coefs = table_coefs %>% 
  group_by(region,outcome,term) %>% 
  summarize(est_pooled = sum(estimate*inv_variance)/sum(inv_variance),
            se_pooled = 1/sqrt(sum(inv_variance))) %>% 
  mutate(L_pooled = est_pooled - 1.96*se_pooled,
         U_pooled = est_pooled + 1.96*se_pooled) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(est_pooled,e_flag),2)," \t(",
                         round_d(exp_T(L_pooled,e_flag),2),", ",
                         round_d(exp_T(U_pooled,e_flag),2),")"),
         Coef.adj = exp_T(est_pooled,e_flag),
         lci.adj = exp_T(L_pooled,e_flag),
         uci.adj = exp_T(U_pooled,e_flag))

bind_rows(table_coefs,
          summarized_coefs %>% mutate(model = "Pooled")) %>% 
  dplyr::select(model,region,term,outcome,RR.adj) %>% 
  pivot_wider(names_from=c("region","outcome"),values_from=RR.adj)  %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR")) %>% 
  write_csv(.,"paper/table_sex stratified coefficients.csv")
