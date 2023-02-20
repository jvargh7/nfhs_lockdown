source(".Rprofile")
source("functions/exp_T.R")
source("C:/code/external/functions/nhst/se_from_pvalue.R")
source("C:/code/external/functions/preprocessing/round_d.R")
d_coefs = read_csv("analysis/ns201_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

d_coefs$p.adj = p.adjust(d_coefs$p.value,method="BH")


c_coefs = read_csv("analysis/ns301_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

c_coefs$p.adj = p.adjust(c_coefs$p.value,method="BH")


e_flag = FALSE

table_coefs = bind_rows(d_coefs %>% mutate(region = "District"),
                        c_coefs %>% mutate(region = "Cluster")) %>% 
  mutate(std.error.adj = se_from_pvalue(estimate,p.adj,std.error))%>% 
  mutate(L.adj = estimate - 1.96*std.error.adj,
         U.adj = estimate + 1.96*std.error.adj) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L.adj,e_flag),2),", ",
                         round_d(exp_T(U.adj,e_flag),2),")"),
         lci.adj = exp_T(L.adj,e_flag),
         uci.adj = exp_T(U.adj,e_flag)) %>% 
  dplyr::select(model,region,term,outcome,RR.adj) %>% 
  pivot_wider(names_from=c("region","outcome"),values_from=RR.adj)  %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR"))

write_csv(table_coefs,"paper/table_ecological covariate coefficients.csv")
