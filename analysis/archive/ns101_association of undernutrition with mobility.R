rm(list=ls()); gc(); source(".Rprofile")

source("analysis/nla_analytic sample processing.R")
source("analysis/nla_equations.R")

source("C:/code/external/functions/survey/contrasts_svyglm.R")

# Each regression takes ~5 mins --> 4 x 5 x 3 + time for LRT ~ 1h total
ns101_summary <- map(z_vars,
                     function(o){
                       print(o);
                       # o = "c_stunting"
                       f01a = paste0(o,e01a)
                       f01b = paste0(o,e01b)
                       
                       
                       m1a = svyglm(f01a,design = analytic_svy,family="gaussian")
                       m1b = svyglm(f01b,design = analytic_svy,family="gaussian")
                       
                       compare_a_vs_b = anova(m1a,m1b)
                       gc()
                       
                       LRT = bind_rows(outcome = o,
                                       a_vs_b = paste0(compare_a_vs_b$chisq,", p = ",round(compare_a_vs_b$p,3)),
                       )
                       
                       
                       coefs = bind_rows(
                         broom::tidy(m1a) %>% mutate(model = "m1a"),
                         broom::tidy(m1b) %>% mutate(model = "m1b")
                         
                         
                       ) %>% 
                         mutate(outcome = o) %>% 
                         dplyr::select(outcome,model,everything())
                       
                       contrasts = contrasts_svyglm(svymodel = m1b,modifier = "exposure_gt20",exposure = "exposure_estimate") %>% 
                         mutate(outcome = o)
                       
                       
                       return(list(LRT,coefs,contrasts))
                       
                       
                       
                     })

map_dfr(ns101_summary,
        function(m){
          m[1]
        }) %>% 
  
  write_csv(.,"analysis/ns101_LRT of exposure_gt20 as modifier.csv")


map_dfr(ns101_summary,
        function(m){
          m[2]
        }) %>% 
  mutate(L = estimate - 1.96*std.error,
         U = estimate + 1.96*std.error) %>% 
  mutate(RR = paste0(round_d(estimate,2)," \t(",
                     round_d(L,2),", ",
                     round_d(U,2),")"),
         lci = exp(L),
         uci = exp(U)) %>% 
  
  write_csv(.,"analysis/ns101_coefficients of exposure_gt20 as modifier.csv")


map_dfr(ns101_summary,
        function(m){
          m[3]
        }) %>% 
  mutate(RR = paste0(round_d(Estimate,2)," \t(",
                     round_d(LCI,2),", ",
                     round_d(UCI,2),")")) %>% 
  
  write_csv(.,"analysis/ns101_contrasts of exposure_gt20 as modifier.csv")