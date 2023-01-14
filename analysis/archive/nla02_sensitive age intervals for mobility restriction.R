rm(list=ls()); gc(); source(".Rprofile")

source("analysis/nla_analytic sample processing.R")
source("analysis/nla_equations.R")

source("C:/code/external/functions/survey/contrasts_Svyglm.R")

# Each regression takes ~5-6 mins
nla02_summary <- map(outcome_vars,
                     function(o){
                       print(o);
                       # o = "c_stunting"
                       f02a = paste0(o,e02a)
                       f02b = paste0(o,e02b)
                       f02c = paste0(o,e02c)
                       f02d = paste0(o,e02d)
                       
                       
                       m2a = svyglm(f02a,design = analytic2_svy,family="quasipoisson")
                       m2b = svyglm(f02b,design = analytic2_svy,family="quasipoisson")
                       m2c = svyglm(f02c,design = analytic2_svy,family="quasipoisson")
                       m2d = svyglm(f02d,design = analytic2_svy,family="quasipoisson")
                       
                       
                       
                       coefs = bind_rows(
                         broom::tidy(m2a) %>% mutate(model = "m2a"),
                         broom::tidy(m2b) %>% mutate(model = "m2b"),
                         broom::tidy(m2c) %>% mutate(model = "m2c"),
                         broom::tidy(m2d) %>% mutate(model = "m2d")
                         
                         
                       ) %>% 
                         mutate(outcome = o) %>% 
                         dplyr::select(outcome,model,everything())
                       
                       contrasts2a = contrasts_svyglm(svymodel = m2a,modifier = "age_le24",exposure = "phase2") %>% 
                         mutate(outcome = o) 
                       contrasts2b = contrasts_svyglm(svymodel = m2b,modifier = "age_le24",exposure = "exposure_estimate") %>% 
                         mutate(outcome = o)

                       contrasts <- bind_rows(contrasts2a,
                                              contrasts2b)
                       
                       return(list(coefs,contrasts))
                       
                     })
                       
 

map_dfr(nla02_summary,
        function(m){
          m[1]
        }) %>% 
  mutate(L = estimate - 1.96*std.error,
         U = estimate + 1.96*std.error) %>% 
  mutate(RR = paste0(round_d(exp(estimate),2)," \t(",
                     round_d(exp(L),2),", ",
                     round_d(exp(U),2),")"),
         lci = exp(L),
         uci = exp(U)) %>% 
  write_csv(.,"analysis/nla02_coefficients of sensitive age.csv")

map_dfr(nla02_summary,
        function(m){
          m[2]
        }) %>% 
  mutate(RR = paste0(round_d(exp(Estimate),2)," \t(",
                     round_d(exp(LCI),2),", ",
                     round_d(exp(UCI),2),")")) %>% 
  
  write_csv(.,"analysis/nla02_contrasts of sensitive age.csv")