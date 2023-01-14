source("analysis/nla_analytic sample processing.R")
source("analysis/nla_equations.R")

# Each regression takes ~5 mins --> 4 x 5 x 3 + time for LRT ~ 1h total
nla00_summary <- map(outcome_vars,
                          function(o){
                            print(o);
                            # o = "c_stunting"
                            f01a = paste0(o,e01a)
                            f01b = paste0(o,e01b)
                            f01c = paste0(o,e01c)
                            f01d = paste0(o,e01d)
                            # m1a = glm(f01a,data = analytic_sample,family="binomial")
                            
                            m1a = svyglm(f01a,design = analytic_svy,family="quasipoisson")
                            m1b = svyglm(f01b,design = analytic_svy,family="quasipoisson")
                            m1c = svyglm(f01c,design = analytic_svy,family="quasipoisson")
                            m1d = svyglm(f01d,design = analytic_svy,family="quasipoisson")
                            
                            compare_a_vs_b = anova(m1a,m1b)
                            compare_a_vs_c = anova(m1a,m1c)
                            compare_c_vs_d = anova(m1c,m1d)
                            gc()
                            
                            LRT = bind_rows(outcome = o,
                                      a_vs_b = paste0(compare_a_vs_b$chisq,", p = ",round(compare_a_vs_b$p,3)),
                                      a_vs_c = paste0(compare_a_vs_c$chisq,", p = ",round(compare_a_vs_c$p,3)),
                                      c_vs_d = paste0(compare_c_vs_d$chisq,", p = ",round(compare_c_vs_d$p,3)))
                            
                            
                            coefs = bind_rows(
                                              broom::tidy(m1a) %>% mutate(model = "m1a"),
                                              broom::tidy(m1b) %>% mutate(model = "m1b"),
                                              broom::tidy(m1c) %>% mutate(model = "m1c"),
                                              broom::tidy(m1d) %>% mutate(model = "m1d"),
                                              
                                              ) %>% 
                              mutate(outcome = o) %>% 
                              dplyr::select(outcome,model,everything())
                            
                              return(list(LRT,coefs))
                            
                            
                            
                          })

map_dfr(nla00_summary,
        function(m){
          m[1]
        }) %>% 

write_csv(.,"analysis/nla00_LRT of urbanicity as modifier.csv")

map_dfr(nla00_summary,
        function(m){
          m[2]
        }) %>% 
  
  write_csv(.,"analysis/nla00_coefficients of urbanicity as modifier.csv")