rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/nla_analytic sample processing.R")

prefix = "naa"
o_vars = c("c_stunting","c_underweight","c_wasting")
f_s = family_svyglm = "quasipoisson"
exposure1b = "phase2"
modifier1b = "c_age3mo"
unique_ages = unique(analytic_sample$c_age3mo) %>% as.character()

nsaa_summary <- map(o_vars,
                     function(o){
                       print(o);
                       f01a = paste0(o,e03a);
                       f01b = paste0(o,e03b);
                       
                       # print(f01a);
                       
                       m1a = svyglm(f01a,design = analytic_svy,family=f_s);
                       m1b = svyglm(f01b,design = analytic_svy,family=f_s);
                       
                       compare_a_vs_b = anova(m1a,m1b);
                       gc();
                       
                       LRT = bind_rows(outcome = o,
                                       a_vs_b = paste0(compare_a_vs_b$chisq,", p = ",round(compare_a_vs_b$p,3)),
                       );
                       
                       
                       coefs = bind_rows(
                         broom::tidy(m1a) %>% mutate(model = "m1a"),
                         broom::tidy(m1b) %>% mutate(model = "m1b")
                         
                         
                       ) %>% 
                         mutate(outcome = o) %>% 
                         dplyr::select(outcome,model,everything());
                       
                       contrasts = map_dfr(unique_ages,
                                           function(u_age){
                                             contrasts_svyglm(svymodel = m1b,exposure = exposure1b,modifier = paste0(modifier1b,u_age))}) %>% 
                         mutate(outcome = o);
                       
                       return(list(LRT,coefs,contrasts))
                       
                     })


e_flag = TRUE
if(family_svyglm == "gaussian"){
  e_flag = FALSE
}

map_dfr(nsaa_summary,
        function(m){
          m[1]
        }) %>% 
  
  write_csv(.,paste0("analysis/",prefix,"01_LRT of exposure_gt20 as modifier.csv"))


map_dfr(nsaa_summary,
        function(m){
          m[2]
        }) %>% 
  mutate(L = estimate - 1.96*std.error,
         U = estimate + 1.96*std.error) %>% 
  mutate(RR = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                     round_d(exp_T(L,e_flag),2),", ",
                     round_d(exp_T(U,e_flag),2),")"),
         lci = exp_T(L,e_flag),
         uci = exp_T(U,e_flag)) %>% 
  
  write_csv(.,paste0("analysis/",prefix,"01_coefficients of exposure_gt20 as modifier.csv"))


map_dfr(nsaa_summary,
        function(m){
          m[3]
        }) %>% 
  mutate(RR = paste0(round_d(exp_T(Estimate,e_flag),2)," \t(",
                     round_d(exp_T(LCI,e_flag),2),", ",
                     round_d(exp_T(UCI,e_flag),2),")")) %>% 
  
  write_csv(.,paste0("analysis/",prefix,"01_contrasts of exposure_gt20 as modifier.csv"))

