rm(list=ls()); gc(); source(".Rprofile")
source("analysis/ns6_analytic sample processing.R")
source("analysis/nla_equations.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")
source("functions/exp_T.R")

district_covariates = "+ Zone + pc1 "
e06a = paste0("~ phase2*c_age3mo + m_rural + factor(sdist) + factor(c_measurementmonth) ",district_covariates,c_covariates,m_covariates,hh_covariates)


age_categories = levels(analytic_sample$c_age3mo) %>% paste0("c_age3mo",.)



nae_summary <- map(outcome_vars,
                   function(o){
                     f06a = paste0(o,e06a);
                     
                     m6a = svyglm(f06a,design = district_svy,family="quasipoisson");
                     coefs = broom::tidy(m6a) %>% mutate(model = "m6a");
                     vcovs = vcov(m6a);
                     
                     
                     contrasts = map_dfr(age_categories,
                             function(a_c){
                               contrasts_svyglm(svymodel = m6a,modifier = a_c,exposure = "phase2") %>% 
                                 mutate(outcome = o);
                               
                             })
                             
                        
                     return(list(coefs,contrasts,vcovs))
                   })

saveRDS(nae_summary,paste0(path_lockdown_folder,"/working/nae_summary.RDS"))

e_flag = TRUE

map_dfr(nae_summary,
        function(m){
          m[1]
        }) %>% 
  
  write_csv(.,paste0("analysis/nae_coefficients of phase2 as exposure and age as modifier.csv"))


map_dfr(nae_summary,
        function(m){
          m[2]
        }) %>% 
  mutate(RR = paste0(round_d(exp_T(Estimate,e_flag),2)," \t(",
                     round_d(exp_T(LCI,e_flag),2),", ",
                     round_d(exp_T(UCI,e_flag),2),")")) %>% 
  # PR of children measured in Phase 2 of same age vs measured in Phase 1
  write_csv(.,paste0("analysis/nae_contrasts of phase2 as exposure and age as modifier.csv"))
