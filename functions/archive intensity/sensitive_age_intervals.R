
source("analysis/nla_equations.R")
source("C:/code/external/functions/survey/contrasts_Svyglm.R")
source("functions/exp_T.R")


sensitive_age_intervals = function(prefix = "ns1",
                                   svy_des = NULL,
                                   o_vars = c("c_stunting","c_underweight","c_wasting"),
                                   family_svyglm = "quassipoisson",
                                   eqA,eqB,eqC,eqD,
                                   modifier2a="age_le24",
                                   modifier2b="age_le24",
                                   exposure2a = "phase2",
                                   exposure2b = "exposure_estimate_centered"
                                   ){
  
  nsX02_summary <- map(o_vars,
                       function(o){
                         print(o);
                         s_d <<- svy_des;
                         f_s <<- family_svyglm;
                         eA <<- eqA;
                         eB <<- eqB;
                         eC <<- eqC;
                         eD <<- eqD;
                         
                         # o = "c_stunting"
                         f02a = paste0(o,eA)
                         f02b = paste0(o,eB)
                         f02c = paste0(o,eC)
                         f02d = paste0(o,eD)
                         
                         
                         m2a = svyglm(f02a,design = s_d,family=f_s)
                         m2b = svyglm(f02b,design = s_d,family=f_s)
                         m2c = svyglm(f02c,design = s_d,family=f_s)
                         m2d = svyglm(f02d,design = s_d,family=f_s)
                         
                         
                         
                         coefs = bind_rows(
                           broom::tidy(m2a) %>% mutate(model = "m2a"),
                           broom::tidy(m2b) %>% mutate(model = "m2b"),
                           broom::tidy(m2c) %>% mutate(model = "m2c"),
                           broom::tidy(m2d) %>% mutate(model = "m2d")
                           
                           
                         ) %>% 
                           mutate(outcome = o) %>% 
                           dplyr::select(outcome,model,everything())
                         
                         
                         
                         
                         contrasts2a = contrasts_svyglm(svymodel = m2a,modifier = modifier2a,exposure = exposure2a) %>% 
                           mutate(outcome = o) 
                         contrasts2b = contrasts_svyglm(svymodel = m2b,modifier = modifier2b,exposure = exposure2b) %>% 
                           mutate(outcome = o)
                         
                         contrasts <- bind_rows(contrasts2a,
                                                contrasts2b);
                         
                         vcovs = list(vcov(m2a),
                                      vcov(m2b),
                                      vcov(m2c),
                                      vcov(m2d));
                         
                         return(list(coefs,contrasts,vcovs))
                         
                       })
  
  # e_flag check for exponentiating results
  e_flag = TRUE
  if(family_svyglm == "gaussian"){
    e_flag = FALSE
  }
  
  saveRDS(nsX02_summary,paste0(path_lockdown_folder,"/working/",prefix,"02_summary.RDS"))
  
  map_dfr(nsX02_summary,
          function(m){
            m[1]
          }) %>% 
    mutate(L = estimate - 1.96*std.error,
           U = estimate + 1.96*std.error) %>% 
    mutate(Coef = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L,e_flag),2),", ",
                         round_d(exp_T(U,e_flag),2),")"),
           lci = exp(L),
           uci = exp(U)) %>% 
    write_csv(.,paste0("analysis/",prefix,"02_coefficients of sensitive age.csv"))
  
  map_dfr(nsX02_summary,
          function(m){
            m[2]
          }) %>% 
    mutate(Coef = paste0(round_d(exp_T(Estimate,e_flag),2)," \t(",
                         round_d(exp_T(LCI,e_flag),2),", ",
                         round_d(exp_T(UCI,e_flag),2),")")) %>% 
    
    write_csv(.,paste0("analysis/",prefix,"02_contrasts of sensitive age.csv"))
  
}

