
source("analysis/nla_equations.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")
source("functions/exp_T.R")


association_mobility_restriction <- function(prefix = "ns1",
                                             svy_des = NULL,
                                             o_vars = c("c_stunting","c_underweight","c_wasting"),
                                             family_svyglm = "quasipoisson",
                                             eqA,eqB,
                                             exposure1b = "exposure_estimate_centered",
                                             modifier1b = "exposure_gt20_centered"){
  

  
  nsX01_summary <- map(o_vars,
                           function(o){
                             s_d <<- svy_des;
                             f_s <<- family_svyglm;
                             eA <<- eqA;
                             eB <<- eqB;
                             
                             print(o);
                             f01a = paste0(o,eA);
                             f01b = paste0(o,eB);

                             # print(f01a);
                             
                             m1a = svyglm(f01a,design = s_d,family=f_s);
                             m1b = svyglm(f01b,design = s_d,family=f_s);
                             
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
                             
                             
                             
                             contrasts = contrasts_svyglm(svymodel = m1b,modifier = modifier1b,exposure = exposure1b) %>% 
                               mutate(outcome = o);
                             
                             
                             vcovs = list(vcov(m1a),
                                          vcov(m1b));
                             
                             return(list(LRT,coefs,contrasts,vcovs))
                             
                           })

  e_flag = TRUE
  if(family_svyglm == "gaussian"){
    e_flag = FALSE
  }
  
  saveRDS(nsX01_summary,paste0(path_lockdown_folder,"/working/",prefix,"01_summary.RDS"))
  
  map_dfr(nsX01_summary,
          function(m){
            m[1]
          }) %>% 
    
    write_csv(.,paste0("analysis/",prefix,"01_LRT of exposure_gt20 as modifier.csv"))
  
  
  map_dfr(nsX01_summary,
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
  
  
  map_dfr(nsX01_summary,
          function(m){
            m[3]
          }) %>% 
    mutate(RR = paste0(round_d(exp_T(Estimate,e_flag),2)," \t(",
                       round_d(exp_T(LCI,e_flag),2),", ",
                       round_d(exp_T(UCI,e_flag),2),")")) %>% 
    
    write_csv(.,paste0("analysis/",prefix,"01_contrasts of exposure_gt20 as modifier.csv"))
  
  
  
}

