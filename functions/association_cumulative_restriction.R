source("analysis/nlg_equations.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")
source("functions/exp_T.R")


association_cumulative_restriction <- function(prefix = "nlg",
                                             svy_des = list(),
                                             o_vars = c("c_haz","c_waz","c_whz"),
                                             family_svyglm = "gaussian",
                                             eq = list()){
  
  # svy_des = svy_des
  
  nsX01_summary <- map(o_vars,
                       function(o){
                         s_d1 <<- svy_des[[1]];
                         s_d2 <<- svy_des[[2]];
                         s_d3 <<- svy_des[[3]];
                         s_d4 <<- svy_des[[4]];
                         s_d5 <<- svy_des[[5]];
                         s_d6 <<- svy_des[[6]];
                         s_d7 <<- svy_des[[7]];
                         f_s <<- family_svyglm;
                         eA <<- eq[[1]];
                         eB <<- eq[[2]];
                         eC <<- eq[[3]];
                         eD <<- eq[[4]];
                         eE <<- eq[[5]];
                         eF <<- eq[[6]];
                         eG <<- eq[[7]];
                         
                         print(o);
                         f1 = paste0(o,eA);
                         f2 = paste0(o,eB);
                         f3 = paste0(o,eC);
                         f4 = paste0(o,eD);
                         f5 = paste0(o,eE);
                         f6 = paste0(o,eF);
                         f7 = paste0(o,eG);
                         
                         # print(f01a);
                         
                         m1 = svyglm(f1,design = s_d1,family=f_s);
                         m2 = svyglm(f2,design = s_d2,family=f_s);
                         m3 = svyglm(f3,design = s_d3,family=f_s);
                         m4 = svyglm(f4,design = s_d4,family=f_s);
                         m5 = svyglm(f5,design = s_d5,family=f_s);
                         m6 = svyglm(f6,design = s_d6,family=f_s);
                         m7 = svyglm(f7,design = s_d7,family=f_s);
                         
                         coefs = bind_rows(
                           broom::tidy(m1) %>% mutate(model = "m1"),
                           broom::tidy(m2) %>% mutate(model = "m2"),
                           broom::tidy(m3) %>% mutate(model = "m3"),
                           broom::tidy(m4) %>% mutate(model = "m4"),
                           broom::tidy(m5) %>% mutate(model = "m5"),
                           broom::tidy(m6) %>% mutate(model = "m6"),
                           broom::tidy(m7) %>% mutate(model = "m7")
                           
                           
                         ) %>% 
                           mutate(outcome = o) %>% 
                           dplyr::select(outcome,model,everything());
                         
                         
                         vcovs = list(vcov(m1),
                                      vcov(m2),
                                      vcov(m3),
                                      vcov(m4),
                                      vcov(m5),
                                      vcov(m6),
                                      vcov(m7)
                                      
                                      );
                         
                         
                      return(list(coefs,vcovs))
                         
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
    mutate(L = estimate - 1.96*std.error,
           U = estimate + 1.96*std.error) %>% 
    mutate(RR = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                       round_d(exp_T(L,e_flag),2),", ",
                       round_d(exp_T(U,e_flag),2),")"),
           lci = exp_T(L,e_flag),
           uci = exp_T(U,e_flag)) %>% 
    
    write_csv(.,paste0("analysis/",prefix,"01_coefficients.csv"))
  
  
  
  
}

