
source("analysis/nla_equations.R")
source("functions/exp_T.R")
source("C:/code/external/functions/preprocessing/round_D.R")

e_flag = TRUE

ns401_summary <- map(outcome_vars,
                     function(o){
                       f_s = "quasipoisson";

                       print(o);
                       f01a = paste0(o,e04a);

                       # print(f01a);
                       
                       m1a = svyglm(f01a,design = spline_svy,family=f_s)
                       
                       
                       coefs = bind_rows(
                         broom::tidy(m1a) %>% mutate(model = "m1a"),

                         
                       ) %>% 
                         mutate(outcome = o) %>% 
                         dplyr::select(outcome,model,everything());
                       
                        vcovs = list(vcov(m1a));
                       
                       return(list(coefs,vcovs))
                       
                     })

saveRDS(ns401_summary,paste0(path_lockdown_folder,"/working/ns401_summary.RDS"))


map_dfr(ns401_summary,
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
  
  write_csv(.,paste0("analysis/ns401_coefficients of exposure_gt20 as modifier.csv"))
