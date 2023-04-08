source("analysis/nlg_analytic sample processing.R")
source("analysis/nlg_equations.R")

source("C:/code/external/functions/preprocessing/round_d.R")
source("functions/exp_T.R")
e_flag = FALSE

eg_pooled = paste0("~ p1_cumulative + p2_cumulative + p3_cumulative + p4_cumulative + 
                   p5_cumulative + p6_cumulative + p7_cumulative + p8_cumulative + 
                   factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


rpooled_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(m_rural == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

upooled_svy <- analytic_sample_wdistrict %>% 
  dplyr::filter(m_rural == 0) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")


o_vars = c("c_haz","c_waz","c_whz")

ns4_summary <- map(o_vars,
                   function(o){
                     
                     f_pooled = paste0(o,eg_pooled) %>% as.formula(.);
                     
                     m1r = svyglm(f_pooled,design = rpooled_svy,family="gaussian");
                     m1u = svyglm(f_pooled,design = upooled_svy,family="gaussian");
                     
                     coefs = bind_rows(broom::tidy(m1r) %>% mutate(region = "Rural"),
                                       broom::tidy(m1u) %>% mutate(region = "Urban")) %>% 
                       mutate(outcome = o,
                              model = "m1")
                     
                     vcovs = list(vcov(m1r),
                                  vcov(m1u))
                     
                     return(list(coefs,vcovs))
                   })


saveRDS(ns4_summary,paste0(path_lockdown_folder,"/working/ns04_summary.RDS"))

map_dfr(ns4_summary,
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
  
  write_csv(.,paste0("analysis/ns4_coefficients.csv"))
