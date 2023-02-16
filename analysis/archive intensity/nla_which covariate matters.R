rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nla_equations.R")
source("analysis/nla_analytic sample processing.R")
source("functions/exp_T.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")
w00 = paste0("~ exposure_estimate_centered + exposure_gt20_centered")
w01 = paste0("~ exposure_estimate_centered + exposure_gt20_centered","+ c_age3mo")
w02 = paste0("~ exposure_estimate_centered + exposure_gt20_centered",c_covariates)
w03 = paste0("~ exposure_estimate_centered + exposure_gt20_centered",m_covariates)
w04 = paste0("~ exposure_estimate_centered + exposure_gt20_centered",hh_covariates)

# Replace district with nfhs5_state
w05 = paste0("~ exposure_estimate_centered + exposure_gt20_centered + m_rural + factor(nfhs5_state) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
# Exclude m_rural
w06 = paste0("~ exposure_estimate_centered + exposure_gt20_centered + factor(nfhs5_state) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

s_d = analytic_svy
f_s = "quasipoisson"

nswcm_summary <- map(c("c_stunting","c_underweight","c_wasting"),
                     function(o){
                       
                       print(o);
                       f00 = paste0(o,w00);
                       f01 = paste0(o,w01);
                       f02 = paste0(o,w02);
                       f03 = paste0(o,w03);
                       f04 = paste0(o,w04);
                       
                       
                       # print(f01a);
                       
                       m00 = svyglm(f00,design = s_d,family=f_s);
                       m01 = svyglm(f01,design = s_d,family=f_s);
                       m02 = svyglm(f02,design = s_d,family=f_s);
                       m03 = svyglm(f03,design = s_d,family=f_s);
                       m04 = svyglm(f04,design = s_d,family=f_s);
                       
                        coefs = bind_rows(
                         broom::tidy(m00) %>% mutate(model = "m00"),
                         broom::tidy(m01) %>% mutate(model = "m01"),
                         broom::tidy(m02) %>% mutate(model = "m02"),
                         broom::tidy(m03) %>% mutate(model = "m03"),
                         broom::tidy(m04) %>% mutate(model = "m04"),
                         
                         
                       ) %>% 
                         mutate(outcome = o) %>% 
                         dplyr::select(outcome,model,everything());
                       
                       
                       vcovs = list(vcov(m00),
                                    vcov(m01),
                                    vcov(m02),
                                    vcov(m03),
                                    vcov(m04));
                       
                       return(list(coefs,vcovs))
                       
                     })


e_flag = TRUE
map_dfr(nswcm_summary,
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
  
  write_csv(.,paste0("analysis/nswcm_which covariate matters.csv"))
