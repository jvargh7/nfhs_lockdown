require(survey)
require(splines)

source("main analysis/nlma_analytic sample.R")
analytic_sample <- nlma_analytic_sample %>% 
  dplyr::filter(m_rural == 0)


  analytic_survey <- analytic_sample %>% 
    as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                     variance = "YG",pps = "brewer")
  
  glm_stunting <- svyglm(as_formula(paste0("c_stunting ~ e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                            e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d",region_covariates)),design = analytic_survey,
                         family = quasipoisson())
  
  glm_underweight <- svyglm(as_formula(paste0("c_underweight ~ e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                            e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d",region_covariates)),design = analytic_survey,
                            family = quasipoisson())
  
  glm_wasting <- svyglm(as_formula(paste0("c_wasting ~ e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                            e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d",region_covariates)),design = analytic_survey,
                        family = quasipoisson())

saveRDS(save_svyglm(glm_stunting) ,paste0(path_lockdown_folder,"/working/nlsa/nlsa01u_glm_stunting.RDS"))
saveRDS(save_svyglm(glm_underweight),paste0(path_lockdown_folder,"/working/nlsa/nlsa01u_glm_underweight.RDS"))
saveRDS(save_svyglm(glm_wasting),paste0(path_lockdown_folder,"/working/nlma/nlsa01u_glm_wasting.RDS"))
# Stunting -----------


summary_poisson = bind_rows(
  broom::tidy(glm_stunting) %>% mutate(outcome = "Stunting"),
  broom::tidy(glm_underweight) %>% mutate(outcome = "Underweight"),
  broom::tidy(glm_wasting) %>% mutate(outcome = "Wasting")) %>% 
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::filter(str_detect(term,"^e(1|2)")) 

summary_poisson %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"sensitivity additive/nlsa01u_summary poisson regression urban.csv")

