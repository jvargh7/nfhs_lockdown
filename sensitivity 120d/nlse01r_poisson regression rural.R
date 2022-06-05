require(survey)
require(splines)



analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>% 
  dplyr::filter(m_rural == 1)




analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

glm_stunting <- svyglm(as_formula(paste0("c_stunting ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                       family = quasipoisson())

glm_underweight <- svyglm(as_formula(paste0("c_underweight ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                          family = quasipoisson())

glm_wasting <- svyglm(as_formula(paste0("c_wasting ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                      family = quasipoisson())

saveRDS(save_svyglm(glm_stunting) ,paste0(path_lockdown_folder,"/working/nlse/nlse01r_glm_stunting.RDS"))
saveRDS(save_svyglm(glm_underweight),paste0(path_lockdown_folder,"/working/nlse/nlse01r_glm_underweight.RDS"))
saveRDS(save_svyglm(glm_wasting),paste0(path_lockdown_folder,"/working/nlse/nlse01r_glm_wasting.RDS"))
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
  dplyr::filter(str_detect(term,"factor\\(e"))

summary_poisson %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"sensitivity 120d/nlse01r_summary poisson regression rural.csv")


