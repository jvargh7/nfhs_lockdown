require(survey)
require(splines)

source("sensitivity demonetization/nlsd_analytic sample.R")
analytic_sample <- nlsd_analytic_sample %>% 
  dplyr::filter(m_rural == 0)
rm(nlsd_analytic_sample)


analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")
rm(analytic_sample)

glm_stunting <- svyglm(as_formula(paste0("c_stunting ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                       family = quasipoisson())
saveRDS(save_svyglm(glm_stunting) ,paste0(path_lockdown_folder,"/working/nlsd/nlsd01r_glm_stunting.RDS"))
summary_poisson <- broom::tidy(glm_stunting) %>% mutate(outcome = "Stunting")
rm(glm_stunting)
gc()

glm_underweight <- svyglm(as_formula(paste0("c_underweight ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                          family = quasipoisson())
saveRDS(save_svyglm(glm_underweight),paste0(path_lockdown_folder,"/working/nlsd/nlsd01r_glm_underweight.RDS"))
summary_poisson <- bind_rows(summary_poisson,broom::tidy(glm_underweight) %>% mutate(outcome = "Underweight"))
rm(glm_underweight)
gc()

glm_wasting <- svyglm(as_formula(paste0("c_wasting ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                      family = quasipoisson())
saveRDS(save_svyglm(glm_wasting),paste0(path_lockdown_folder,"/working/nlsd/nlsd01r_glm_wasting.RDS"))
summary_poisson <- bind_rows(summary_poisson,broom::tidy(glm_wasting) %>% mutate(outcome = "Wasting"))
rm(glm_wasting)
# Stunting -----------


summary_poisson = summary_poisson %>% 
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
  write_csv(.,"sensitivity demonetization/nlsd01u_summary poisson regression urban.csv")


