require(survey)
require(splines)

overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  dplyr::filter(m_rural == 0)


analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

glm_haz <- svyglm(as_formula(paste0("c_haz ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                       family = gaussian())

glm_waz <- svyglm(as_formula(paste0("c_waz ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                          family = gaussian())

glm_whz <- svyglm(as_formula(paste0("c_whz ~ factor(e_interaction)",region_covariates)),design = analytic_survey,
                      family = gaussian())


saveRDS(save_svyglm(glm_haz) ,paste0(path_lockdown_folder,"/working/nlsz/nlsz01u_glm_haz.RDS"))
saveRDS(save_svyglm(glm_waz),paste0(path_lockdown_folder,"/working/nlsz/nlsz01u_glm_waz.RDS"))
saveRDS(save_svyglm(glm_whz),paste0(path_lockdown_folder,"/working/nlsz/nlsz01u_glm_whz.RDS"))

# Stunting -----------


summary_gaussian = bind_rows(
  broom::tidy(glm_haz) %>% mutate(outcome = "HAZ"),
  broom::tidy(glm_waz) %>% mutate(outcome = "WAZ"),
  broom::tidy(glm_whz) %>% mutate(outcome = "WHZ")) %>% 
  mutate(coef = estimate,
         lci = (estimate - 1.96*std.error),
         uci = (estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::filter(str_detect(term,"factor\\(e")) 

summary_gaussian %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"sensitivity zscores/nlsz01u_summary gaussian regression urban.csv")





