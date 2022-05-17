require(survey)
require(splines)

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)  %>% 
  dplyr::filter(m_rural == 1)


analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

# Stunting -----------

lm_haz <- svyglm(c_haz ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                   e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                   m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                 family = gaussian())

lm_waz <- svyglm(c_waz ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                   e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                   m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                 family = gaussian())

lm_whz <- svyglm(c_whz ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                   e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                   m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                 family = gaussian())

summary_gaussian = bind_rows(
  broom::tidy(lm_haz) %>% mutate(outcome = "Stunting"),
  broom::tidy(lm_waz) %>% mutate(outcome = "Underweight"),
  broom::tidy(lm_whz) %>% mutate(outcome = "Wasting")) %>% 
  mutate(coef = estimate,
         lci = estimate - 1.96*std.error,
         uci = estimate + 1.96*std.error) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::filter(str_detect(term,"^e(1|2)")) 

summary_gaussian %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from = coef_ci) %>% 
  write_csv(.,"sensitivity/sens02b_gaussian outcomes x economic shocks for rural.csv")
