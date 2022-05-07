require(survey)
require(splines)
# Arunachal Pradesh
# Chandigarh
# Chhattisgarh
# Haryana
# Jharkhand
# Madhya Pradesh
# Nct Of Delhi
# Odisha
# Puducherry
# Punjab
# Rajasthan
# Tamil Nadu
# Uttar Pradesh
# Uttarakhand

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% c(12,4,22,6,20,23,7,21,34,3,8,33,9,5))

analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

# Stunting -----------

glm_stunting <- svyglm(c_stunting ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                         e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                         m_caste + m_rural + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                       family = poisson())

glm_underweight <- svyglm(c_underweight ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                            e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                            m_caste + m_rural + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                          family = poisson())

glm_wasting <- svyglm(c_wasting ~ ns(c_age,df=4) + e1_p1_d + e1_p2_d + e1_p3_d + e1_p4_d + e1_p5_d +
                        e2_p1_d + e2_p2_d + e2_p3_d + e2_p4_d + e2_p5_d + nfhs5 + 
                        m_caste + m_rural + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                      family = poisson())

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
  write_csv(.,"paper/table_poisson outcomes x economic shocks 13 states.csv")



