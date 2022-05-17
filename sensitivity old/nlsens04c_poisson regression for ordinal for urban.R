require(survey)
require(splines)

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,m_rural == 0) %>% 
  dplyr::select(-ends_with("_d")) %>% 
  mutate_at(vars(starts_with("e")),.f = list(o = function(x) case_when(x > 150 ~ 2,
                                                                       x > 90 ~ 1,
                                                                       TRUE ~ 0))) %>% 
  mutate_at(vars(ends_with("_o")),.f= function(x) factor(x,levels=c(0,1,2),labels=c("low","med","high")))

analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

# Stunting -----------

glm_stunting <- svyglm(c_stunting ~ ns(c_age,df=4) + e1_p1_o + e1_p2_o + e1_p3_o + e1_p4_o + e1_p5_o +
                         e2_p1_o + e2_p2_o + e2_p3_o + e2_p4_o + e2_p5_o + nfhs5 + 
                         m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                       family = poisson())

glm_underweight <- svyglm(c_underweight ~ ns(c_age,df=4) + e1_p1_o + e1_p2_o + e1_p3_o + e1_p4_o + e1_p5_o +
                            e2_p1_o + e2_p2_o + e2_p3_o + e2_p4_o + e2_p5_o + nfhs5 + 
                            m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
                          family = poisson())

glm_wasting <- svyglm(c_wasting ~ ns(c_age,df=4) + e1_p1_o + e1_p2_o + e1_p3_o + e1_p4_o + e1_p5_o +
                        e2_p1_o + e2_p2_o + e2_p3_o + e2_p4_o + e2_p5_o + nfhs5 + 
                        m_caste + m_wealthq + m_religion + m_education + m_alcohol + m_smoking + factor(v024_nfhs5),design = analytic_survey,
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
  write_csv(.,"sensitivity/sens04_poisson outcomes x economic shocks with ordinal for urban.csv")
