require(survey)
require(srvyr)
require(splines)

source("main analysis/nlma_analytic sample.R")
analytic_sample <- nlma_analytic_sample
rm(nlma_analytic_sample)

analytic_survey <- analytic_sample %>% 
  as_survey_design(.data=.,ids = v021,strata=v024_nfhs5,nest=TRUE,weights = combined_sampleweight,
                   variance = "YG",pps = "brewer")

glm_stunting_pooled <- svyglm(as_formula(paste0("c_stunting ~ factor(e_interaction) + m_rural",region_covariates)),design = analytic_survey,
                       family = quasipoisson())

glm_stunting_stratified <- svyglm(as_formula(paste0("c_stunting ~ factor(e_interaction)*m_rural",interaction_covariates)),design = analytic_survey,
                       family = quasipoisson())

anova(glm_stunting_pooled,glm_stunting_stratified,test = "F",method = "LRT")
gc()

glm_underweight_pooled <- svyglm(as_formula(paste0("c_underweight ~ factor(e_interaction) + m_rural",region_covariates)),design = analytic_survey,
                              family = quasipoisson())

glm_underweight_stratified <- svyglm(as_formula(paste0("c_underweight ~ factor(e_interaction)*m_rural",interaction_covariates)),design = analytic_survey,
                                  family = quasipoisson())

anova(glm_underweight_pooled,glm_underweight_stratified,test = "F",method = "LRT")
gc()

glm_wasting_pooled <- svyglm(as_formula(paste0("c_wasting ~ factor(e_interaction) + m_rural",region_covariates)),design = analytic_survey,
                                 family = quasipoisson())

glm_wasting_stratified <- svyglm(as_formula(paste0("c_wasting ~ factor(e_interaction)*m_rural",interaction_covariates)),design = analytic_survey,
                                     family = quasipoisson())

anova(glm_wasting_pooled,glm_wasting_stratified,test = "F",method = "LRT")
gc()

