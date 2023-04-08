
# We haven't added m_rural as a covariate since our reported analysis is stratified by region of residence

c_covariates = c("+ c_age2mo + c_male ")
m_covariates = c("+ m_age + m_eduyr + m_height ")
# hh_wealthqur: not used 
hh_covariates = c("+ m_caste + hh_religion ")
# outcome_vars = c("c_stunting","c_underweight","c_wasting")
z_vars = c("c_haz","c_waz","c_whz","c_bmiz")

# G = 0 to 6
eg1 = paste0("~ p1_cumulative + p2_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

# G = 7 to 12
eg2 = paste0("~ p2_cumulative + p3_cumulative + p1_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

eg3 = paste0("~ p3_cumulative + p4_cumulative + p2_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

eg4 = paste0("~ p4_cumulative + p5_cumulative + p3_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

eg5 = paste0("~ p5_cumulative + p6_cumulative + p4_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

eg6 = paste0("~ p6_cumulative + p7_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)

eg7 = paste0("~ p7_cumulative + p8_cumulative + factor(nfhs5_state) + pc1 + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
