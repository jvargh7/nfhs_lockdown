
c_covariates = c("+ c_age3mo + c_sex")
m_covariates = c("+ m_age_centered + m_eduyr_centered + m_height_centered")
# hh_wealthqur: not used 
hh_covariates = c("+ m_caste + hh_religion")
outcome_vars = c("c_stunting","c_underweight","c_wasting")
z_vars = c("c_haz","c_waz","c_whz","c_bmiz")

e01a = paste0("~ exposure_estimate_centered + exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e01b = paste0("~ exposure_estimate_centered*exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e01c = paste0("~ exposure_estimate*m_rural + exposure_gt20*m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e01d = paste0("~ exposure_estimate*exposure_gt20*m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


e02a = paste0("~ phase2*age_le24 + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e02b = paste0("~ exposure_estimate_centered*age_le24 + exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e02c = paste0("~ phase2 + p1_estimate_centered + p2_estimate_centered + p3_estimate_centered + exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e02d = paste0("~ p1_estimate_centered + p2_estimate_centered + p3_estimate_centered + p4_estimate_centered + 
              exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


e03a = paste0("~ c_age3mo + phase2 + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e03b = paste0("~ phase2*c_age3mo + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


e04a = paste0("~ exposure_estimate + exposure_estimate_gt10 + exposure_estimate_gt20 + exposure_estimate_gt30 + exposure_gt20_centered + m_rural + factor(sdist) + factor(c_measurementmonth) ",
              c_covariates,m_covariates,hh_covariates)

e05a = paste0("~ exposure_estimate_centered + exposure_gt20_centered + c_age3mo*phase2 + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)
e05b = paste0("~ exposure_estimate_centered*exposure_gt20_centered + c_age3mo*phase2 + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


e06a = paste0("~ phase2*c_age3mo + m_rural + factor(sdist) + factor(c_measurementmonth) ",c_covariates,m_covariates,hh_covariates)


# Which variable mattered?

