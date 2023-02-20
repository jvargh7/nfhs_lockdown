require(tidyverse)
require(survey)
require(srvyr)
require(lubridate)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
path_lockdown_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS Lockdown"
path_lockdown_repo <- "C:/code/external/nfhs_lockdown"
path_covid19india_data <- "C:/Cloud/OneDrive - Emory University/data/covid19 india"

lockdown_start <- "2020-03-25"
lockdown_stop <- "2020-09-24"

delta_start <- "2021-04-01"
delta_stop <- "2021-07-01"

demonetization_start <- "2016-11-09"
demonetization_stop <- "2017-06-30"

nfhs4_start <- "2015-01-20"
nfhs4_stop <- "2016-12-04"

nfhs5p1_start <- "2019-06-17"
nfhs5p1_stop <- "2020-03-21"

nfhs5p2_start <- "2020-11-21"
nfhs5p2_stop <- "2021-04-30"

v024_nfhs5_14states <- c(12,4,22,6,20,23,7,21,34,3,8,33,9,5)


region_covariates <- " + age_categories + nfhs5 + c_male + m_caste + m_religion + m_age + m_education + m_height + factor(sdistri) + factor(c_month)"
spline_covariates <- " + ns(c_age,df=4) + nfhs5 + c_male + m_caste + m_religion + m_age + m_education + m_height + factor(sdistri) + factor(c_month)"
total_covariates <- paste0(" + m_rural",region_covariates)

interaction_covariates <- " + age_categories*m_rural + nfhs5*m_rural + c_male*m_rural + m_caste*m_rural + m_religion*m_rural + m_age*m_rural + m_education*m_rural + m_height*m_rural + factor(sdistri) + factor(c_month)"


as_formula = function(...){as.formula(...)}


source("functions/save_svyglm.R")
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
