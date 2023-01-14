rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("analysis/nla_analytic sample processing.R")
association_mobility_restriction(prefix = "nap",
                                 svy_des = analytic_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=e05a,
                                 eqB=e05b,
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")



