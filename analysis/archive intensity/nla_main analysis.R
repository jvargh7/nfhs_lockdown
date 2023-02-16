rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/nla_analytic sample processing.R")
association_mobility_restriction(prefix = "nla",
                                 svy_des = analytic_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=e01a,
                                 eqB=e01b,
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")


sensitive_age_intervals(prefix = "nla",
                        svy_des = analytic2_svy,
                        o_vars = outcome_vars,
                        family_svyglm = "quasipoisson",
                        eqA = e02a,
                        eqB = e02b,
                        eqC = e02c,
                        eqD = e02d,
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")
