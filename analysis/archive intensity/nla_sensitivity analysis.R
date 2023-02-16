
# Z-scores -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/nla_analytic sample processing.R")
association_mobility_restriction(prefix = "ns1",
                                 svy_des = analytic_svy,
                                 o_vars = z_vars,
                                 family_svyglm = "gaussian",
                                 eqA=e01a,
                                 eqB=e01b,
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns1",
                        svy_des = analytic2_svy,
                        o_vars = z_vars,
                        family_svyglm = "gaussian",
                        eqA = e02a,
                        eqB = e02b,
                        eqC = e02c,
                        eqD = e02d,
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")

# Boys -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns2_analytic sample processing.R")

association_mobility_restriction(prefix = "ns2",
                                 svy_des = boys_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=str_replace(e01a,"\\+ c_sex",""),
                                 eqB=str_replace(e01b,"\\+ c_sex",""),
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns2",
                        svy_des = boys2_svy,
                        o_vars = outcome_vars,
                        family_svyglm = "quasipoisson",
                        eqA = str_replace(e02a,"\\+ c_sex",""),
                        eqB = str_replace(e02b,"\\+ c_sex",""),
                        eqC = str_replace(e02c,"\\+ c_sex",""),
                        eqD = str_replace(e02d,"\\+ c_sex",""),
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")


# Girls -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns3_analytic sample processing.R")
association_mobility_restriction(prefix = "ns3",
                                 svy_des = girls_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=str_replace(e01a,"\\+ c_sex",""),
                                 eqB=str_replace(e01b,"\\+ c_sex",""),
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns3",
                        svy_des = girls2_svy,
                        o_vars = outcome_vars,
                        family_svyglm = "quasipoisson",
                        eqA = str_replace(e02a,"\\+ c_sex",""),
                        eqB = str_replace(e02b,"\\+ c_sex",""),
                        eqC = str_replace(e02c,"\\+ c_sex",""),
                        eqD = str_replace(e02d,"\\+ c_sex",""),
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")

# Linear splines with knots --------
rm(list=ls()); gc(); source(".Rprofile")
source("analysis/ns4_analytic sample processing.R")
prefix = "ns4"
source("analysis/ns4_main analysis with splines.R")

# Conceived before March 25, 2020 -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns5_analytic sample processing.R")
association_mobility_restriction(prefix = "ns5",
                                 svy_des = conceived_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=e01a,
                                 eqB=e01b,
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns5",
                        svy_des = conceived2_svy,
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


# District-cases -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns6_analytic sample processing.R")
association_mobility_restriction(prefix = "ns6",
                                 svy_des = district_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=str_replace(e01a,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                                 eqB=str_replace(e01b,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns6",
                        svy_des = district2_svy,
                        o_vars = outcome_vars,
                        family_svyglm = "quasipoisson",
                        eqA = str_replace(e02a,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                        eqB = str_replace(e02b,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                        eqC = str_replace(e02c,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                        eqD = str_replace(e02d,"\\+ c_sex","+ c_sex + exposure_acases + Zone + pc1 "),
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")

# Cluster covariates -------------

rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns7_analytic sample processing.R")
association_mobility_restriction(prefix = "ns7",
                                 svy_des = cluster_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=str_replace(e01a,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                                 eqB=str_replace(e01b,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns7",
                        svy_des = cluster2_svy,
                        o_vars = outcome_vars,
                        family_svyglm = "quasipoisson",
                        eqA = str_replace(e02a,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                        eqB = str_replace(e02b,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                        eqC = str_replace(e02c,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                        eqD = str_replace(e02d,"\\+ c_sex","+ c_sex + bpl_card + icds "),
                        modifier2a="age_le24",
                        modifier2b="age_le24",
                        exposure2a = "phase2",
                        exposure2b = "exposure_estimate_centered")


# Measured before April 1, 2021 -------------
rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_mobility_restriction.R")
source("functions/sensitive_age_intervals.R")
source("analysis/ns8_analytic sample processing.R")
association_mobility_restriction(prefix = "ns8",
                                 svy_des = predelta_svy,
                                 o_vars = outcome_vars,
                                 family_svyglm = "quasipoisson",
                                 eqA=e01a,
                                 eqB=e01b,
                                 exposure1b = "exposure_estimate_centered",
                                 modifier1b = "exposure_gt20_centered")

sensitive_age_intervals(prefix = "ns8",
                        svy_des = predelta2_svy,
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