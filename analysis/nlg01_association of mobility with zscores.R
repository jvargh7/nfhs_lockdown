rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_cumulative_restriction.R")
source("analysis/nlg_analytic sample processing.R")
association_cumulative_restriction(prefix = "nlg",
                                   svy_des = list(g1_svy,
                                                  g2_svy,
                                                  g3_svy,
                                                  g4_svy,
                                                  g5_svy,
                                                  g6_svy,
                                                  g7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7) %>% map(.,function(x) paste0(x," + m_rural")))


source("analysis/nlr_analytic sample processing.R")
association_cumulative_restriction(prefix = "nlr",
                                   svy_des = list(r1_svy,
                                                  r2_svy,
                                                  r3_svy,
                                                  r4_svy,
                                                  r5_svy,
                                                  r6_svy,
                                                  r7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7))

source("analysis/nlu_analytic sample processing.R")
association_cumulative_restriction(prefix = "nlu",
                                   svy_des = list(u1_svy,
                                                  u2_svy,
                                                  u3_svy,
                                                  u4_svy,
                                                  u5_svy,
                                                  u6_svy,
                                                  u7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7))