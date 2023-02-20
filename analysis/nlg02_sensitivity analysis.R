rm(list=ls()); gc(); source(".Rprofile")
source("functions/association_cumulative_restriction.R")
source("analysis/ns1_analytic sample processing.R")

association_cumulative_restriction(prefix = "ns1f",
                                   svy_des = list(f1_svy,
                                                  f2_svy,
                                                  f3_svy,
                                                  f4_svy,
                                                  f5_svy,
                                                  f6_svy,
                                                  f7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7) %>% map(.,function(x) str_replace(x,"\\+ c_male","")))

association_cumulative_restriction(prefix = "ns1m",
                                   svy_des = list(m1_svy,
                                                  m2_svy,
                                                  m3_svy,
                                                  m4_svy,
                                                  m5_svy,
                                                  m6_svy,
                                                  m7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7) %>% map(.,function(x) str_replace(x,"\\+ c_male","")))


source("analysis/nlr_analytic sample processing.R")

association_cumulative_restriction(prefix = "ns2",
                                   svy_des = list(r1_svy,
                                                  r2_svy,
                                                  r3_svy,
                                                  r4_svy,
                                                  r5_svy,
                                                  r6_svy,
                                                  r7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7) %>% map(.,function(x) str_replace(x,"\\+ c_male ",
                                                                                                          "+ c_male + exposure_acases + Zone ")))
                                                                                                          


source("analysis/ns3_analytic sample processing.R")

association_cumulative_restriction(prefix = "ns3",
                                   svy_des = list(c1_svy,
                                                  c2_svy,
                                                  c3_svy,
                                                  c4_svy,
                                                  c5_svy,
                                                  c6_svy,
                                                  c7_svy),
                                   o_vars = z_vars,
                                   family_svyglm = "gaussian",
                                   eq=list(eg1,eg2,eg3,eg4,eg5,eg6,eg7) %>% map(.,function(x) str_replace(x,"\\+ c_male ",
                                                                                                          "+ c_male + bpl_card + icds ")))
