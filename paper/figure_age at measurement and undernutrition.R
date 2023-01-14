source("analysis/nla_analytic sample processing.R")

analytic_sample %>% 
  dplyr::filter(phase == 2) %>% 
  summarize(
            r = cor(exposure_estimate,c_age),
            r2 = cor(exposure_estimate,exposure_gt20))





analytic_sample %>% 
  dplyr::filter(phase == 1) %>% 
  summarize(r = cor(exposure_estimate,c_age),
            r2 = cor(exposure_estimate,exposure_gt20))

source("C:/code/external/functions/survey/svysummary.R")

p_vars = c("c_stunting","c_underweight","c_wasting","c_overweight")
id_vars = c("phase","age_le24","age_ge36")

n5_sy <- svysummary(analytic_svy,
                    # c_vars = c_vars,
                    p_vars = p_vars,
                    # g_vars = g_vars,
                    id_vars = id_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

n5_sy %>% 
  dplyr::select(one_of(id_vars),variable,est_ci) %>% 
  pivot_wider(names_from=variable,values_from=est_ci) %>% 
  View()
