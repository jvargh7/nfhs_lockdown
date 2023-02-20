source("analysis/nlg_equations.R")


nlg_prediction_dfs = function(r,curr_change, pre_change){
  g1_df = analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G1",m_rural == r) %>% 
    mutate(p1_cumulative = case_when(p1_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p1_cumulative + pre_change),
           p2_cumulative = case_when(p2_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p2_cumulative + curr_change))
  
  g2_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G2",m_rural == r) %>% 
    mutate(p2_cumulative = case_when(p2_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p2_cumulative + pre_change),
           p3_cumulative = case_when(p3_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p3_cumulative + curr_change))
  
  
  g3_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G3",m_rural == r)  %>% 
    mutate(p3_cumulative = case_when(p3_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p3_cumulative + pre_change),
           p4_cumulative = case_when(p4_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p4_cumulative + curr_change))
  
  
  g4_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G4",m_rural == r)  %>% 
    mutate(p4_cumulative = case_when(p4_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p4_cumulative + pre_change),
           p5_cumulative = case_when(p5_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p5_cumulative + curr_change))
  
  g5_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G5",m_rural == r) %>% 
    mutate(p5_cumulative = case_when(p5_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p5_cumulative + pre_change),
           p6_cumulative = case_when(p6_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p6_cumulative + curr_change))
  
  
  g6_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G6",m_rural == r)%>% 
    mutate(p6_cumulative = case_when(p6_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p6_cumulative + pre_change),
           p7_cumulative = case_when(p7_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p7_cumulative + curr_change))
  
  g7_df <- analytic_sample_wdistrict %>% 
    dplyr::filter(group == "G7",m_rural == r)  %>% 
    mutate(p7_cumulative = case_when(p7_cumulative + pre_change < 0 ~ 0,
                                     TRUE ~ p7_cumulative + pre_change),
           p8_cumulative = case_when(p8_cumulative + curr_change < 0 ~ 0,
                                     TRUE ~ p8_cumulative + curr_change))
  
  
  
  g1_mat = model.matrix(as.formula(eg1),g1_df)
  g2_mat = model.matrix(as.formula(eg2),g2_df)
  g3_mat = model.matrix(as.formula(eg3),g3_df)
  g4_mat = model.matrix(as.formula(eg4),g4_df)
  g5_mat = model.matrix(as.formula(eg5),g5_df)
  g6_mat = model.matrix(as.formula(eg6),g6_df)
  g7_mat = model.matrix(as.formula(eg7),g7_df)
  
  
  list(g1_mat,
       g2_mat,
       g3_mat,
       g4_mat,
       g5_mat,
       g6_mat,
       g7_mat) %>% 
    return(.)
  
}


get_predictions = function(mat,coef_mat,vcov_mat,wt){
  
  y = mat %*% coef_mat
  
  n = sum(wt > 0)
  # n = nrow(mat)
  
  var_y = Hmisc::wtd.var(y,weights=wt)
  mean_y = Hmisc::wtd.mean(y,weights = wt)
  
  se_y = sqrt(var_y/n)
  
  data.frame(mean = mean_y,
             se = se_y) %>% 
    return(.)
  
}


run_prediction_dfs = function(x,g,rural){
  
  if(rural == 1){
    
    nl_summary = nlr01_summary
    
  } else{nl_summary = nlu01_summary}
  
  # Each Beta is separate for HAZ, WAZ, WHZ, BMIZ
  # HAZ: _summary[[1]][[.]]
  haz_beta = nl_summary[[1]][[1]] %>% 
    dplyr::filter(model == paste0("m",g)) %>% 
    dplyr::select(estimate) %>% 
    pull();
  
  # WAZ: _summary[[2]][[.]]
  waz_beta = nl_summary[[2]][[1]] %>% 
    dplyr::filter(model == paste0("m",g))%>% 
    dplyr::select(estimate) %>% 
    pull();
  
  # WHZ: _summary[[3]][[.]]
  whz_beta = nl_summary[[3]][[1]] %>% 
    dplyr::filter(model == paste0("m",g))%>% 
    dplyr::select(estimate) %>% 
    pull();
  
  # Each VCOV is separate for HAZ, WAZ, WHZ, BMIZ
  
  haz_vcov = nl_summary[[1]][[2]][[g]];
  waz_vcov = nl_summary[[2]][[2]][[g]];
  whz_vcov = nl_summary[[3]][[2]][[g]];
  
  sw = analytic_sample_wdistrict %>% 
    dplyr::filter(group == paste0("G",g),m_rural == rural) %>% 
    mutate(sampleweight = case_when(phase == 1 ~ 0,
                                    TRUE ~ sampleweight)) %>% 
    dplyr::select(sampleweight) %>% 
    pull();
  
  bind_rows(
    get_predictions(mat = x,
                    coef_mat = haz_beta,
                    vcov_mat = haz_vcov,
                    wt = sw) %>% 
      mutate(outcome = "HAZ"),
    
    get_predictions(mat = x,
                    coef_mat = waz_beta,
                    vcov_mat = waz_vcov,
                    wt = sw) %>% 
      mutate(outcome = "WAZ"),
    get_predictions(mat = x,
                    coef_mat = whz_beta,
                    vcov_mat = whz_vcov,
                    wt = sw) %>% 
      mutate(outcome = "WHZ")
    
  ) %>% 
    return(.)
  
  
}
