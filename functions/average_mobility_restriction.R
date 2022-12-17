
# qc:
# d = seq.Date(from=ymd("2014-01-01"),to=ymd("2018-01-01"),by="day")
# d1 = (d[d >= (ymd("2015-01-01") - 38*7) & d <= (ymd("2015-01-01") + 733) ])


average_mobility_restriction <- function(child_dob,state_id,district_id,measurement_date){
  
  print(child_dob);
  
  if(state_id != 34){
    exposure = readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
      # v024 ~ v024_nfhs5
      dplyr::filter(v024 == state_id)  %>% 
      dplyr::filter(date <= measurement_date) 
  } else{
    
    exposure = readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
      dplyr::filter(v024 == state_id,sdist == district_id) %>% 
      dplyr::filter(date <= measurement_date)
     
  }
  
  exposure <- exposure %>% 
    mutate(period = case_when(date >= (child_dob - (38*7)) &  date < child_dob ~ "p1_estimate",
                              date >= child_dob & date < (child_dob + (30.5*6)) ~ "p2_estimate",
                              date >= (child_dob + (30.5*6)) & date < (child_dob + (30.5*24)) ~ "p3_estimate",
                              TRUE ~ "p4_estimate"))
  
  
  exposure_estimate = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7) %>% 
    dplyr::summarize(exposure_estimate = mean(mobility_composite,na.rm=TRUE),
                     exposure_gt20 = sum(mobility_composite < (-20),na.rm=TRUE),
                     exposure_n = sum(!is.na(mobility_composite)))
  
  period_estimates = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7) %>% 
    group_by(period) %>% 
    summarize(est = mean(mobility_composite,na.rm=TRUE),
              gt20 = sum(mobility_composite < (-20),na.rm=TRUE),
              n = sum(!is.na(mobility_composite))) %>% 
    ungroup()
    

  exposure_estimate %>%  
    bind_cols(period_estimates %>% 
                dplyr::select(-n,-gt20) %>% 
                pivot_wider(names_from="period",values_from="est"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","gt20")) %>% 
                dplyr::select(-est,-n) %>% 
                pivot_wider(names_from="period",values_from="gt20"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","n")) %>% 
                dplyr::select(-est,-gt20) %>% 
                pivot_wider(names_from="period",values_from="n")
              ) %>% 

    return(.)
  
  
}

