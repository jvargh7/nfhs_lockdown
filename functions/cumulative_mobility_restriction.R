
# qc:
# d = seq.Date(from=ymd("2014-01-01"),to=ymd("2018-01-01"),by="day")
# d1 = (d[d >= (ymd("2015-01-01") - 38*7) & d <= (ymd("2015-01-01") + 733) ])

all_district_cases <- readRDS(paste0(path_lockdown_folder,"/working/district_cases.RDS"))
india_cmi_imputed_step2 <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS"))

cumulative_mobility_restriction <- function(child_dob,state_id,district_id,measurement_date){
  
  print(child_dob);
  district_cases <- all_district_cases %>% 
    dplyr::filter(sdist == district_id) %>% 
    dplyr::filter(var_name <= measurement_date) 
  
  if(state_id != 34){
    exposure = india_cmi_imputed_step2 %>% 
      # v024 ~ v024_nfhs5
      dplyr::filter(v024 == state_id)  %>% 
      dplyr::filter(date <= measurement_date) 
  } else{
    
    exposure = india_cmi_imputed_step2 %>% 
      dplyr::filter(v024 == state_id,sdist == district_id) %>% 
      dplyr::filter(date <= measurement_date)
    
  }
  
  exposure <- exposure %>% 
    left_join(district_cases %>% dplyr::select(var_name,district_imputed),
              by = c("date" = "var_name")) %>% 
    # To make sure we don't average over NAs
    mutate(district_imputed = case_when(is.na(district_imputed) ~ 0,
                                        TRUE ~ district_imputed)) %>% 
    mutate(period = case_when(date >= (child_dob - (38*7)) &  date < child_dob ~ "p1_cumulative",
                              date >= child_dob & date < (child_dob + (30.5*6)) ~ "p2_cumulative",
                              date >= (child_dob + (30.5*6)) & date < (child_dob + (30.5*12)) ~ "p3_cumulative",
                              date >= (child_dob + (30.5*12)) & date < (child_dob + (30.5*18)) ~ "p4_cumulative",
                              date >= (child_dob + (30.5*18)) & date < (child_dob + (30.5*24)) ~ "p5_cumulative",
                              date >= (child_dob + (30.5*24)) & date < (child_dob + (30.5*36)) ~ "p6_cumulative",
                              date >= (child_dob + (30.5*36)) & date < (child_dob + (30.5*48)) ~ "p7_cumulative",
                              date >= (child_dob + (30.5*48)) & date < (child_dob + (30.5*60)) ~ "p8_cumulative",
                              TRUE ~ NA_character_))
  
  
  exposure_estimate = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7,date <= lockdown_stop) %>% 
    dplyr::summarize(exposure_cumulative = sum(mobility_composite,na.rm=TRUE),
                     exposure_gt40 = sum(mobility_composite < (-40),na.rm=TRUE),
                     exposure_n = sum(!is.na(mobility_composite)),
                     exposure_lcases = sum(district_imputed),
                     exposure_acases = mean(district_imputed))
  
  period_estimates = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7, date <= lockdown_stop) %>% 
    group_by(period) %>% 
    summarize(cumulative = sum(mobility_composite,na.rm=TRUE),
              gt40 = sum(mobility_composite < (-40),na.rm=TRUE),
              n = sum(!is.na(mobility_composite)),
              lcases = sum(district_imputed),
              acases = mean(district_imputed)) %>% 
    ungroup()
  
  
  exposure_estimate %>%  
    bind_cols(period_estimates %>% 
                dplyr::select(-n,-gt40,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="cumulative"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"cumulative","gt40")) %>% 
                dplyr::select(-cumulative,-n,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="gt40"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"cumulative","n")) %>% 
                dplyr::select(-cumulative,-gt40,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="n"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"cumulative","lcases")) %>% 
                dplyr::select(-cumulative,-gt40,-n,-acases) %>% 
                pivot_wider(names_from="period",values_from="lcases"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"cumulative","acases")) %>% 
                dplyr::select(-cumulative,-gt40,-n,-lcases) %>% 
                pivot_wider(names_from="period",values_from="acases")
    ) %>% 
    
    return(.)
  
  
}

