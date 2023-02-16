
# qc:
# d = seq.Date(from=ymd("2014-01-01"),to=ymd("2018-01-01"),by="day")
# d1 = (d[d >= (ymd("2015-01-01") - 38*7) & d <= (ymd("2015-01-01") + 733) ])

all_district_cases <- readRDS(paste0(path_lockdown_folder,"/working/district_cases.RDS"))
india_cmi_imputed_step2 <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS"))

average_mobility_restriction <- function(child_dob,state_id,district_id,measurement_date){
  
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
    mutate(period = case_when(date >= (child_dob - (38*7)) &  date < child_dob ~ "p1_estimate",
                              date >= child_dob & date < (child_dob + (30.5*6)) ~ "p2_estimate",
                              date >= (child_dob + (30.5*6)) & date < (child_dob + (30.5*24)) ~ "p3_estimate",
                              TRUE ~ "p4_estimate"),
           
           
           window = case_when(date >= (child_dob - (12*30.5)) & date < (child_dob - (9*30.5)) ~ "bm12_estimate",
                              date >= (child_dob - (9*30.5)) & date < (child_dob - (6*30.5)) ~ "bm09_estimate",
                              date >= (child_dob - (6*30.5)) & date < (child_dob - (3*30.5)) ~ "bm06_estimate",
                              date >= (child_dob - (3*30.5)) & date < (child_dob) ~ "bm3_estimate",
                              date >= (child_dob) & date < (child_dob + (3*30.5)) ~ "bp0_estimate",
                              date >= (child_dob + (3*30.5)) & date < (child_dob + (6*30.5)) ~ "bp03_estimate",
                              date >= (child_dob + (6*30.5)) & date < (child_dob + (9*30.5)) ~ "bp06_estimate",
                              date >= (child_dob + (9*30.5)) & date < (child_dob + (12*30.5)) ~ "bp09_estimate",
                              date >= (child_dob + (12*30.5)) & date < (child_dob + (15*30.5)) ~ "bp12_estimate",
                              date >= (child_dob + (15*30.5)) & date < (child_dob + (18*30.5)) ~ "bp15_estimate",
                              date >= (child_dob + (18*30.5)) & date < (child_dob + (21*30.5)) ~ "bp18_estimate",
                              date >= (child_dob + (21*30.5)) & date < (child_dob + (24*30.5)) ~ "bp21_estimate",
                              date >= (child_dob + (24*30.5)) & date < (child_dob + (27*30.5)) ~ "bp24_estimate",
                              date >= (child_dob + (27*30.5)) & date < (child_dob + (30*30.5)) ~ "bp27_estimate",
                              date >= (child_dob + (30*30.5)) & date < (child_dob + (33*30.5)) ~ "bp30_estimate",
                              date >= (child_dob + (33*30.5)) & date < (child_dob + (36*30.5)) ~ "bp33_estimate",
                              date >= (child_dob + (36*30.5)) & date < (child_dob + (39*30.5)) ~ "bp36_estimate",
                              date >= (child_dob + (39*30.5)) & date < (child_dob + (42*30.5)) ~ "bp39_estimate",
                              date >= (child_dob + (42*30.5)) & date < (child_dob + (45*30.5)) ~ "bp42_estimate",
                              date >= (child_dob + (45*30.5)) & date < (child_dob + (48*30.5)) ~ "bp45_estimate",
                              date >= (child_dob + (48*30.5)) & date < (child_dob + (51*30.5)) ~ "bp48_estimate",
                              date >= (child_dob + (51*30.5)) & date < (child_dob + (54*30.5)) ~ "bp51_estimate",
                              date >= (child_dob + (54*30.5)) & date < (child_dob + (57*30.5)) ~ "bp54_estimate",
                              date >= (child_dob + (57*30.5)) & date < (child_dob + (60*30.5)) ~ "bp57_estimate", # Catch-all up to 60 months
                              TRUE ~ NA_character_
           ))
  
  
  exposure_estimate = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7) %>% 
    dplyr::summarize(exposure_estimate = mean(mobility_composite,na.rm=TRUE),
                     exposure_gt20 = sum(mobility_composite < (-20),na.rm=TRUE),
                     exposure_n = sum(!is.na(mobility_composite)),
                     exposure_lcases = sum(district_imputed),
                     exposure_acases = mean(district_imputed))
  
  period_estimates = exposure %>% 
    dplyr::filter(date >= child_dob - 38*7) %>% 
    group_by(period) %>% 
    summarize(est = mean(mobility_composite,na.rm=TRUE),
              gt20 = sum(mobility_composite < (-20),na.rm=TRUE),
              n = sum(!is.na(mobility_composite)),
              lcases = sum(district_imputed),
              acases = mean(district_imputed)) %>% 
    ungroup()
    
  
  exposure_window = exposure %>% 
    dplyr::filter(!is.na(window)) %>% 
    group_by(window) %>% 
    summarize(est = mean(mobility_composite,na.rm=TRUE)) %>% 
    dplyr::select(window,est) %>% 
    pivot_wider(names_from="window",values_from="est") %>% 
    dplyr::select(starts_with("bm"),starts_with("bp"))

  exposure_estimate %>%  
    bind_cols(period_estimates %>% 
                dplyr::select(-n,-gt20,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="est"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","gt20")) %>% 
                dplyr::select(-est,-n,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="gt20"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","n")) %>% 
                dplyr::select(-est,-gt20,-lcases,-acases) %>% 
                pivot_wider(names_from="period",values_from="n"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","lcases")) %>% 
                dplyr::select(-est,-gt20,-n,-acases) %>% 
                pivot_wider(names_from="period",values_from="lcases"),
              
              period_estimates %>% 
                mutate(period = str_replace(period,"estimate","acases")) %>% 
                dplyr::select(-est,-gt20,-n,-lcases) %>% 
                pivot_wider(names_from="period",values_from="acases"),
              
              exposure_window
              ) %>% 

    return(.)
  
  
}

