nlpre_preprocessing <- function(df){
  
  
  df %>%
    
    mutate(sampleweight = v005/(10^6)) %>% 
    
    mutate(c_day_orig = c_birthday,
           c_birthdate = case_when(is.na(c_birthday) | c_birthday == 98 ~ 15,
                            c_birthmonth == 2 & c_birthyear %in% c(2008,2012,2016,2020) & c_birthday > 29 ~ 29,
                            c_birthmonth == 2 & c_birthday > 28 ~ 28,
                            c_birthmonth %in% c(4,6,9,11) & c_birthday > 30 ~ 30,
                            TRUE ~ as.numeric(c_birthday)),
           v016 = case_when(is.na(v016) | v016 == 98 ~ 15,
                            v006 == 2 & v006 %in% c(2008,2012,2016,2020) & v016 > 29 ~ 29,
                            v006 == 2 & v016 > 28 ~ 28,
                            v006 %in% c(4,6,9,11) & v016 > 30 ~ 30,
                            TRUE ~ as.numeric(v016)),
    ) %>%
    mutate(c_dob = as_date(paste0(c_birthyear,"-",c_birthmonth,"-",c_birthdate)),
           c_interview = as_date(paste0(v007,"-",v006,"-",v016)),
           phase = case_when(c_interview <= "2016-12-31" ~ 0,
                             c_interview <= "2020-03-23" ~ 1,
                             c_interview > "2020-03-23" ~ 2,
                             TRUE ~ NA_real_)) %>% 
    
    mutate( c_haz = case_when(c_haz >= 9996 ~ NA_real_,
                             is.na(c_haz) ~ NA_real_,
                             TRUE ~ c_haz/100),
           c_waz = case_when(c_waz >= 9996 ~ NA_real_,
                             is.na(c_waz) ~ NA_real_,
                             TRUE ~ c_waz/100),
           c_whz = case_when(c_whz >= 9996 ~ NA_real_,
                             is.na(c_whz) ~ NA_real_,
                             TRUE ~ c_whz/100),
           c_bmiz = case_when(c_bmiz >= 9996 ~ NA_real_,
                              is.na(c_bmiz) ~ NA_real_,
                              TRUE ~ c_bmiz/100)) %>% 
    
    mutate(c_male = case_when(c_sex == 1 ~ 1,
                              c_sex == 2 ~ 0,
                              TRUE ~ NA_real_),
           c_stunting = case_when(c_haz >= 9996 ~ NA_real_,
                                  is.na(c_haz) ~ NA_real_,
                                  c_haz < -200 ~ 1,
                                  TRUE ~ 0),
           c_underweight = case_when(c_waz >= 9996 ~ NA_real_,
                                     is.na(c_waz) ~ NA_real_,
                                     c_waz < -200 ~ 1,
                                     TRUE ~ 0),
           c_wasting = case_when(c_whz >= 9996 ~ NA_real_,
                                 is.na(c_whz) ~ NA_real_,
                                 c_whz < -200 ~ 1,
                                 TRUE ~ 0),
           c_severewasting = case_when(c_whz >= 9996 ~ NA_real_,
                                       is.na(c_whz) ~ NA_real_,
                                       c_whz < -300 ~ 1,
                                       TRUE ~ 0),
           c_overweight = case_when(c_whz >= 9996 ~ NA_real_,
                                    is.na(c_whz) ~ NA_real_,
                                    c_whz > 200 ~ 1,
                                    TRUE ~ 0)
           
    ) %>% 
    
    # # HB
    # mutate_at(vars(v456,hb56), function(x) case_when(x >= 250 | x < 3 ~ NA_real_,
    #                                                  TRUE ~ as.numeric(x))) %>% 
    # BMI
    mutate(m_bmi = case_when(m_pregnant == 1 ~ NA_real_,
                             m_bmi > 6000 ~ NA_real_,
                            TRUE ~ m_bmi/100),
           m_height = case_when(m_height > 2500 ~ NA_real_,
                                TRUE ~ m_height/10),
           m_weightstatus = case_when(m_bmi < 18.5 ~ "Underweight",
                                      m_bmi < 25 ~ "Normal",
                                      m_bmi >= 25 ~ "Overweight or Obese",
                                      TRUE ~ NA_character_)) %>% 
    # # Glucose
    # mutate_at(vars(smb70,sb70), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
    #                                                   TRUE ~ as.numeric(x))) %>% 
    # Caste
    mutate_at(vars(m_caste),function(x) case_when(x == 1 ~ "Schedule Caste",
                                               x == 2 ~ "Schedule Tribe",
                                               x == 3 ~ "OBC",
                                               x == 4 ~ "General",
                                               x == 8 ~ "General",
                                               # CASTE IMPUTATION -----------
                                               TRUE ~ "General")) %>% 
    # Education
    mutate_at(vars(m_education,f_education),function(x) case_when(x == 0 ~ "No education",
                                               x == 1 ~ "Primary",
                                               x == 2 ~ "Secondary",
                                               x == 3 ~ "Higher",
                                               x == 9 ~ NA_character_,
                                               TRUE ~ NA_character_)) %>% 
    # Religion
    mutate_at(vars(hh_religion),function(x) case_when(x == 1 ~ "Hindu",
                                               x == 2 ~ "Muslim",
                                               TRUE ~ "Other")) %>% 
    
    # alcohol
    mutate_at(vars(m_alcohol), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(v463z), function(x) case_when(x == 1 ~ 0,
                                                 x == 0 ~ 1,
                                                 TRUE ~ NA_real_)) %>% 
    
    
    # mutate_at(vars(v501,mv501,
    #                s301,sm213), function(x) case_when(x %in% c(2) ~ 0,
    #                                                   x %in% c(1) ~ 1,
    #                                                   TRUE ~ NA_real_)) %>% 
    mutate(
      m_rural = case_when(v025 == 1 ~ 0,
                       v025 == 2 ~ 1,
                       TRUE ~ NA_real_),
      
      m_employment = case_when(v714 == 9 ~ NA_real_,
                               TRUE ~ as.numeric(v714)),
      
      
    ) %>% 
    
    # ICDS -----
  mutate_at(vars(icds_child_thrfreq), function(x) case_when(x %in% c(0,4,8) ~ 11, # Never, Less than once a month, don't know
                                                            x %in% c(1,2,3) ~ 12, # Daily, weekly, Monthly
                                                            TRUE ~ NA_real_)) %>% 
    mutate_at(vars(icds_child_healthfreq,icds_child_visitfreq), function(x) case_when(x %in% c(0,2,8) ~ 11, #Never, Less often, Don't know
                                                                 x %in% c(1) ~ 12, # Atleast once a month
                                                                 TRUE ~ NA_real_)) %>% 
    mutate_at(vars(icds_child_weightfreq), function(x) case_when(x %in% c(0,3,8) ~ 11, # 
                                                                 x %in% c(1,2) ~ 12,
                                                                 TRUE ~ NA_real_)) %>% 
    mutate_at(vars(starts_with("icds_child")), function(x) case_when(x == 11 ~ 0,
                                                                     x == 12 ~ 1,
                                                                     TRUE ~ NA_real_)) %>% 
    
    mutate_at(vars(icds_child_weightcounselling), function(x) case_when(x %in% c(0,8) ~ 0,
                                                                        x %in% c(1) ~ 1,
                                                                        TRUE ~ NA_real_)) %>% 
    mutate_at(vars(starts_with("icds_preg"),starts_with("icds_bf")), function(x) case_when(x == 0 ~ 0,
                                                                                           x == 1 ~ 1,
                                                                                           x == 3 ~ NA_real_, # for icds_bf_benefits
                                                                                           TRUE ~ NA_real_)) %>% 
  
  
    
    mutate(m_employment = factor(m_employment,levels=c("no","former","current"))) %>% 
    dplyr::rename(
                  m_smoking = v463z) %>% 
    dplyr::select(v000:v007,sampleweight,v021,v023,v024,sdist,phase,
                  starts_with("c_"),starts_with("m_"),starts_with("hh_"),starts_with("f_"),starts_with("icds_")) %>% 
    return(.)
  
  
  
}