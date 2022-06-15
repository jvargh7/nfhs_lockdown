require(haven)
require(tidyverse)
require(lubridate)

iakr7a_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth")$iakr7adt %>% na.omit(.)
iair7a_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth")$iair7adt %>% na.omit(.)


nfhs5 <- read_dta(paste0(path_dhs_data,"/IA/IAKR7ADT/IAKR7AFL.dta"),col_select = iakr7a_variables) %>%
  
  mutate(sampleweight = v005/(10^6)) %>% 
  
  mutate(c_day_orig = hw16,
         hw16 = case_when(is.na(hw16) | hw16 == 98 ~ 15,
                          b1 == 2 & b2 %in% c(2008,2012,2016,2020) & hw16 > 29 ~ 29,
                          b1 == 2 & hw16 > 28 ~ 28,
                          b1 %in% c(4,6,9,11) & hw16 > 30 ~ 30,
                          TRUE ~ as.numeric(hw16)),
         v016 = case_when(is.na(v016) | v016 == 98 ~ 15,
                          v006 == 2 & v006 %in% c(2008,2012,2016,2020) & v016 > 29 ~ 29,
                          v006 == 2 & v016 > 28 ~ 28,
                          v006 %in% c(4,6,9,11) & v016 > 30 ~ 30,
                          TRUE ~ as.numeric(v016)),
  ) %>%
  mutate(c_dob = as_date(paste0(b2,"-",b1,"-",hw16)),
         c_interview = as_date(paste0(v007,"-",v006,"-",v016)),
         c_month = hw18,
         phase = case_when(c_interview <= "2020-03-23" ~ 1,
                           c_interview > "2020-03-23" ~ 2,
                           TRUE ~ NA_real_)) %>% 
  
  mutate(c_male = case_when(b4 == 1 ~ 1,
                            b4 == 2 ~ 0,
                            TRUE ~ NA_real_),
         c_age = hw1,
         c_stunting = case_when(hw70 >= 9996 ~ NA_real_,
                                is.na(hw70) ~ NA_real_,
                                hw70 < -200 ~ 1,
                                TRUE ~ 0),
         c_underweight = case_when(hw71 >= 9996 ~ NA_real_,
                                   is.na(hw71) ~ NA_real_,
                                   hw71 < -200 ~ 1,
                                   TRUE ~ 0),
         c_wasting = case_when(hw72 >= 9996 ~ NA_real_,
                               is.na(hw72) ~ NA_real_,
                               hw72 < -200 ~ 1,
                               TRUE ~ 0),
         c_severewasting = case_when(hw72 >= 9996 ~ NA_real_,
                                     is.na(hw72) ~ NA_real_,
                                     hw72 < -300 ~ 1,
                                     TRUE ~ 0),
         c_overweight = case_when(hw72 >= 9996 ~ NA_real_,
                                  is.na(hw72) ~ NA_real_,
                                  hw72 > 200 ~ 1,
                                  TRUE ~ 0),
         
         c_haz = case_when(hw70 >= 9996 ~ NA_real_,
                           is.na(hw70) ~ NA_real_,
                           TRUE ~ hw70/100),
         c_waz = case_when(hw71 >= 9996 ~ NA_real_,
                           is.na(hw71) ~ NA_real_,
                           TRUE ~ hw71/100),
         c_whz = case_when(hw72 >= 9996 ~ NA_real_,
                           is.na(hw72) ~ NA_real_,
                           TRUE ~ hw72/100),
         c_bmiz = case_when(hw73 >= 9996 ~ NA_real_,
                            is.na(hw73) ~ NA_real_,
                            TRUE ~ hw73/100)
         
  ) %>% 
  
  # # HB
  # mutate_at(vars(v456,hb56), function(x) case_when(x >= 250 | x < 3 ~ NA_real_,
  #                                                  TRUE ~ as.numeric(x))) %>% 
  # BMI
  mutate(v445 = case_when(v454 == 1 ~ NA_real_,
                          v445 > 6000 ~ NA_real_,
                          TRUE ~ v445/100),
         m_weightstatus = case_when(v445 < 18.5 ~ "Underweight",
                                    v445 < 25 ~ "Normal",
                                    v445 >= 25 ~ "Overweight or Obese",
                                    TRUE ~ NA_character_)) %>% 
  # # Glucose
  # mutate_at(vars(smb70,sb70), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
  #                                                   TRUE ~ as.numeric(x))) %>% 
  # Caste
  mutate_at(vars(s116),function(x) case_when(x == 1 ~ "Schedule Caste",
                                             x == 2 ~ "Schedule Tribe",
                                             x == 3 ~ "OBC",
                                             x == 4 ~ "General",
                                             x == 8 ~ "General",
                                             # CASTE IMPUTATION -----------
                                             TRUE ~ "General")) %>% 
  # Education
  mutate_at(vars(v106),function(x) case_when(x == 0 ~ "No education",
                                             x == 1 ~ "Primary",
                                             x == 2 ~ "Secondary",
                                             x == 3 ~ "Higher",
                                             x == 9 ~ NA_character_,
                                             TRUE ~ NA_character_)) %>% 
  # Religion
  mutate_at(vars(v130),function(x) case_when(x == 1 ~ "Hindu",
                                             x == 2 ~ "Muslim",
                                             TRUE ~ "Other")) %>% 
  
  # alcohol
  mutate_at(vars(s716), function(x) case_when(x == 0 ~ 0,
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
    v025 = case_when(v025 == 1 ~ 0,
                     v025 == 2 ~ 1,
                     TRUE ~ NA_real_),
    
    m_employment = case_when(v714 == 1 ~ 2,
                             v714 == 0 ~ 1,
                             TRUE ~ 0)
  ) %>% 
  mutate(m_employment = factor(m_employment,levels=c("no","former","current"))) %>% 
  dplyr::rename(m_bmi = v445,
                m_caste = s116,
                m_education = v106,
                m_religion = v130,
                m_rural = v025,
                m_wealthq = v190,
                m_age = v012,
                m_alcohol = s716,
                m_smoking = v463z) %>% 
  dplyr::select(v000:v007,sampleweight,v021,v023,v024,sdist,phase,
                starts_with("c_"),starts_with("m_"))


# Date ranges of birth dates in NFHS-5 ------

nfhs5 %>% 
  group_by(phase) %>% 
  dplyr::summarise(min = min(c_dob),
                   max = max(c_dob))


# Merge with overlaps.RDS ---------
overlaps <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(-ends_with("_d")) %>% 
  mutate_at(vars(starts_with("e")),.f = list(d = function(x) case_when(x > 90 ~ 1,
                                                                       TRUE ~ 0)))  %>% 
  mutate(sum_e1 = rowSums(.[,c("e1_p1","e1_p2","e1_p3","e1_p4","e1_p5")]),
         sum_e2 = rowSums(.[,c("e2_p1","e2_p2","e2_p3","e2_p4","e2_p5")])
  ) %>% 
  mutate(sum_e1_d = case_when(sum_e1 > 90 ~ 1,
                              TRUE ~ 0),
         sum_e2_d = case_when(sum_e2 > 90 ~ 1,
                              TRUE ~ 0)
  )

# Count 
overlaps %>% 
  right_join(nfhs5,
             by=c("dates"="c_dob")) %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz)) %>% 
  dplyr::select(dates,phase,ends_with("_d")) %>%
  pivot_longer(cols=-one_of("dates","phase"),names_to="exp_period",values_to="value") %>% 
  dplyr::filter(value!=0) %>% 
  group_by(exp_period,phase) %>% 
  dplyr::summarize(n = sum(value)) %>% 
  pivot_wider(names_from = phase,values_from=n)





nfhs5_exposure <- nfhs5 %>% 
  left_join(overlaps %>% 
              dplyr::select(dates,matches("^(e1|e2)"),matches("^(sum)")),
            by=c("c_dob"="dates"))

saveRDS(nfhs5_exposure,paste0(path_lockdown_folder,"/working/nfhs5_exposure.RDS"))




