require(haven)
require(tidyverse)
require(lubridate)

source("preprocessing/nlpre_preprocessing.R")
v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison")


iakr7a_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth") %>% 
  rename("selected" = iakr7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))


nfhs5 <- read_dta(paste0(path_dhs_data,"/IA/IAKR7CDT/IAKR7CFL.dta"),col_select = iakr7a_variables$selected) %>% 
  rename_with(~ iakr7a_variables$new_var[which(iakr7a_variables$selected == .x)], 
              .cols = iakr7a_variables$selected) %>% 
  nlpre_preprocessing() %>% 
  rename(v024_nfhs5 = v024) %>% 
  left_join(v024_comparison %>% 
  dplyr::select(v024_nfhs5,nfhs5_state),
  by = c("v024_nfhs5"))
  
  


# Date ranges of birth dates in NFHS-5 ------

nfhs5 %>% 
  group_by(phase) %>% 
  dplyr::summarise(min = min(c_dob),
                   max = max(c_dob))


# Merge with overlaps.RDS ---------


saveRDS(nfhs5,paste0(path_lockdown_folder,"/working/nfhs5 child.RDS"))




