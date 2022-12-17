require(haven)
require(tidyverse)
require(lubridate)
source("preprocessing/nlpre_preprocessing.R")

v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison")


iakr74_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth") %>% 
  rename("selected" = iakr74dt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

iahr74_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth") %>% 
  rename("selected" = iahr74dt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))


nfhs4 <- read_dta(paste0(path_dhs_data,"/IA/IAKR74DT/IAKR74FL.dta"),col_select = iakr74_variables$selected) %>% 
  rename_with(~ iakr74_variables$new_var[which(iakr74_variables$selected == .x)], 
              .cols = iakr74_variables$selected) %>% 
  
  left_join(read_dta(paste0(path_dhs_data,"/IA/IAHR74DT/IAHR74FL.dta"),col_select = iahr74_variables$selected) %>% 
              rename_with(~ iahr74_variables$new_var[which(iahr74_variables$selected == .x)], 
                          .cols = iahr74_variables$selected) %>% 
              mutate(hh_wealthqur = case_when(is.na(hh_wealthqu) ~ hh_wealthqr,
                                              TRUE ~ hh_wealthqu),
                     hh_wealthiur = case_when(is.na(hh_wealthiu) ~ hh_wealthir,
                                              TRUE ~ hh_wealthiu)
                     ),
            by=c("v001","v002")) %>% 
  
  nlpre_preprocessing() %>% 
  rename(v024_nfhs4 = v024) %>% 
  left_join(v024_comparison %>% 
              dplyr::select(v024_nfhs4,v024_nfhs5,nfhs5_state,nfhs4_state),
            by = c("v024_nfhs4")) %>% 
  mutate(v024_nfhs5 = case_when(sdist %in% c(3,4) ~ 37,
                                TRUE ~ v024_nfhs5),
         nfhs5_state = case_when(v024_nfhs5 == 37 ~ "Ladakh",
                                 TRUE ~ nfhs5_state))


# Merge with overlaps.RDS ---------

saveRDS(nfhs4,paste0(path_lockdown_folder,"/working/nfhs4 child.RDS"))
