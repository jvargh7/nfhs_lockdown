require(haven)
require(tidyverse)
require(lubridate)
v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison")

iahr7a_variables <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="growth") %>% 
  rename("selected" = iahr7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

iahr7a <- read_dta(paste0(path_dhs_data,"/IA/IAHR7CDT/IAHR7CFL.dta"),col_select = iahr7a_variables$selected) %>% 
  rename_with(~ iahr7a_variables$new_var[which(iahr7a_variables$selected == .x)], 
              .cols = iahr7a_variables$selected)


bpl_estimates <- iahr7a %>% 
  group_by(v001) %>% 
  mutate(hh_bpl_card = case_when(hh_bpl_card == 8 ~ 0,
                                 TRUE ~ as.numeric(hh_bpl_card))) %>% 
  summarize(bpl_card = mean(hh_bpl_card,na.rm=TRUE),
            n = n(),
            nonna = sum(!is.na(hh_bpl_card))) %>% 
  mutate(bpl = case_when(bpl_card > 0 ~ 1,
                         TRUE ~ 0))

table(bpl_estimates$bpl)/nrow(bpl_estimates)

icds_estimates <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5 child.RDS")) %>% 
  dplyr::filter(c_age > 24) %>% 
  group_by(v001) %>% 
  summarize_at(vars(icds_child_thrfreq,icds_preg_thrfreq,icds_bf_thrfreq,icds_child_visitfreq),.funs=list(m = ~mean(.,na.rm=TRUE),
                                                                                                                  n = ~sum(!is.na(.)))) %>% 
  mutate(icds = case_when(icds_child_thrfreq_m > 0 ~ 1,
                          icds_preg_thrfreq_m > 0 ~ 1,
                          icds_bf_thrfreq_m > 0 ~ 1,
                          icds_child_visitfreq_m > 0 ~ 1,
                          TRUE ~ 0))

table(icds_estimates$icds)  

bpl_estimates %>% 
  left_join(icds_estimates,
            by=c("v001")) %>% 
  
  saveRDS(.,file=paste0(path_lockdown_folder,"/working/nfhs5 cluster covariates.RDS"))
