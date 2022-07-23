require(lubridate)
overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

# NLMA, NLSZ --------------
nlma_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlma_counts,("paper/nlma_counts.csv"))

# NLXB -----------------

nlxb_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,c_male == 1) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlxb_counts,("paper/nlxb_counts.csv"))

# NLXG -----------------

nlxg_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,c_male == 0) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlxg_counts,("paper/nlxg_counts.csv"))


# NLSE --------------
nlse_counts <- readRDS(paste0(path_lockdown_folder,"/working/nlse/nlse_analytic_sample.RDS")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:15),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,6:2,15:7)))) %>% as.character(.),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlse_counts,("paper/nlse_counts.csv"))

# NLS5 -------------
nls5_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,nfhs5 == 1) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nls5_counts,("paper/nls5_counts.csv"))

# NLSY -------------------

nlsy_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states,c_age <= 36) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlsy_counts,("paper/nlsy_counts.csv"))


# NLSC ---------------

nlsc_counts <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  mutate(c_conception = c_dob - 38*7) %>%  
  dplyr::filter(c_conception < lockdown_start) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(year_categories = case_when(year(c_dob) %in% c(2019,2020) ~ "2019-20",
                                     year(c_dob) %in% c(2017,2018) ~ "2017-18",
                                     TRUE ~ "2010-15")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:19),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlsc_counts,("paper/nlsc_counts.csv"))

# NLSD -----------------

nlsd_counts <- readRDS(paste0(path_lockdown_folder,"/working/nlsd/nlsd_analytic_sample.RDS")) %>% 
  mutate(term2 = factor(e_interaction,
                        levels = c(1:10),
                        labels=paste0("Table Order ",sprintf("%02d",c(1,10:2)))) %>% as.character(.),
         phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  mutate(row_category = case_when(term2 == "Table Order 01" ~ paste0(term2," - ",phase),
                                  TRUE ~ term2)) %>% 
  
  group_by(row_category,m_rural) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")),
                   n = n())  %>% 
  arrange(m_rural,row_category)

write_csv(nlsd_counts,("paper/nlsd_counts.csv"))
