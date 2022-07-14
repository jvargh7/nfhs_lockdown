overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

nlma_analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction)) %>% 
  mutate(e2_p1_d = case_when(phase == 1 & e2_p1_d == 1 ~ 0,
                             TRUE ~ e2_p1_d),
         e2_p2_d = case_when(phase == 1 & e2_p2_d == 1 ~ 0,
                             TRUE ~ e2_p2_d),
         e2_p3_d = case_when(phase == 1 & e2_p3_d == 1 ~ 0,
                             TRUE ~ e2_p3_d),
         e2_p4_d = case_when(phase == 1 & e2_p4_d == 1 ~ 0,
                             TRUE ~ e2_p4_d),
         e2_p5_d = case_when(phase == 1 & e2_p5_d == 1 ~ 0,
                             TRUE ~ e2_p5_d)
         )

overlaps_months <- readRDS("data/overlaps.RDS")  %>% 
  dplyr::select(dates,ends_with("_d")) %>% 
  mutate(month = month(dates),
         year = year(dates)) %>% 
  distinct_at(vars(month,year,ends_with("_d")),.keep_all = TRUE) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11]))
