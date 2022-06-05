
overlaps_dates <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(-ends_with("_d")) %>% 
  mutate_at(vars(starts_with("e")),.f = list(d = function(x) case_when(x > 120 ~ 1,
                                                                       TRUE ~ 0))) %>% 
  dplyr::select(dates,ends_with("_d"))
saveRDS(overlaps_dates,paste0(path_lockdown_folder,"/working/nlse/nlse_overlaps_dates.RDS"))

overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(-ends_with("_d")) %>% 
  mutate_at(vars(starts_with("e")),.f = list(d = function(x) case_when(x > 120 ~ 1,
                                                                       TRUE ~ 0))) %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

saveRDS(overlaps_unique,paste0(path_lockdown_folder,"/working//nlse/nlse_overlaps_unique.RDS"))

nlse_analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  dplyr::select(-ends_with("_d")) %>% 
  left_join(overlaps_dates,
            by=c("c_dob" = "dates")) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11]))

saveRDS(nlse_analytic_sample,paste0(path_lockdown_folder,"/working//nlse/nlse_analytic_sample.RDS"))
