analytic_sample = readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  # Per 10pp change
  mutate_at(vars(ends_with("estimate")),~ case_when(is.na(.) ~ 0,
                                                    TRUE ~ .*-(1))) %>% 
  # Per 30day change
  mutate_at(vars(ends_with("gt20")),~case_when(is.na(.) ~ 0,
                                               TRUE ~ .))

analytic_sample %>% 
  # dplyr::filter(phase == 1) %>%
  sample_n(size = 4000) %>% 
  dplyr::select(exposure_estimate,exposure_gt20,c_age,c_haz) %>% 
  pairs()
