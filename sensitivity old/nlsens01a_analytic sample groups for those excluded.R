nfhs4_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs4_exposure.RDS"))   %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  left_join(readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison") %>% 
              dplyr::select(v024_nfhs5,v024_nfhs4),
            by=c("v024" = "v024_nfhs4")) %>% 
  dplyr::select(-v024) %>% 
  mutate(v024_nfhs5 = case_when(sdistri %in% c(3,4) ~ 37,
                                TRUE ~ v024_nfhs5))


nfhs5_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5_exposure.RDS"))  %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest")),
         m_alcohol = case_when(is.na(m_alcohol) ~ 0,
                               TRUE ~ m_alcohol)) %>% 
  rename(sdistri = sdist,
         v024_nfhs5 = v024)

excluded_sample <- bind_rows(
  nfhs4_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs4_exposure)/(nrow(nfhs4_exposure) + nrow(nfhs5_exposure))),
           nfhs5 = 0),
  nfhs5_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs5_exposure)/(nrow(nfhs5_exposure) + nrow(nfhs4_exposure))),
           nfhs5 = 1)) %>% 
  mutate(group = case_when(nfhs5 == 0 & !(v024_nfhs5 %in% v024_nfhs5_14states) ~ 1,
                           nfhs5 == 1 & phase == 1 & !(v024_nfhs5 %in% v024_nfhs5_14states) ~ 2,
                           nfhs5 == 0 & (is.na(c_haz)|is.na(c_waz)|is.na(c_whz)) ~ 3,
                           nfhs5 == 1 & phase == 1 & (is.na(c_haz)|is.na(c_waz)|is.na(c_whz)) ~ 4,
                           nfhs5 == 1 & phase == 2 & (is.na(c_haz)|is.na(c_waz)|is.na(c_whz)) ~ 5,
                           TRUE ~ NA_real_
                           )) %>% 
  dplyr::filter(!is.na(group)) %>% 
  mutate(group = factor(group,levels=c(1:5),labels=c("S22 NFHS-4",
                                                     "S22 NFHS-5 Pre-lockdown",
                                                     "MIS NFHS-4",
                                                     "MIS NFHS-5 Pre-lockdown",
                                                     "MIS NFHS-5 Post-lockdown")))
  
saveRDS(excluded_sample,paste0(path_lockdown_folder,"/working/excluded_sample.RDS"))
