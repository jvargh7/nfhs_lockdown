overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

overlaps_months <- readRDS("data/overlaps.RDS")  %>% 
  dplyr::select(dates,ends_with("_d")) %>% 
  mutate(month = month(dates),
         year = year(dates)) %>% 
  distinct_at(vars(month,year,ends_with("_d")),.keep_all = TRUE) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) 


set.seed(2022)
sdistri_nfhs5 <- readRDS("data/sdist_nfhs4.RDS") %>% 
  group_by(sdistri) %>% 
  summarize(sdist_list = list(sdist))

nfhs4_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs4_exposure.RDS"))   %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  left_join(readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison") %>% 
              dplyr::select(v024_nfhs5,v024_nfhs4),
            by=c("v024" = "v024_nfhs4"))  %>% 
  mutate(sdistri = as.character(sdistri)) %>% 
  left_join(sdistri_nfhs5,
            by = "sdistri") %>% 
  mutate(sdist = lapply(.$sdist_list,function(x) ifelse(length(x) == 1,x, sample(x,1))) %>% as.numeric(),
         sdistri = as.numeric(sdistri)) %>% 
  dplyr::select(-v024) %>% 
  mutate(v024_nfhs5 = case_when(sdistri %in% c(3,4) ~ 37,
                                TRUE ~ v024_nfhs5)) %>% 
  dplyr::select(-sdist_list)


# nfhs5 --------
sdist_nfhs4 <- readRDS("data/sdist_nfhs4.RDS") %>% 
  group_by(sdist) %>% 
  summarize(sdistri_list = list(sdistri))

nfhs5_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5_exposure.RDS"))  %>% 
  mutate(m_wealthq = factor(m_wealthq,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest")),
         m_alcohol = case_when(is.na(m_alcohol) ~ 0,
                               TRUE ~ m_alcohol)) %>% 
  left_join(sdist_nfhs4,
            by = "sdist") %>% 
  mutate(sdistri = lapply(.$sdistri_list,function(x) ifelse(length(x) == 1,x, sample(x,1))) %>% as.numeric(),
         sdist = as.numeric(sdist)) %>% 
  rename(
    v024_nfhs5 = v024) %>% 
  dplyr::select(-sdistri_list)


nlsd_analytic_sample <- bind_rows(
  nfhs4_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs4_exposure)/(nrow(nfhs4_exposure) + nrow(nfhs5_exposure))),
           nfhs5 = 0),
  nfhs5_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs5_exposure)/(nrow(nfhs5_exposure) + nrow(nfhs4_exposure))),
           nfhs5 = 1)) %>% 
  dplyr::filter(!is.na(c_age)) %>% 
  mutate(age_categories = cut(c_age,breaks=seq(0,60,by=3),right=FALSE,include.lowest=TRUE)) %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz) & !is.na(m_height)) %>% 
  dplyr::filter(phase == 1 | is.na(phase))  %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(e_interaction = case_when(phase == 1 & e_interaction > 10 ~ as.integer(1),
                                   is.na(phase) & e_interaction > 1 ~ as.integer(1),
                                   TRUE ~ e_interaction))
saveRDS(nlsd_analytic_sample,paste0(path_lockdown_folder,"/working/nlsd/nlsd_analytic_sample.RDS"))
rm(nfhs4_exposure,nfhs5_exposure)