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






analytic_sample <- bind_rows(
  nfhs4_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs4_exposure)/(nrow(nfhs4_exposure) + nrow(nfhs5_exposure))),
           nfhs5 = 0),
  nfhs5_exposure %>% 
    mutate(combined_sampleweight = sampleweight*(nrow(nfhs5_exposure)/(nrow(nfhs5_exposure) + nrow(nfhs4_exposure))),
           nfhs5 = 1)) %>% 
  dplyr::filter(!is.na(c_age)) %>% 
  mutate(age_categories = cut(c_age,breaks=seq(0,60,by=3),right=FALSE,include.lowest=TRUE))


# Step 2 ---- Restrict to 14 states --------
analytic_sample_s2 <- analytic_sample  %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

table(analytic_sample_s2$phase,useNA="always")

# Step 3 -----Restrict to non-NA values --------
analytic_sample_s3 <- analytic_sample_s2 %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz))

table(analytic_sample_s3$phase,useNA="always")

# View(analytic_sample_s3 %>% mutate(day_flag = case_when(is.na(c_day_orig) | c_day_orig == 98 ~ -1,
#                                                         month(c_dob) == 2 & year(c_dob) %in% c(2008,2012,2016,2020) & c_day_orig > 29 ~ 1,
#                                                         month(c_dob) == 2 & c_day_orig > 28 ~ 1,
#                                                         month(c_dob) %in% c(4,6,9,11) & c_day_orig > 30 ~ 1,
#                                                         TRUE ~ 0)) %>% group_by(day_flag) %>% tally())

saveRDS(analytic_sample_s3,paste0(path_lockdown_folder,"/working/analytic_sample.RDS"))


