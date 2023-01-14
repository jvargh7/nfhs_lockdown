source("analysis/nla_analytic sample processing.R")
rm(analytic_svy,analytic2_svy)

district_characteristics <- readRDS(paste0(path_lockdown_folder,"/working/district_cases.RDS")) %>% 
  distinct(sdist,v024,V4,V7,V8,V9,V10,V12,V14,V15,V16) %>% 
  group_by(v024) %>% 
  mutate(V4 = case_when(is.na(V4) ~ median(V4,na.rm=TRUE),
                        TRUE ~ V4)) %>% 
  ungroup() %>% 
  dplyr::select(-v024)

pca_obj <- district_characteristics %>%
  dplyr::select(-sdist) %>% 
  prcomp(.,scale. = TRUE)

district_characteristics$pc1 = pca_obj$x[,1]

may_zones <- readxl::read_excel("data/NFHS Lockdown Zones.xlsx",sheet="May 4 to 17 2020") %>% 
  dplyr::select(sdist_nfhs5,Zone)
# Varible 'b4': 1: Male, 2: Female

district_svy <- analytic_sample %>% 
  left_join(may_zones,
            by=c("sdist"="sdist_nfhs5")) %>% 
  left_join(district_characteristics,
            by=c("sdist"="sdist")) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")

district2_svy <- analytic_sample %>% 
  left_join(may_zones,
            by=c("sdist"="sdist_nfhs5")) %>% 
  left_join(district_characteristics,
            by=c("sdist"="sdist")) %>%
  dplyr::filter(age_le24 ==1 | age_ge36 == 1) %>% 
  as_survey_design(ids = v021,strata=v023,weights=sampleweight,variance ="YG",pps="brewer")
