reference_hospitalization <- read_csv(paste0(path_lockdown_folder,"/working/ihme-covid19/2021-09-02/reference_hospitalization_all_locs.csv"))

unique_location_id <- reference_hospitalization %>% 
  dplyr::distinct(location_id,location_name) %>% 
  write_csv(.,"data/unique_location_id in ihme-covid19.csv")

india_cmi <- reference_hospitalization %>% 
  dplyr::filter(location_id %in% c(163,4841:4875)) %>% 
  dplyr::select(date,location_id,location_name,deaths_mean_smoothed,
                mobility_data_type,
                mobility_composite,total_tests,est_infections_mean,
                total_pop) 
View(india_cmi %>% group_by(location_name) %>% tally())

v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison")
# Step 1: Fill for those available
# Impute with previous day's value if missing
# Impute with zero if unavailable
india_cmi_imputed_step1 <- map_dfr(unique(india_cmi$location_id),
                             function(l_i){
                               orig = india_cmi %>% 
                                 dplyr::filter(location_id == l_i) ;
                               
                               orig %>% 
                                 complete(date = seq.Date(ymd("2013-07-01"), ymd("2021-09-01"), by = "day")) %>% 
                                 mutate(location_name = unique(orig$location_name),
                                        location_id = l_i,
                                        mobility_data_type = case_when(is.na(mobility_composite) ~ "Imputed Step 1",
                                                                       TRUE ~ mobility_data_type)) %>% 
                                 arrange(date) %>% 
                                 mutate_at(vars(mobility_composite),~zoo::na.locf(.,na.rm=FALSE)) %>% 
                                 mutate_at(vars(mobility_composite),function(x) case_when(is.na(x) ~ 0,
                                                                                          TRUE ~ x)) %>% 
                                 # This would duplicate rows for location_id = 4854
                                 # Jammu & Kashmir and Ladakh
                                 right_join(
                                   v024_comparison %>% 
                                     dplyr::select(v024_nfhs5,unique_location_id) %>% 
                                     rename(v024 = v024_nfhs5),
                                   by = c("location_id" = "unique_location_id")
                                   
                                 ) %>% 
                                 dplyr::filter(!is.na(date)) %>% 
                                 return(.)
                               
                             })

# Step 2: Fill for Chandigarh since it is straightforward
# Fill for Puducherry districts separately
chandigarh_cmi <- india_cmi_imputed_step1 %>% 
  dplyr::filter(location_id %in% c(4867,4852)) %>% 
  group_by(date) %>% 
  summarize(mobility_composite = mean(mobility_composite,na.rm=TRUE)) %>% 
  mutate(location_id = 48674852,
         v024 = 4,
         sdist = 55,
         location_name = "Chandigarh",
         mobility_data_type = "Imputed Step 2")

puducherry_cmi <- bind_rows(india_cmi_imputed_step1 %>% 
                              dplyr::filter(location_id %in% c(4870)) %>% 
                              group_by(date) %>% 
                              summarize(mobility_composite = mean(mobility_composite,na.rm=TRUE)) %>% 
                              mutate(location_id = 487001,
                                     v024 = 34,
                                     sdist = 635,
                                     location_name = "Puducherry Puducherry",
                                     mobility_data_type = "Imputed Step 2"),
                            
                            india_cmi_imputed_step1 %>% 
                              dplyr::filter(location_id %in% c(4870)) %>% 
                              group_by(date) %>% 
                              summarize(mobility_composite = mean(mobility_composite,na.rm=TRUE)) %>% 
                              mutate(location_id = 487002,
                                     v024 = 34,
                                     sdist = 637,
                                     location_name = "Puducherry Karaikal",
                                     mobility_data_type = "Imputed Step 2"),
                            
                            india_cmi_imputed_step1 %>% 
                              dplyr::filter(location_id %in% c(4841)) %>% 
                              group_by(date) %>% 
                              summarize(mobility_composite = mean(mobility_composite,na.rm=TRUE)) %>% 
                              mutate(location_id = 484101,
                                     v024 = 34,
                                     sdist = 634,
                                     location_name = "Puducherry Yanam",
                                     mobility_data_type = "Imputed Step 2"),
                            
                            india_cmi_imputed_step1 %>% 
                              dplyr::filter(location_id %in% c(4857)) %>% 
                              group_by(date) %>% 
                              summarize(mobility_composite = mean(mobility_composite,na.rm=TRUE)) %>% 
                              mutate(location_id = 485701,
                                     v024 = 34,
                                     sdist = 636,
                                     location_name = "Puducherry Mahe",
                                     mobility_data_type = "Imputed Step 2")
                            
                            )

india_cmi_imputed_step2 = bind_rows(india_cmi_imputed_step1,
                                    
                                    chandigarh_cmi,
                                    puducherry_cmi
                                    ) %>% 
  mutate(mobility_composite = case_when(mobility_composite > 0 ~ 0,
                                        TRUE ~ mobility_composite))
  

View(india_cmi_imputed_step2 %>% group_by(location_id,location_name) %>% tally())
View(india_cmi_imputed_step2 %>% group_by(date) %>% tally())


# Issues with Nagaland but shouldn't affect our analysis since the state is not included in the Lockdown paper
india_cmi_imputed_step2 %>% 
  dplyr::filter(location_name %in% c("Nagaland"),date >= "2020-03-01",date<="2020-05-01") %>% View()

saveRDS(india_cmi_imputed_step2,paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS"))
