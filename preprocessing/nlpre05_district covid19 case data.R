c19i_state_map <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="mapnfhs5_v024") %>% 
  dplyr::select(v024,state_covid19india)

c19i_district_map <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="covid19india") %>% 
  left_join(c19i_state_map,
            by=c("State"="state_covid19india"))

nfhs5d_map <- readxl::read_excel("data/NFHS5 Mapping.xlsx")
nfhs5sdist_map <- readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="mapnfhs5_sdist") %>% 
  dplyr::select(REGCODE,v024) %>% 
  rename(sdist=REGCODE)

nfhs5d_factsheets <- read.csv("https://raw.githubusercontent.com/jvargh7/nfhs5_factsheets/main/data%20for%20analysis/districts.csv",header=TRUE) %>% 
  left_join(nfhs5d_map %>% 
              dplyr::select(nfhs5_factsheet_state,nfhs5_factsheet_district,sdist),
            by=c("state" = "nfhs5_factsheet_state","district"="nfhs5_factsheet_district")) %>% 
  dplyr::filter(str_detect(Indicator,"^(7|8|9|10|14|15|16)\\.")) %>% 
  mutate(variable = paste0("V",str_extract(Indicator,"^[0-9]+\\.") %>% str_replace(.,"\\.",""))) 


nfhs5u_factsheets <- read.csv("https://raw.githubusercontent.com/jvargh7/nfhs5_factsheets/main/data%20for%20analysis/states.csv",header=TRUE) %>% 
  dplyr::filter(state %in% c("Chandigarh","Lakshadweep"),
                str_detect(Indicator,"^(7|8|9|10|14|16|20)\\.")
                ) %>% 
  mutate(variable = paste0("V",str_extract(Indicator,"^[0-9]+\\.") %>% str_replace(.,"\\.",""))) %>% 
  mutate(variable = case_when(variable == "V16" ~ "V15",
                              variable == "V20" ~ "V16",
                              TRUE ~ variable),
         sdist = case_when(state == "Chandigarh" ~ 55,
                           state == "Lakshadweep" ~ 587,
                           TRUE ~ NA_real_)) %>% 
  rename(NFHS5 = Total)


factsheets <- bind_rows(nfhs5d_factsheets,
                        nfhs5u_factsheets)%>% 
  dplyr::select(sdist,variable,NFHS5) %>% 
  pivot_wider(names_from=variable,values_from=NFHS5) %>% 
  left_join(nfhs5sdist_map,
            by=c("sdist"))


dcases <- read_csv(paste0(path_covid19india_data,"/districts.csv")) %>% 
  left_join(c19i_district_map %>% 
              dplyr::select(State,District,sdist_nfhs5,v024),
            by=c("State","District")) %>% 
  dplyr::filter(Date > "2020-03-15") %>% 
  # mutate(var_name = paste0("D",Date)) %>% 
  mutate(var_name = Date) %>% 
  dplyr::select(v024,sdist_nfhs5,var_name,Confirmed) %>% 
  group_by(sdist_nfhs5,var_name) %>% 
  # Varanasi --> Prayagraj; Raigarh in Chhatisgarh and Madhya Pradesh
  summarize(Confirmed = mean(Confirmed,na.rm=TRUE),
            v024 = median(v024)) %>% 
  dplyr::filter(!is.na(sdist_nfhs5)) # %>% 
  # pivot_wider(names_from = var_name,values_from=Confirmed)

scases <- read_csv(paste0(path_covid19india_data,"/states.csv")) %>% 
  left_join(c19i_state_map,
            by = c("State"="state_covid19india")) %>% 
  dplyr::filter(Date > "2020-03-15") %>% 
  # mutate(var_name = paste0("D",Date)) %>% 
  mutate(var_name = Date) %>%
  dplyr::select(v024,var_name,Confirmed) %>% 
  dplyr::filter(!is.na(v024)) #%>%
  # pivot_wider(names_from = var_name,values_from=Confirmed,values_fill = 0) 


district_cases <- factsheets %>% 
  # BIG ASSUMPTION 1 -----------
left_join(scases %>% 
            mutate(Confirmed = case_when(is.na(Confirmed) ~ 0,
                                         TRUE ~ Confirmed)) %>% 
                     dplyr::rename(state_cases = Confirmed),
            "v024"="v024") %>% 
  left_join(dcases %>% rename(district_cases = Confirmed),
            by=c("sdist"="sdist_nfhs5","v024"="v024","var_name")) %>% 
  mutate(district_cases = case_when(state_cases == 0 ~ 0,
                                    TRUE ~ district_cases)) %>% 
  group_by(v024,var_name) %>% 
  mutate(unaccounted_cases = state_cases - sum(district_cases,na.rm=TRUE),
         n_districts = sum(is.na(district_cases))) %>% 
  mutate(district_imputed = case_when(!is.na(district_cases) ~ district_cases,
                                      n_districts == 0 ~ district_cases,
                                      unaccounted_cases == 0 ~ 0,
                                      TRUE ~ unaccounted_cases/n_districts)) %>% 
  ungroup()


saveRDS(district_cases,paste0(path_lockdown_folder,"/working/district_cases.RDS"))
# # https://cran.r-project.org/web/packages/Amelia/vignettes/using-amelia.html -----------
# Doesn't give reliable estimates
# library(Amelia)
# 
# district_imputed <- amelia(district_cases,m=1,p2s=2,idvars=c("v024"),
#                            ts="var_name",cs="sdist",polytime=3,intercs=TRUE,
#                            lags=c("state_cases","district_cases"),leads=c("state_cases","district_cases"))

