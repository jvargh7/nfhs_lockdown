require(lubridate)

iakr74_df <- haven::read_dta(paste0(path_dhs_data,"/IA/IAKR74DT/IAKR74FL.dta"),
                                                      col_select = iakr_variables$iakr74dt) %>%
  dplyr::select(starts_with("b"),hw16) %>%
  mutate(hw16 = case_when(is.na(hw16) | hw16 == 98 ~ 15,
                          b1 == 2 & b2 %in% c(2008,2012,2016,2020) & hw16 > 29 ~ 29,
                          b1 == 2 & hw16 > 28 ~ 28,
                          b1 %in% c(4,6,9,11) & hw16 > 30 ~ 30,
                          TRUE ~ as.numeric(hw16))) %>%
  mutate(dob = as_date(paste0(b2,"-",b1,"-",hw16)))

# iakr74_df %>%
#   summarize(min = min(dob),
#             max = max(dob))

# # A tibble: 1 x 2
# min        max       
# <date>     <date>    
#   1 2010-02-02 2016-11-19

overlaps <- readRDS("data/overlaps.RDS")

# How many unique combinations of exposure with > 100 days = 10
# overlaps <- readRDS(paste0("data/overlaps.RDS"))
# overlaps %>%
#   dplyr::select(dates,ends_with("_d")) %>%
#   dplyr::filter(rowSums(.[,-1])!=0) %>%
#   group_by_at(vars(ends_with("_d"))) %>%
#   summarize(n = n(),
#             min = min(dates),
#             max = max(dates)) %>%
#   ungroup() %>% 
#   dplyr::filter(n > 100) %>%
#   View()


dates_x_survey_count <- overlaps %>% 
  right_join(iakr74_df,
             by=c("dates"="dob")) %>% 
  dplyr::select(dates,ends_with("_d")) %>%
  pivot_longer(cols=-dates,names_to="exp_period",values_to="value") %>% 
  dplyr::filter(value!=0) %>% 
  group_by(exp_period) %>% 
  dplyr::summarize(n = sum(value))


dates_x_survey_range <- overlaps %>% 
  dplyr::select(dates,ends_with("_d")) %>%
  pivot_longer(cols=-dates,names_to="exp_period",values_to="value") %>% 
  dplyr::filter(value!=0) %>% 
  group_by(exp_period) %>% 
  summarize(min = min(dates),
            max = max(dates)) %>% 
  mutate_at(vars(min,max),function(x) format(x,"%d-%m-%Y")) %>% 
  mutate(range = paste0(min," to ",max)) 


left_join(dates_x_survey_range,
          dates_x_survey_count,
          by="exp_period")%>% 
  write_csv(.,paste0(path_lockdown_folder,"/working/table_exposure x period.csv"))

