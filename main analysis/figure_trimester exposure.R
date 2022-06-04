overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))


sample_e_eq19 <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  dplyr::filter(m_rural == 0) %>% 
  dplyr::filter(e_interaction %in% c(17,18,19))  %>%
  mutate(e_interaction = factor(e_interaction,levels=c(17,18,19),labels=c("Birth 0-6 + None","Gestation + Birth 0-6",
                                                       "Gestation + None"))) %>% 
  dplyr::mutate(c_conception = c_dob - 38*7)


require(lubridate)
sample_e_eq19  %>% 
  ggplot(data=.,aes(x=c_conception,fill = e_interaction,group = e_interaction)) +
  geom_histogram(alpha = 0.4) +
  xlab("Conception date") +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y",
               limits = c(as_date("2019-01-01"),as_date("2021-04-21")))


sample_e_eq19 %>% 
  ggplot(data=.,aes(x=c_age,fill=e_interaction)) +
  geom_histogram()

sample_e_eq19 %>% 
  dplyr::filter(c_conception < "2020-04-15") %>% 
  ggplot(data=.,aes(x=c_age,fill=e_interaction)) +
  geom_histogram()

sample_e_eq19 %>% 
  mutate(month = factor(c_month,levels=c(1:12),labels=month.abb)) %>% 
  group_by(month,e_interaction) %>% 
  tally() %>% 
  ggplot(data=.,aes(x=month,y = n, fill=e_interaction)) +
  geom_col()

sample_e_eq19 %>% 
  mutate(month = factor(c_month,levels=c(1:12),labels=month.abb)) %>% 
  group_by(month,e_interaction,c_age) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(month,c_age) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(data=.,aes(x=factor(c_age),y = prop,fill=e_interaction)) +
  geom_col() +
  xlab("Age at data collection") +
  facet_grid(~month) +
  ylab ("Proportion within each month from cohorts")
