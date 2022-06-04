require(lubridate)

overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  # mutate(phase = factor(phase,levels=c(0:2),labels=c("NFHS-4","NFHS-5 Pre-lockdown","NFHS-5 Post-lockdown")))
  mutate(phase = factor(phase,levels=c(0:2),labels=c("2015-16","2019-20","2020-21"))) %>% 
  mutate(dob_x = paste0("15-",month(c_dob),"-",year(c_dob)) %>% dmy(.),
         data_y = paste0("15-",month(c_interview),"-",year(c_interview)) %>% dmy(.)) %>% 
  mutate(term2 = factor(e_interaction,levels=c(1:19),labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.)) %>% 
  mutate(term3 = factor(term2,levels=paste0("Table Order ",sprintf("%02d",c(1:19))) %>% as.character(.),
                        labels =c("None-None",
                                  "D Gestation-None","D Gestation and 0/6mo","D 0/6mo and None","D 0/6mo and 7/12mo",
                                  "D 7/12mo and None","D 7/12mo and 13/18mo","D 13/18mo and None","D 13/18mo and 19/24mo",
                                  "D 19/24mo and None",
                                  "L Gestation-None","L Gestation and 0/6mo","L 0/6mo and None","L 0/6mo and 7/12mo",
                                  "L 7/12mo and None","L 7/12mo and 13/18mo","L 13/18mo and None","L 13/18mo and 19/24mo",
                                  "L 19/24mo and None"),ordered = TRUE))


# 1. Number exposed in each bin by Rural Urban

exposed_bins <- analytic_sample %>% 
  group_by(m_rural,phase, e_interaction,term2,term3) %>% 
  tally() %>% 
  mutate(m_rural = factor(m_rural,levels=c(0,1),labels=c("Urban","Rural"))) %>% 
  pivot_wider(names_from="m_rural",values_from="n") %>% 
  arrange(term2)

write_csv(exposed_bins,"paper/number exposed in each bin by residence.csv")
# 3. Different figure to explain who is compared to whom

grid_df <- analytic_sample  %>% 
  dplyr::filter(phase %in% c("2019-20","2020-21")) %>% 
  group_by(m_rural,dob_x,e_interaction,term3) %>% 
  tally()  %>% 
  arrange(dob_x)

figA <- grid_df %>% 
  dplyr::filter(m_rural == 0) %>% 
ggplot(data=.,aes(x=dob_x,y=term3,fill=n)) + 
  geom_tile() +
  scale_fill_gradient(name = "N",low="white",high = "darkblue",limits=c(0,600),breaks=c(0,200,400,600)) +
  xlab("Month of Birth") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  ylab("") +
  theme_bw() +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  theme(legend.position = "bottom")

figB <- grid_df %>% 
  dplyr::filter(m_rural == 1) %>% 
  ggplot(data=.,aes(x=dob_x,y=term3,fill=n)) + 
  geom_tile() +
  scale_fill_gradient(name = "N",low="white",high = "darkred",limits=c(0,1800),breaks=seq(0,1800,by=400)) +
  xlab("Month of Birth") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  ylab("") +
  theme_bw() +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  theme(legend.position = "bottom")

require(ggpubr)
ggarrange(figA,figB,
          labels=LETTERS[1:2],
          nrow = 1,ncol=2, legend = "bottom",common.legend = FALSE) %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_grid of counts by dob and exposure window.png"),width = 10,height=8)


