grid_df <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  # mutate(phase = factor(phase,levels=c(0:2),labels=c("NFHS-4","NFHS-5 Pre-lockdown","NFHS-5 Post-lockdown")))
  mutate(phase = factor(phase,levels=c(0:2),labels=c("2015-16","2019-20","2020-2021"))) %>% 
  mutate(dob_x = paste0("15-",month(c_dob),"-",year(c_dob)) %>% dmy(.),
         data_y = paste0("15-",month(c_interview),"-",year(c_interview)) %>% dmy(.)) %>% 
  group_by(m_rural,dob_x,c_age) %>% 
  dplyr::summarize (stunting = Hmisc::wtd.mean(c_stunting,weights = combined_sampleweight),
                    underweight = Hmisc::wtd.mean(c_underweight,weights = combined_sampleweight),
                    wasting = Hmisc::wtd.mean(c_wasting,weights = combined_sampleweight))


figA <- grid_df %>% 
  dplyr:::filter(m_rural == 0) %>% 
  ggplot(data=.,aes(x=dob_x,y=c_age,fill=stunting*100)) +
  geom_tile() +
  scale_fill_gradient2(name = "",low="white",mid = "yellow",high = "red") +
  xlab("Month of Birth") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  ylab("Age at data collection (in months)") +
  theme_bw() +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) 
  # geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -5,ymax = 65, fill="grey80",alpha=0.01,col=NA) +
  # geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -5,ymax = 65, fill="grey80",alpha=0.01,col=NA)

figB <- grid_df %>% 
  dplyr:::filter(m_rural == 1) %>% 
  ggplot(data=.,aes(x=dob_x,y=c_age,fill=stunting*100)) +
  geom_tile() +
  scale_fill_gradient2(name = "",low="white",mid = "yellow",high = "red") +
  xlab("Month of Birth") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  ylab("Age at data collection (in months)") +
  theme_bw() +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) 
  # geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -5,ymax = 65, fill="grey80",alpha=0.01,col=NA) +
  # geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -5,ymax = 65, fill="grey80",alpha=0.01,col=NA)



ggarrange(figA,figB,
          labels=LETTERS[1:2],
          nrow = 2,ncol=1, legend = "bottom",common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_grid of dob and age at data collection for prevalence.png"),width = 10,height=10)
