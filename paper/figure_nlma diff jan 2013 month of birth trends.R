
overlap_predictions <- bind_rows(
  read_csv("main analysis/nlma03u_prediction for unique months and year for urban.csv") %>% mutate(m_rural = "Urban"),
  read_csv("main analysis/nlma03r_prediction for unique months and year for rural.csv") %>% mutate(m_rural = "Rural")) %>% 
  dplyr::filter(dates > "2012-11-30")


jan13_predictions <- overlap_predictions %>% 
  dplyr::filter(dates == "2013-01-01") %>% 
  dplyr::select(m_rural,starts_with("mean")) %>% 
  rename_at(vars(starts_with("mean")),~str_replace(.,"mean","jan2013"))

overlap_predictions2 <- overlap_predictions %>% 
  left_join(jan13_predictions,
            by = "m_rural") %>% 
  mutate(diff_pred_e_stunting = mean_pred_e_stunting - jan2013_pred_e_stunting,
         diff_pred_e_underweight = mean_pred_e_underweight - jan2013_pred_e_underweight,
         diff_pred_e_wasting = mean_pred_e_wasting - jan2013_pred_e_wasting)

require(lubridate)
limits_axis = c(-5,5)

figA <- overlap_predictions2 %>% 
  mutate(est = diff_pred_e_stunting*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,group = m_rural,col=m_rural))  +
  # geom_path() +
  geom_smooth(se = FALSE,method = "gam",linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +

  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence Difference (%)") +
  scale_y_continuous(limits=limits_axis) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle=45)) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA) +
  geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA)

figB <- overlap_predictions2 %>% 
  mutate(est = diff_pred_e_underweight*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,group = m_rural,col=m_rural))  +
  # geom_path() +
  geom_smooth(se = FALSE,method = "gam",linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +
  
  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence Difference (%)") +
  scale_y_continuous(limits=limits_axis) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle=45)) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA) +
  geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA)

figC <- overlap_predictions2 %>% 
  mutate(est = diff_pred_e_wasting*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,group = m_rural,col=m_rural))  +
  # geom_path() +
  geom_smooth(se = FALSE,method = "gam",linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +
  
  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence Difference (%)") +
  scale_y_continuous(limits=limits_axis) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle=45)) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA) +
  geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA)


require(ggpubr)

ggarrange(figA,
          figB,
          figC,
          labels=c("A","B","C"),nrow=3,ncol=1,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/nlma_diff 2013 month of birth trends.png"),width=8,height = 10)

