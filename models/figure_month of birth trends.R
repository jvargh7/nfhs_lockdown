
overlap_predictions <- bind_rows(
  read_csv("paper/nlmb02_prediction for unique months and year for urban.csv") %>% mutate(m_rural = "Urban"),
  read_csv("paper/nlmb03_prediction for unique months and year for rural.csv") %>% mutate(m_rural = "Rural")) %>% 
  dplyr::filter(dates > "2012-11-30")


limits_axis = c(15,35)

figA <- overlap_predictions %>% 
  mutate(est = mean_pred_e_stunting*100,
         lci = (mean_pred_e_stunting - 1.96*se_pred_e_stunting)*100,
         uci = (mean_pred_e_stunting + 1.96*se_pred_e_stunting)*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,ymin = lci,ymax=uci,group = m_rural,col=m_rural))  +
  geom_path() +
  geom_smooth(se = FALSE,linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +
  geom_ribbon(alpha = 0.5,fill="grey80",col=NA) +
  
  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence (%)") +
  scale_y_continuous(limits=limits_axis) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle=45)) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA) +
  geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA)

figB <- overlap_predictions %>% 
  mutate(est = mean_pred_e_underweight*100,
         lci = (mean_pred_e_underweight - 1.96*se_pred_e_underweight)*100,
         uci = (mean_pred_e_underweight + 1.96*se_pred_e_underweight)*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,ymin = lci,ymax=uci,group = m_rural,col=m_rural))  +
  geom_path() +
  geom_smooth(se = FALSE,linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +
  geom_ribbon(alpha = 0.5,fill="grey80",col=NA) +
  
  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence (%)") +
  scale_y_continuous(limits=limits_axis) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle=45)) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  geom_rect(xmin = as_date(demonetization_start),xmax = as_date(demonetization_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA) +
  geom_rect(xmin = as_date(lockdown_start),xmax = as_date(lockdown_stop),ymin = -10,ymax = 60,fill="grey80",alpha=0.01,col=NA)

figC <- overlap_predictions %>% 
  mutate(est = mean_pred_e_wasting*100,
         lci = (mean_pred_e_wasting - 1.96*se_pred_e_wasting)*100,
         uci = (mean_pred_e_wasting + 1.96*se_pred_e_wasting)*100) %>% 
  ggplot(data=.,aes(x=dates,y=est,ymin = lci,ymax=uci,group = m_rural,col=m_rural))  +
  geom_path() +
  geom_smooth(se = FALSE,linetype=2,size=0.8)  +
  scale_color_manual(name="",values=c("darkgreen","purple")) +
  geom_ribbon(alpha = 0.5,fill="grey80",col=NA) +
  
  xlab("Date of Birth") +
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n %Y") +
  ylab("Prevalence (%)") +
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
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_month of birth trends.png"),width=8,height = 10)

