analytic_sample <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  mutate(phase = case_when(is.na(phase) ~ 0,
                           TRUE ~ phase)) %>% 
  # mutate(phase = factor(phase,levels=c(0:2),labels=c("NFHS-4","NFHS-5 Pre-lockdown","NFHS-5 Post-lockdown")))
  mutate(phase = factor(phase,levels=c(0:2),labels=c("2015-16","2019-20","2020-2021")))

require(splines)

plotA <- analytic_sample %>% 
  dplyr::filter(m_rural == 0) %>% 
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_stunting*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "") +
  theme(legend.text = element_text(size = 14))

plotB <- analytic_sample %>% 
  dplyr::filter(m_rural == 0) %>% 
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_underweight*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "")+
  theme(legend.text = element_text(size = 14))

plotC <- analytic_sample %>% 
  dplyr::filter(m_rural == 0) %>%  
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_wasting*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "")+
  theme(legend.text = element_text(size = 14))

plotD <- analytic_sample %>% 
  dplyr::filter(m_rural == 1) %>% 
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_stunting*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "") +
  theme(legend.text = element_text(size = 14))

plotE <- analytic_sample %>% 
  dplyr::filter(m_rural == 1) %>% 
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_underweight*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "")+
  theme(legend.text = element_text(size = 14))

plotF <- analytic_sample %>% 
  dplyr::filter(m_rural == 1) %>% 
  mutate(c_male = factor(c_male,levels=c(0,1),labels=c("Girls","Boys"))) %>% 
  ggplot(data=.,aes(x=c_age,y=c_wasting*100,group=interaction(phase,c_male),col=phase)) +
  coord_cartesian(ylim =c(0,50)) +
  geom_smooth(method="lm",aes(linetype=c_male),formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue","green")) +
  scale_linetype_discrete(name = "")+
  theme(legend.text = element_text(size = 14))

require(ggpubr)

ggarrange(ggarrange(plotA,plotB,plotC,
                    labels=LETTERS[1:3],
                    nrow = 1,ncol=3, legend = "bottom",common.legend = TRUE),
          ggarrange(plotD,plotE,plotF,
                    labels=LETTERS[4:6],
                    nrow = 1,ncol=3, legend = "bottom",common.legend = TRUE),
          nrow = 2,ncol=1) %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_sex stratified age trends in prevalence.png"),width = 15,height=7)
