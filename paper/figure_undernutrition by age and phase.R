source("analysis/nla_analytic sample processing.R")

source("C:/code/external/functions/survey/svysummary.R")


prevalence_estimate = svysummary(analytic_svy,
                        # c_vars = c("c_haz","c_waz","c_whz","c_bmiz"),
                        p_vars = c("c_stunting","c_underweight","c_wasting","c_overweight"),
                        # g_vars = g_vars,
                        id_vars = c("phase","c_age")
) 
write_csv(prevalence_estimate,"paper/table_undernutrition by age and phase.csv")

figA = prevalence_estimate %>% 
  dplyr::filter(variable == "c_stunting") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Prevalence (%)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6)) +
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10))


figB = prevalence_estimate %>% 
  dplyr::filter(variable == "c_underweight") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Prevalence (%)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6))+
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10))

figC = prevalence_estimate %>% 
  dplyr::filter(variable == "c_wasting") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Prevalence (%)") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6))+
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10))


library(ggpubr)
ggarrange(figA,figB,figC,
          labels=c("A","B","C"),nrow=3,
          common.legend=TRUE,legend="bottom") %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_undernutrition by age and phase.png"),width=8,height=8)

