source("analysis/nla_analytic sample processing.R")

source("C:/code/external/functions/survey/svysummary.R")


z_estimate = svysummary(analytic_svy,
                                 c_vars = c("c_haz","c_waz","c_whz","c_bmiz"),
                                 # p_vars = c("c_stunting","c_underweight","c_wasting"),
                                 # g_vars = g_vars,
                                 id_vars = c("phase","c_age")
) 
write_csv(z_estimate,"paper/table_anthropometric z scores by age and phase.csv")

figA = z_estimate %>% 
  dplyr::filter(variable == "c_haz") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Average z-score") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6)) +
  scale_y_continuous(limits=c(-2.1,0),breaks=seq(-2,0,by=0.5))


figB = z_estimate %>% 
  dplyr::filter(variable == "c_waz") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Average z-score") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6)) +
  scale_y_continuous(limits=c(-2.1,0),breaks=seq(-2,0,by=0.5))

figC = z_estimate %>% 
  dplyr::filter(variable == "c_whz") %>% 
  ggplot(data=.,aes(x=c_age,y=estimate,col=factor(phase),fill=factor(phase),ymin=lci,ymax=uci)) +
  geom_path() +
  geom_ribbon(alpha=0.15,col=NA) + 
  scale_color_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  scale_fill_manual(name = "Phase",values=c("1"="darkgreen","2"="red")) +
  xlab("Age at measurement") +
  ylab("Average z-score") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,60,by=6)) +
  scale_y_continuous(limits=c(-2.1,0),breaks=seq(-2,0,by=0.5))


library(ggpubr)
ggarrange(figA,figB,figC,
          labels=c("A","B","C"),nrow=3,
          common.legend=TRUE,legend="bottom") %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_anthropometric z-scores by age and phase.png"),width=8,height=8)
