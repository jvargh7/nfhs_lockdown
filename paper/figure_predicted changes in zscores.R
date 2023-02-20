pred_z = readRDS(paste0("analysis/nlg02_predicted values of zscores.RDS"))

figA = pred_z %>% 
  dplyr::filter(outcome == "HAZ") %>% 
  ggplot(data=.,aes(x=group,
                    ymin = lci,
                    ymax = uci,
                    y = mean,
                    shape = region,
                    linetype = region,
                    col = exposure_type)) +
  geom_point(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab("Age at measurement") +
  ylab("HAZ") +
  # scale_y_continuous(limits=c(-2,-0.4)) +
  scale_shape_discrete(name = "") +
  scale_color_discrete(name = "") +
  scale_linetype_discrete(name = "")

figB = pred_z %>% 
  dplyr::filter(outcome == "WAZ") %>% 
  ggplot(data=.,aes(x=group,
                    ymin = lci,
                    ymax = uci,
                    y = mean,
                    shape = region,
                    linetype = region,
                    col = exposure_type)) +
  geom_point(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab("Age at measurement") +
  ylab("WAZ") +
  # scale_y_continuous(limits=c(-2,-0.4)) +
  scale_shape_discrete(name = "") +
  scale_color_discrete(name = "") +
  scale_linetype_discrete(name = "")

figC = pred_z %>% 
  dplyr::filter(outcome == "WHZ") %>% 
  ggplot(data=.,aes(x=group,
                    ymin = lci,
                    ymax = uci,
                    y = mean,
                    shape = region,
                    linetype = region,
                    col = exposure_type)) +
  geom_point(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab("Age at measurement") +
  ylab("WHZ") +
  # scale_y_continuous(limits=c(-2,-0.4))  +
  scale_shape_discrete(name = "") +
  scale_color_discrete(name = "") +
  scale_linetype_discrete(name = "")


library(ggpubr)
ggarrange(figA,
          figB,
          figC,
          nrow = 1,ncol=3,
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_predicted changes in zscores.png"),width=10,height=4)
