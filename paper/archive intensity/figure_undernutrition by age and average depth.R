source("analysis/nla_analytic sample processing.R")

source("C:/code/external/functions/survey/svysummary.R")

prevalence_estimate = svysummary(analytic_svy,
                                 # c_vars = c_vars,
                                 p_vars = c("c_stunting","c_underweight","c_wasting"),
                                 # g_vars = g_vars,
                                 id_vars = c("phase","c_age3mo","exposure_estimate_categories")
) 

prevalence_ct <- analytic_sample %>% 
  group_by_at(vars(one_of(c("phase","c_age3mo","exposure_estimate_categories")))) %>%
  summarize_at(vars(one_of(c(
    # c_vars,
     c("c_stunting","c_underweight","c_wasting")
    # g_vars = g_vars,
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",
               cols=-one_of(c("phase","c_age3mo","exposure_estimate_categories"))
  ) %>% 
  mutate(variable = str_replace(variable,"_n$",""));

prevalence_df = left_join(prevalence_estimate,
          prevalence_ct,
          by=c(
            c("phase","c_age3mo","exposure_estimate_categories"),
            "variable"))

x_axis_breaks = c("[0,3)","[9,12)","[18,21)","[27,30)","[36,39)","[45,48)","[54,57)")

figA = prevalence_df %>% 
  dplyr::filter(variable == "c_stunting") %>% 
  ggplot(data=.,aes(x=c_age3mo,y=exposure_estimate_categories,fill=estimate)) +
  geom_tile() +
  scale_fill_gradient2(low="white",mid="yellow",high="red") +
  facet_grid(~phase) +
  xlab("Age at measurement") +
  ylab("Average Depth category") +
  theme_bw() +
  scale_x_discrete(breaks=x_axis_breaks)

figB = prevalence_df %>% 
  dplyr::filter(variable == "c_underweight") %>% 
  ggplot(data=.,aes(x=c_age3mo,y=exposure_estimate_categories,fill=estimate)) +
  geom_tile() +
  scale_fill_gradient2(low="white",mid="yellow",high="red") +
  facet_grid(~phase) +
  xlab("Age at measurement") +
  ylab("Average Depth category") +
  theme_bw()+
  scale_x_discrete(breaks=x_axis_breaks)

figC = prevalence_df %>% 
  dplyr::filter(variable == "c_wasting") %>% 
  ggplot(data=.,aes(x=c_age3mo,y=exposure_estimate_categories,fill=estimate)) +
  geom_tile() +
  scale_fill_gradient2(low="white",mid="yellow",high="red") +
  facet_grid(~phase) +
  xlab("Age at measurement") +
  ylab("Average Depth category") +
  theme_bw()+
  scale_x_discrete(breaks=x_axis_breaks)


library(ggpubr)
ggarrange(figA,figB,figC,
          labels=c("A","B","C"),nrow=3,
          common.legend=TRUE,legend="bottom") %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_undernutrition by age and average depth.png"),width=8,height=8)
