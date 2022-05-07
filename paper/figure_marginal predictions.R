
marginal_predictions <- read_csv(paste0("paper/table_marginal predictions from poisson for outcomes.csv"))


means = marginal_predictions %>% 
  dplyr::select(exposure,marginal_stunting,marginal_underweight,marginal_wasting) %>% 
  pivot_longer(cols=-one_of("exposure"),names_to="outcome",values_to="marginal") %>% 
  mutate(outcome = case_when(outcome == "marginal_stunting" ~ "Stunting",
                             outcome == "marginal_underweight" ~ "Underweight",
                             outcome == "marginal_wasting" ~ "Wasting",
                             TRUE ~ NA_character_),
         shock = case_when(str_detect(exposure,"e1_") ~ "Demonetization",
                           TRUE ~ "COVID-19 Lockdown"),
         period = case_when(str_detect(exposure,"p1_") ~ 1,
                            str_detect(exposure,"p2_") ~ 2,
                            str_detect(exposure,"p3_") ~ 3,
                            str_detect(exposure,"p4_") ~ 4,
                            str_detect(exposure,"p5_") ~ 5,
                            TRUE ~ NA_real_)) 

se = marginal_predictions %>% 
  dplyr::select(exposure,sd_marginal_stunting,sd_marginal_underweight,sd_marginal_wasting) %>% 
  pivot_longer(cols=-one_of("exposure"),names_to="outcome",values_to="marginal") %>% 
  mutate(outcome = case_when(outcome == "sd_marginal_stunting" ~ "Stunting",
                             outcome == "sd_marginal_underweight" ~ "Underweight",
                             outcome == "sd_marginal_wasting" ~ "Wasting",
                             TRUE ~ NA_character_),
         shock = case_when(str_detect(exposure,"e1_") ~ "Demonetization",
                           TRUE ~ "COVID-19 Lockdown"),
         period = case_when(str_detect(exposure,"p1_") ~ 1,
                            str_detect(exposure,"p2_") ~ 2,
                            str_detect(exposure,"p3_") ~ 3,
                            str_detect(exposure,"p4_") ~ 4,
                            str_detect(exposure,"p5_") ~ 5,
                            TRUE ~ NA_real_),
         # Probably an incorrect approximation. ------- Need to check ---------
         se = marginal/sqrt(nrow(analytic_sample))) 


fig_out <- left_join(means,
          se %>% dplyr::select(exposure,outcome,se),
          by=c("outcome","exposure")) %>% 
  mutate(lci = marginal - 1.96*se,
         uci = marginal + 1.96*se) %>% 
  mutate(period = factor(period,labels = c("38 weeks till birth","Birth to 6 months",
                                           "7 to 12 months","13 to 18 months","19 to 24 months"))) %>% 
  ggplot(data=.,aes(x=period,y=marginal*100,ymin = lci*100,ymax=uci*100,fill=outcome,group=outcome)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(position = position_dodge(width = 0.9),width = 0.1) +
  facet_grid(~shock) +
  xlab("") +
  ylab("Difference % (Unexposed - Exposed)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "")


fig_out %>% 
  ggsave(.,filename = paste0(path_lockdown_folder,"/figures/figure_marginal predictions.png"),width = 12,height = 5)
