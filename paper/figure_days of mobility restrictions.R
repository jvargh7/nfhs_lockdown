source("analysis/nlg_analytic sample processing.R")
pre_curr_group = analytic_sample_wdistrict %>% 
  dplyr::select(v000:v023,group,phase,matches("p[0-9]_gt40")) %>% 
  pivot_longer(cols=contains("gt40"),names_to="interval",values_to="value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(period = case_when(group == "G1" & interval == "p1_gt40" ~ "Pre",
                            group == "G1" & interval == "p2_gt40" ~ "Curr",
                            group == "G2" & interval == "p1_gt40" ~ "Pre2",
                            group == "G2" & interval == "p2_gt40" ~ "Pre",
                            group == "G2" & interval == "p3_gt40" ~ "Curr",
                            
                            group == "G3" & interval == "p2_gt40" ~ "Pre2",
                            group == "G3" & interval == "p3_gt40" ~ "Pre",
                            group == "G3" & interval == "p4_gt40" ~ "Curr",
                            
                            group == "G4" & interval == "p3_gt40" ~ "Pre2",
                            group == "G4" & interval == "p4_gt40" ~ "Pre",
                            group == "G4" & interval == "p5_gt40" ~ "Curr",
                            
                            group == "G5" & interval == "p4_gt40" ~ "Pre2",
                            group == "G5" & interval == "p5_gt40" ~ "Pre",
                            group == "G5" & interval == "p6_gt40" ~ "Curr",
                            
                            group == "G6" & interval == "p5_gt40" ~ "Pre2",
                            group == "G6" & interval == "p6_gt40" ~ "Pre",
                            group == "G6" & interval == "p7_gt40" ~ "Curr",
                            
                            group == "G7" & interval == "p6_gt40" ~ "Pre2",
                            group == "G7" & interval == "p7_gt40" ~ "Pre",
                            group == "G7" & interval == "p8_gt40" ~ "Curr",
                            TRUE ~ NA_character_
  )) %>% 
  dplyr::filter(!is.na(period))  %>% 
  distinct(.keep_all=TRUE)

group_summary = pre_curr_group %>% 
  mutate(group = factor(group,levels=paste0("G",1:7),
                        labels=c("0 to 6","7 to 12","13 to 18",
                                 "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE),
         age_exposure = factor(interval,
                               levels=paste0("p",1:8,"_gt40"),
                               labels=c("Gestation","0 to 6",
                                        "7 to 12","13 to 18",
                                        "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE)
         
  ) %>% 
  group_by(phase,group,age_exposure) %>% 
  summarize(est = Hmisc::wtd.mean(value,weights=sampleweight),
            min = min(value),
            n_overall = n(),
            n = sum(value>0),
            max = max(value),
            se = sqrt(Hmisc::wtd.var(value,weights=sampleweight)/length(sampleweight))
  ) %>% 
  mutate(lci = est - 1.96*se,
         uci = est + 1.96*se) 


fig_days = group_summary %>% 
  dplyr::filter(phase == 2) %>% 
  ggplot(., aes(fill=group, x=est,y=age_exposure)) + 
  geom_bar(position = position_dodge2(reverse = TRUE), stat="identity", color="black") +
  theme_bw()+
  # scale_fill_grey() +
  scale_y_discrete(limits=rev) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  # https://stackoverflow.com/questions/61239753/change-order-of-filling-variable-on-a-ggplot-geom-bar
  scale_fill_discrete(name = "Age at measurement (g)") +
  xlab("Days of >40% restricted mobility from baseline") +
  ylab("Interval of exposure (a)") +
  theme(legend.position = "top") 

fig_days %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/days of mobility restrictions.png"),width=7,height = 5)
