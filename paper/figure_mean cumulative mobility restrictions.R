source("analysis/nlg_analytic sample processing.R")
pre_curr_group = analytic_sample_wdistrict %>% 
  dplyr::select(v000:v023,group,phase,matches("p[0-9]_cumulative")) %>% 
  pivot_longer(cols=contains("cumulative"),names_to="interval",values_to="value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(period = case_when(group == "G1" & interval == "p1_cumulative" ~ "Pre",
                            group == "G1" & interval == "p2_cumulative" ~ "Curr",
                            group == "G2" & interval == "p1_cumulative" ~ "Pre2",
                            group == "G2" & interval == "p2_cumulative" ~ "Pre",
                            group == "G2" & interval == "p3_cumulative" ~ "Curr",
                            
                            group == "G3" & interval == "p2_cumulative" ~ "Pre2",
                            group == "G3" & interval == "p3_cumulative" ~ "Pre",
                            group == "G3" & interval == "p4_cumulative" ~ "Curr",
                            
                            group == "G4" & interval == "p3_cumulative" ~ "Pre2",
                            group == "G4" & interval == "p4_cumulative" ~ "Pre",
                            group == "G4" & interval == "p5_cumulative" ~ "Curr",
                            
                            group == "G5" & interval == "p4_cumulative" ~ "Pre2",
                            group == "G5" & interval == "p5_cumulative" ~ "Pre",
                            group == "G5" & interval == "p6_cumulative" ~ "Curr",
                            
                            group == "G6" & interval == "p5_cumulative" ~ "Pre2",
                            group == "G6" & interval == "p6_cumulative" ~ "Pre",
                            group == "G6" & interval == "p7_cumulative" ~ "Curr",
                            
                            group == "G7" & interval == "p6_cumulative" ~ "Pre2",
                            group == "G7" & interval == "p7_cumulative" ~ "Pre",
                            group == "G7" & interval == "p8_cumulative" ~ "Curr",
                            TRUE ~ NA_character_
  )) %>% 
  dplyr::filter(!is.na(period))  %>% 
  distinct(.keep_all=TRUE)


pre_curr_group %>% 
  dplyr::filter(phase == 2) %>% 
  group_by(period) %>% 
  summarize(est = Hmisc::wtd.mean(value*10000,weights=sampleweight),
            min = min(value*10000),
            n_overall = n(),
            n = sum(value>0),
            max = max(value*10000),
            se = sqrt(Hmisc::wtd.var(value*10000,weights=sampleweight)/length(sampleweight))
  ) %>% 
  mutate(lci = est - 1.96*se,
         uci = est + 1.96*se)    %>% 
  write_csv(.,"paper/text_pre and curr period estimate of cumulative mobility restriction.csv")



group_summary = pre_curr_group %>% 
  mutate(group = factor(group,levels=paste0("G",1:7),
                        labels=c("0 to 6","7 to 12","13 to 18",
                                 "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE),
         age_exposure = factor(interval,
                               levels=paste0("p",1:8,"_cumulative"),
                               labels=c("Gestation","0 to 6",
                                        "7 to 12","13 to 18",
                                        "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE)
         
         ) %>% 
  group_by(phase,group,age_exposure) %>% 
  summarize(est = Hmisc::wtd.mean(value*10000,weights=sampleweight),
            min = min(value*10000),
            n_overall = n(),
            n = sum(value>0),
            max = max(value*10000),
         se = sqrt(Hmisc::wtd.var(value*10000,weights=sampleweight)/length(sampleweight))
         ) %>% 
  mutate(lci = est - 1.96*se,
         uci = est + 1.96*se) 



fig_summary = group_summary %>% 
  dplyr::filter(phase == 2) %>% 
  ggplot(., aes(fill=group, x=est,xmin=lci,xmax=uci,y=age_exposure)) + 
  geom_bar(position = position_dodge2(reverse = TRUE), stat="identity", color="black") +
  theme_bw()+
  # scale_fill_grey() +
  scale_y_discrete(limits=rev) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  # https://stackoverflow.com/questions/61239753/change-order-of-filling-variable-on-a-ggplot-geom-bar
  geom_errorbar(position = position_dodge2(reverse = TRUE, padding = 0.6, width = 0.5)) +
  scale_fill_discrete(name = "Age at measurement (g)") +
  xlab("Mean Cumulative mobility restrictions (pp)") +
  ylab("Interval of exposure (a)") +
  theme(legend.position = "top") 

fig_summary %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/mean cumulative mobility restrictions.png"),width=7,height = 5)





pre_curr_group %>% 
  pivot_wider(names_from=period,values_from=value,values_fn={mean}) %>% 
  group_by(group) %>% 
  dplyr::summarise(r = cor.test(Pre,Curr)$statistic)


# For Table 1 -----------


  analytic_sample_wdistrict %>% 
  group_by(group,phase) %>% 
  tally() %>% 
  pivot_wider(names_from=phase,values_from=n)  %>% 
  mutate(group = factor(group,levels=paste0("G",1:7),
                        labels=c("0 to 6","7 to 12","13 to 18",
                                 "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE)) %>% 
  left_join(
    group_summary %>% 
      mutate(mean_ci = paste0(round(est,0)," (",
                              round(lci,0),", ",
                              round(uci,0),")"),
             range = paste0(round(min,0)," - ",
                            round(max,0))) %>% 
      dplyr::select(phase,group,age_exposure,n,mean_ci,range) %>% 
      mutate_all(~as.character(.)) %>% 
      pivot_longer(cols=-one_of(c("phase","group","age_exposure")),names_to="variable",values_to="value") %>% 
      pivot_wider(names_from=c("phase","variable"),values_from = "value"),
    by = "group"
    
  ) %>% 
  write_csv(.,"paper/table_mean cumulative mobility restrictions.csv")

