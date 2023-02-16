v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison") %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

nfhs4 <- readRDS(paste0(path_lockdown_folder,"/working/nfhs4 child.RDS")) %>% 
  dplyr::mutate(status = case_when(v024 %in% v024_comparison$v024_nfhs4 ~ "Included",
                                   TRUE ~ "Excluded"))

india_cmi <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
  dplyr::filter(date >= "2019-06-17",date <= "2021-04-30",!location_name %in% c("India","Chandigarh")&!str_detect(location_name,"Puducherry")) %>% 
  dplyr::mutate(status = case_when(location_name %in% v024_comparison$location_name ~ "Included",
                                   TRUE ~ "Excluded"))


figB <- india_cmi %>% 
  ggplot(data=.,aes(x=date,y=mobility_composite,group=location_name,col=status)) +
  geom_path() +
  theme_bw() +
  xlab("Date") +
  ylab("%Mobility Reduction relative to pre-pandemic") +
  geom_hline(yintercept = 0,col="red",linetype=2) +
  theme(legend.position = "bottom") +
  scale_color_manual(name="",values=c("grey","darkblue")) +
  scale_y_continuous(limits = c(-100,0),breaks=seq(-100,0,by=20))


figA <- nfhs4 %>% 
  ggplot(data=.,aes(x=c_age,y=c_haz,group=v024,col=status)) +
  geom_smooth(alpha=0.2,fill="grey80") +
  theme_bw() +
  xlab("Age (months)") +
  ylab("Height-for-age z-scores") +
  # geom_hline(yintercept = 0,col="red",linetype=2) +
  theme(legend.position = "bottom") +
  scale_color_manual(name="",values=c("grey","darkblue"))
# figB

require(ggpubr)
ggarrange(figA,
          figB,
          nrow=2,ncol=1,
          labels = c("A","B"),
          common.legend=TRUE,
          legend = "bottom") %>% 
  ggsave(.,filename = paste0(path_lockdown_folder,"/figures/figure_analytic and excluded.png"),
         height = 10,width = 8)
