
v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison") %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)



india_cmi <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
  dplyr::filter(date >= "2019-06-17",date <= "2021-04-30",!location_name %in% c("India","Chandigarh")&!str_detect(location_name,"Puducherry")) %>% 
  dplyr::filter(location_name %in% v024_comparison$location_name)


fig <- india_cmi %>% 
  ggplot(data=.,aes(x=date,group=location_name,y = mobility_composite, col=location_name)) +
  # geom_point() +
  geom_path() +
  theme_bw() +
  xlab("Date") +
  ylab("%Mobility Reduction relative to pre-pandemic") +
  geom_hline(yintercept = 0,col="red",linetype=2) +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="") +
  scale_y_continuous(limits = c(-80,0),breaks=seq(-80,0,by=20))

fig %>% 
  ggsave(.,filename = paste0(path_lockdown_folder,"/figures/figure_intensity of mobility restriction by state.png"),width=10,height=6)
