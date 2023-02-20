
v024_comparison = readxl::read_excel("data/NFHS Lockdown Variable List.xlsx",sheet="v024 comparison") %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

library(lubridate)

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
  geom_vline(xintercept = c(ymd(lockdown_start)),col="black",linetype=2) +
  
  # https://www.statology.org/add-text-to-ggplot/ 
  # Use annotate
  # https://stackoverflow.com/questions/26684023/how-to-left-align-text-in-annotate-from-ggplot2
  # Use hjust = 0
  annotate("text",x= (ymd(lockdown_start) + 1),y= 5,label="Phase 1 Lockdown starts",hjust = 0) +
  geom_vline(xintercept = ymd(delta_start),col="darkblue",linetype=2) +
  annotate("text",x= (ymd(delta_start) + 1),y= 5,label="Delta starts",hjust = 0) +
  theme(legend.position = "bottom") +
  scale_color_discrete(name="") +
  scale_y_continuous(limits = c(-100,5),breaks=seq(-80,5,by=20)) +
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
  scale_x_date(limits=c(ymd(nfhs5p1_start), ymd(nfhs5p2_stop)+15),date_labels = "%b-%Y",
               breaks = seq.Date(ymd(nfhs5p1_start), ymd(nfhs5p2_stop),by="quarter")) +
  geom_rect(aes(xmin=ymd(nfhs5p1_start),xmax=ymd(nfhs5p1_stop),ymin=-100,ymax=-95),fill="grey80",col=NA) +
  annotate("text",x= (ymd(nfhs5p1_start) + 80),y= -98,label="NFHS5 Phase 1",hjust = 0) +
  geom_rect(aes(xmin=ymd(nfhs5p2_start),xmax=ymd(nfhs5p2_stop),ymin=-100,ymax=-95),fill="grey80",col=NA) +
  annotate("text",x= (ymd(nfhs5p2_start) + 30),y= -98,label="NFHS5 Phase 2",hjust = 0)
  
  
  
  # geom_vline(xintercept = c(ymd(nfhs5p1_start)),col="black",linetype=2) + 
  # geom_vline(xintercept = c(ymd(nfhs5p1_stop)),col="black",linetype=2) +
  # geom_vline(xintercept = c(ymd(nfhs5p2_start)),col="black",linetype=2)
  # geom_vline(xintercept = c(ymd(nfhs5p2_stop)),col="black",linetype=2)
  # annotate("text",x= (ymd(nfhs5p1_start) + 1),y= -100,label="NFHS5 Phase 1 starts",hjust = 0) +
  # annotate("text",x= (ymd(nfhs5p2_start) + 1),y= -100,label="NFHS5 Phase 2 starts",hjust = 0) +
  # annotate("text",x= (ymd(nfhs5p2_stop) + 1),y= -100,label="NFHS5 Phase 2 ends",hjust = 0)

fig %>% 
  ggsave(.,filename = paste0(path_lockdown_folder,"/figures/figure_intensity of mobility restriction by state.png"),width=10,height=6)
