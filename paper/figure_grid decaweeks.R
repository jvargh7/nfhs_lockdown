require(lubridate)

overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.)) %>% 
  mutate(term2 = factor(e_interaction,levels=c(1:19),labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))) %>% as.character(.))


overlaps <- readRDS("data/overlaps.RDS")  %>% 
  mutate(conception = dates - 38*7,
         end1000 = dates + 1000 - 38*7,
         start24mo = dates + 24*30,
         end60mo = dates + 60*30,
         
         end1000_v2 = case_when(end1000 > as_date(nfhs5p2_stop) ~ as_date(nfhs5p2_stop),
                                TRUE ~ end1000),
         start24mo_v2 = case_when(start24mo > end1000_v2 ~ NA_Date_,
                                  start24mo > as_date(nfhs5p2_stop) ~ as_date(nfhs5p2_stop),
                                TRUE ~ end1000),
         end60mo_v2 = case_when(end60mo < start24mo_v2 ~ NA_Date_,
                                end60mo > as_date(nfhs5p2_stop) ~ as_date(nfhs5p2_stop),
                                TRUE ~ end60mo),
         ) %>% 
  dplyr::select(dates,conception,end1000,start24mo,end60mo,
                end1000_v2,start24mo_v2,end60mo_v2,
                ends_with("_d")) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-c(11:13)]))


# Combined ----------------


figCombined <- overlaps %>% 
  dplyr::filter(month(dates) %in% seq(1,11,by=2)) %>% 
  dplyr::filter(term2 %in% paste0("Table Order ",sprintf("%02d",c(1:19)))) %>% 
  mutate(term3 = factor(term2,levels=paste0("Table Order ",sprintf("%02d",c(1:19))) %>% as.character(.),
                        labels =c("None",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo"),ordered = TRUE)) %>% 
  
  ggplot(data=.,aes(xmin=conception,x=dates,xmax=end1000,y=dates,group=dates,col=term3)) +
  geom_errorbar() +
  geom_errorbar(aes(xmin = start24mo,xmax = end60mo),col="grey40") +
  geom_point(col="black") + 
  ylab("Month of Birth") +
  xlab("Calendar Month") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  scale_y_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  theme_bw() +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2,size=1.2) +
  scale_color_brewer(name="",palette = "Set3") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 2)))
require(ggpubr)
figCombined %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_Combined grid exposure first 1000 days.png"),width = 10,height=10)


figCombined2 <- overlaps %>% 
  dplyr::filter(month(dates) %in% seq(1,11,by=2)) %>% 
  dplyr::filter(term2 %in% paste0("Table Order ",sprintf("%02d",c(1:19)))) %>% 
  mutate(term3 = factor(term2,levels=paste0("Table Order ",sprintf("%02d",c(1:19))) %>% as.character(.),
                        labels =c("None",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo"),ordered = TRUE)) %>% 
  
  ggplot(data=.,aes(xmin=conception,x=dates,xmax=end1000_v2,y=dates,group=dates,col=term3)) +
  geom_errorbar() +
  geom_errorbar(aes(xmin = start24mo_v2,xmax = end60mo_v2),col="grey40") +
  geom_point(col="black") + 
  ylab("Month of Birth") +
  xlab("Calendar Month") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  scale_y_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  theme_bw() +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2,size=1.2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2,size=1.2) +
  scale_color_brewer(name="",palette = "Set3") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 2)))
require(ggpubr)
figCombined2 %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_Combined2 grid exposure first 1000 days.png"),width = 10,height=10)


figA <- overlaps %>% 
  dplyr::filter(month(dates) %in% seq(1,11,by=2)) %>% 
  dplyr::filter(term2 %in% paste0("Table Order ",sprintf("%02d",c(1:10)))) %>% 
  mutate(term3 = factor(term2,levels=paste0("Table Order ",sprintf("%02d",c(1:10))) %>% as.character(.),
                        labels =c("None",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo"),ordered = TRUE)) %>% 
  
ggplot(data=.,aes(xmin=conception,x=dates,xmax=end1000,y=dates,group=dates,col=term3)) +
  geom_errorbar() +
  geom_point(col="black") + 
  ylab("Month of Birth") +
  xlab("Calendar Month") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  scale_y_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  theme_bw() +
  geom_vline(xintercept = as_date(demonetization_start),col="red",linetype=2) +
  geom_vline(xintercept = as_date(demonetization_stop),col="red",linetype=2) +
  scale_color_brewer(name="",palette = "Set3") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 2)))


figB <- overlaps %>% 
  dplyr::filter(month(dates) %in% seq(1,11,by=2),year(dates)>2016) %>% 
  dplyr::filter(term2 %in% paste0("Table Order ",sprintf("%02d",c(1,11:19)))) %>% 
  mutate(term3 = factor(term2,levels=paste0("Table Order ",sprintf("%02d",c(1,11:19))) %>% as.character(.),
                        labels =c("None",
                                  "Gestation","Gestation and 0/6mo","0/6mo","0/6mo and 7/12mo",
                                  "7/12mo","7/12mo and 13/18mo","13/18mo","13/18mo and 19/24mo",
                                  "19/24mo"),ordered = TRUE)) %>% 
  
  ggplot(data=.,aes(xmin=conception,x=dates,xmax=end1000,y=dates,group=dates,col=term3)) +
  geom_errorbar() +
  geom_point(col="black") + 
  ylab("Month of Birth") +
  xlab("Calendar Month") +
  scale_x_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  scale_y_date(date_breaks = "1 year",date_labels =  "%b\n %Y") +
  theme_bw() +
  geom_vline(xintercept = as_date(lockdown_start),col="blue",linetype=2) +
  geom_vline(xintercept = as_date(lockdown_stop),col="blue",linetype=2) +
  scale_color_brewer(name="",palette = "Set3") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 2)))

ggarrange(figA,figB,
          labels=LETTERS[1:2],
          nrow = 2,ncol=1, legend = "bottom",common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_grid exposure first 1000 days.png"),width = 10,height=10)
