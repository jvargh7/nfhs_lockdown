nfhs4 <- readRDS(paste0(path_lockdown_folder,"/working/nfhs4 child.RDS")) %>% 
  mutate_at(vars(hh_wealthq,hh_wealthqur), function(x) factor(x,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  dplyr::select(v001,v002,v003,v024_nfhs5,nfhs5_state,phase,
                c_dob,c_age,c_interview,
                starts_with("icds")
  )

dates_restrictions <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
  mutate(month = paste0(year(date),"-",month(date),"-","15") %>% ymd(.)) %>% 
  group_by(v024,sdist) %>% 
  dplyr::filter(date >= min(date) + 364) %>% 
  ungroup()

monthly_restrictions <- readRDS(paste0(path_lockdown_folder,"/working/india_cmi_imputed_step2.RDS")) %>% 
  group_by(v024,sdist) %>% 
  summarize(mean_composite = zoo::rollmean(mobility_composite,k=365,align="right")) %>% 
  bind_cols(dates_restrictions %>% dplyr::select(month,date)) %>% 
  group_by(month,v024) %>% 
  summarize(mean_composite = mean(mean_composite)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(v024))


nfhs5 <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5 child.RDS")) %>% 
  dplyr::select(v001,v002,v003,v024_nfhs5,nfhs5_state,phase,
                c_dob,c_age,c_interview,
                starts_with("icds")
  )


# The key variables are: 
# - ICDS Service
# - Exposure to mobility restriction
# - State


fig_df <- bind_rows(nfhs5,
                    nfhs4
                    ) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  mutate(c_mob = paste0(year(c_dob),"-",month(c_dob),"-","15") %>% ymd(.),
         c_moi = paste0(year(c_interview),"-",month(c_interview),"-","15") %>% ymd(.),
         c_agecategory = case_when(c_age <= 6 ~ 1,
                                   c_age %in% c(7:24) ~ 2,
                                   c_age > 24 ~ 3,
                                   TRUE ~ NA_real_)
         ) %>% 
  mutate(c_agecategory = factor(c_agecategory,labels=c("0-6","7-24","25-59"))) %>% 
  dplyr::filter(!is.na(c_age)) %>% 
  group_by(v024_nfhs5,nfhs5_state,
           phase,c_moi,
           c_agecategory) %>% 
  mutate_at(vars(starts_with("icds_")),function(x) case_when(is.na(x) ~ 0,
                                                             TRUE ~ x)) %>% 
  summarize_at(vars(starts_with("icds_")),.funs = list(m= ~mean(.,na.rm=TRUE)*100, n = ~sum(!is.na(.)))) %>% 
  
  right_join(monthly_restrictions,
            by=c("c_moi"="month","v024_nfhs5"="v024")) %>% 
  arrange(v024_nfhs5,c_moi) %>% 
  group_by(v024_nfhs5) %>% 
  mutate_at(vars(nfhs5_state,phase),~zoo::na.locf(.,na.rm = FALSE)) %>% 
  ungroup()


fig_df <- bind_rows(
  fig_df %>% 
    dplyr::filter(!is.na(nfhs5_state),is.na(c_agecategory)) %>% 
    mutate(c_agecategory = "0-6") %>% 
    bind_rows({.} %>% 
                mutate(c_agecategory = "7-24"),
              {.} %>% 
                mutate(c_agecategory = "25-59")),
  fig_df %>% 
    dplyr::filter(!is.na(nfhs5_state),!is.na(c_agecategory))
  
) %>% 
  arrange(v024_nfhs5,c_moi)


n5_states = unique(fig_df$nfhs5_state)

plot_ts <- function(df,n5_state,y_var,y_title){
  
  plt = df %>% 
    dplyr::filter(nfhs5_state == n5_state) %>% 
    rename(y_var = y_var) %>% 
    ggplot(data=.,aes(x=c_moi,y=y_var,col=c_agecategory)) +
    geom_path() +
    geom_line(aes(y = I(-1*mean_composite),col="Mobility Restriction")) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20)) +
    scale_x_date(date_breaks = "6 months",date_labels="%b-%y") +
    scale_color_manual(name = "",values=c("0-6"="blue","7-24"="darkgreen","25-59"="black","Mobility Restriction"="red")) +
    theme_bw() +
    geom_vline(xintercept = ymd(c("2020-03-24","2020-05-01")),col="red",linetype=2) +
    ggtitle(y_title) +
    xlab("Month of Interview") +
    ylab("Percentage")
    
  
  plt %>% 
    return(.)
  
  
}

  
  require(ggpubr)
  pdf(paste0(path_lockdown_folder,"/figures/figure_icds by month of measurement.pdf"),width = 10,height=6)
  for (n5_s in n5_states){
    title = paste0(n5_s);
    
    
    figA <- plot_ts(fig_df,n5_s,"icds_child_thrfreq_m","Received Take Home Rations in Childhood");
    figB <- plot_ts(fig_df,n5_s,"icds_preg_thrfreq_m","Received Take Home Rations in Pregnancy");
    figC <- plot_ts(fig_df,n5_s,"icds_bf_thrfreq_m","Received Take Home Rations when breastfeeding");
    ggarrange(figA,figB,figC, nrow = 3,ncol=1,labels = LETTERS[1:3]) %>% 
      annotate_figure(., top = text_grob(title, 
                                         color = "black", face = "bold", size = 12)) %>% 
      print(.)
    
    
  }
  
  dev.off()
  






