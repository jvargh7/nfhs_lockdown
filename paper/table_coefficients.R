source(".Rprofile")
source("functions/exp_T.R")
source("C:/code/external/functions/nhst/se_from_pvalue.R")
source("C:/code/external/functions/preprocessing/round_d.R")
r_coefs = read_csv("analysis/nlr01_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

r_coefs$p.adj = p.adjust(r_coefs$p.value,method="BH")


u_coefs = read_csv("analysis/nlu01_coefficients.csv") %>% 
  dplyr::filter(str_detect(term,"cumulative")) %>% 
  dplyr::filter(outcome %in% c("c_haz","c_waz","c_whz"))

u_coefs$p.adj = p.adjust(u_coefs$p.value,method="BH")


e_flag = FALSE

table_coefs = bind_rows(r_coefs %>% mutate(region = "Rural"),
                        u_coefs %>% mutate(region = "Urban")) %>% 
  mutate(std.error.adj = se_from_pvalue(estimate,p.adj,std.error))%>% 
  mutate(L.adj = estimate - 1.96*std.error.adj,
         U.adj = estimate + 1.96*std.error.adj) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(estimate,e_flag),2)," \t(",
                         round_d(exp_T(L.adj,e_flag),2),", ",
                         round_d(exp_T(U.adj,e_flag),2),")"),
         Coef.adj = exp_T(estimate,e_flag),
         lci.adj = exp_T(L.adj,e_flag),
         uci.adj = exp_T(U.adj,e_flag)) %>% 
  mutate(inv_variance = 1/(std.error.adj^2))


summarized_coefs = table_coefs %>% 
  group_by(region,outcome,term) %>% 
  summarize(est_pooled = sum(estimate*inv_variance)/sum(inv_variance),
            se_pooled = 1/sqrt(sum(inv_variance))) %>% 
  mutate(L_pooled = est_pooled - 1.96*se_pooled,
         U_pooled = est_pooled + 1.96*se_pooled) %>% 
  mutate(RR.adj = paste0(round_d(exp_T(est_pooled,e_flag),2)," \t(",
                         round_d(exp_T(L_pooled,e_flag),2),", ",
                         round_d(exp_T(U_pooled,e_flag),2),")"),
         Coef.adj = exp_T(est_pooled,e_flag),
         lci.adj = exp_T(L_pooled,e_flag),
         uci.adj = exp_T(U_pooled,e_flag))


bind_rows(table_coefs,
          summarized_coefs %>% mutate(model = "Pooled")) %>% 
  dplyr::select(model,region,term,outcome,RR.adj) %>% 
  pivot_wider(names_from=c("region","outcome"),values_from=RR.adj)  %>% 
  rename_at(vars(starts_with("c_")),~paste0(.,"_RR")) %>% 

write_csv(.,"paper/table_coefficients.csv")



fig_coefs = bind_rows(table_coefs,
                      summarized_coefs %>% mutate(model = "Pooled")) %>% 
  dplyr::filter(region == "Rural") %>% 
  dplyr::mutate_at(vars(lci.adj,uci.adj),function(x) case_when(x < -1.5 ~ -1.5,
                                                               x > 1.5 ~ 1.5,
                                                               TRUE ~ x)) %>% 
  mutate(group = factor(model,levels=c(paste0("m",1:7),"Pooled"),
                        labels=c("0 to 6","7 to 12","13 to 18",
                                 "19 to 24","25 to 36","37 to 48","49 to 60","Pooled"),ordered=TRUE),
         age_exposure = factor(term,
                               levels=paste0("p",1:8,"_cumulative"),
                               labels=c("Gestation","0 to 6",
                                        "7 to 12","13 to 18",
                                        "19 to 24","25 to 36","37 to 48","49 to 60"),ordered=TRUE),
         outcome = factor(outcome,labels=c("Height-for-Age","Weight-for-Age","Weight-for-Height"))) %>%
  # ,linetype = region
  ggplot(data=.,aes(x=Coef.adj,y= age_exposure,xmin = lci.adj,xmax=uci.adj,col=group)) +
  theme_bw() +
  geom_point(position = position_dodge2(reverse = TRUE, width = 0.9), stat="identity") +
  geom_errorbar(position = position_dodge2(reverse=TRUE,width=0.9)) +
  facet_grid(age_exposure~outcome,scales="free_y") +
  scale_x_continuous(limits=c(-1.5,1.5)) +
  scale_y_discrete(limits=rev) +
  scale_color_manual(name = "Age at measurement (g)",values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "black")) +
  xlab("Coefficient (95% CI)") +
  ylab("Interval at exposure (a)") + 
  geom_vline(xintercept = 0,col="black",linetype = 3) +
  theme(legend.position = "top")

fig_coefs %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_coefficients for rural.png"),width=9,height = 6)






