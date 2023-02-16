source("analysis/nla_analytic sample processing.R")
source("analysis/nla_equations.R")
nla01_coef = read_csv("analysis/nla01_coefficients of exposure_gt20 as modifier.csv") %>% 
  dplyr::filter(model == "m1a")

ee_df = analytic_sample %>% 
  dplyr::distinct(exposure_estimate,exposure_estimate_centered) 

set.seed(2023)
ee_selected = ee_df[c(1,sample(c(2:(nrow(ee_df)-1)),1000,replace=FALSE),nrow(ee_df)),] %>% 
  arrange(exposure_estimate)
  
summary_marginal = map_dfr(ee_selected$exposure_estimate_centered,
                           function(ee){
                             df = analytic_sample %>% 
                               mutate(exposure_estimate_centered = ee) %>% 
                               as.data.frame();
                             print(ee);
                             map_dfr(outcome_vars,
                                     function(o){
                                       # Creating model matrix -------
                                       f01a = paste0(o,e01a) %>% as.formula();
                                       
                                       
                                       mat = model.matrix(f01a,df);
                                       
                                       # Matching coefficients
                                       coef_mat = nla01_coef %>% 
                                         dplyr::filter(outcome == o) %>% 
                                         dplyr::select(term,estimate);
                                       
                                       pred = mat %*% coef_mat$estimate;
                                       
                                       data.frame(outcome = o,
                                                  exposure_estimate_centered = ee,
                                                  mean_pred = Hmisc::wtd.mean(pred,weights=df$sampleweight,normwt="ignored",na.rm=TRUE)) %>% 
                                         return(.)

                                       
                                     })
                             
                             
                             
                           })

saveRDS(summary_marginal,"paper/table_main analysis marginal standardization.RDS")
# summary_marginal <- readRDS("paper/table_main analysis marginal standardization.RDS")

fig_marginal <- summary_marginal %>% 
  mutate(prob_pred = exp(mean_pred)) %>% 
  dplyr::select(-mean_pred) %>% 
  # pivot_wider(values_from="prob_pred",names_from="outcome") %>% 
  mutate(exposure_estimate = rep(ee_selected$exposure_estimate,each=length(outcome_vars))) %>% 
  mutate(outcome = factor(outcome,levels=outcome_vars,labels=c("Stunting","Underweight","Wasting"))) %>% 
  arrange(exposure_estimate) %>% 
  ggplot(data=.,aes(x=exposure_estimate*10,y=prob_pred,col=outcome)) +
  geom_path() +
  xlab("Average Depth (pp)") +
  ylab("Probability of Outcome") +
  scale_color_manual(name="",values=c("red","darkgreen","darkblue")) +
  theme_bw() +
  scale_y_continuous(limits=c(0,0.5),breaks=seq(0,0.5,by=0.05)) +
  theme(legend.position = "bottom")

fig_marginal %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_main analysis marginal standardization.png"),width=6,height=4)

