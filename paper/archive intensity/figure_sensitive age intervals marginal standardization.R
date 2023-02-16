source("analysis/nla_analytic sample processing.R")
source("analysis/nla_equations.R")
nla01_coef = read_csv("analysis/nla01_coefficients of exposure_gt20 as modifier.csv") %>% 
  dplyr::filter(model == "m1b")

ee_df = analytic_sample %>% 
  dplyr::filter(age_le24 ==1 | age_ge36 == 1) %>% 
  dplyr::distinct(exposure_estimate,exposure_estimate_centered) 

set.seed(2023)
ee_selected = ee_df[c(1,sample(c(2:(nrow(ee_df)-1)),1000,replace=FALSE),nrow(ee_df)),] %>% 
  arrange(exposure_estimate)

summary_marginal = map_dfr(ee_selected$exposure_estimate_centered,
                           function(ee){
                             df = analytic_sample %>%
                               dplyr::filter(age_le24 ==1 | age_ge36 == 1) %>% 
                               mutate(exposure_estimate_centered = ee) %>% 
                               as.data.frame();
                             print(ee);
                             
                             map_dfr(outcome_vars,
                                     function(o){
                                       # Creating model matrix -------
                                       f01b = paste0(o,e01b) %>% as.formula();
                                       
                                       
                                       mat = model.matrix(f01b,df);
                                       
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

saveRDS(summary_marginal,"paper/table_sensitive age intervals marginal standardization.RDS")

summary_marginal %>% 
  mutate(prob_pred = exp(mean_pred)) %>% 
  dplyr::select(-mean_pred) %>% 
  # pivot_wider(values_from="prob_pred",names_from="outcome") %>% 
  mutate(exposure_estimate = rep(ee_selected$exposure_estimate,each=length(outcome_vars))) %>% 
  mutate(outcome = factor(outcome,levels=outcome_vars,labels=c("Stunting","Underweight","Wasting"))) %>% 
  arrange(exposure_estimate) %>% 
  ggplot(data=.,aes(x=exposure_estimate,y=prob_pred,col=outcome)) +
  geom_path() +
  xlab("Average Depth (in 10 pp)") +
  ylab("Probability of Outcome") +
  scale_color_manual(values=c("red","darkgreen","darkblue")) +
  theme_bw() +
  scale_y_continuous(limits=c(0,0.4),breaks=seq(0,0.4,by=0.05))
