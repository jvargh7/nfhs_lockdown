ns401_summary <- readRDS(paste0(path_lockdown_folder,"/working/ns401_summary.RDS"))

model_terms = (ns401_summary[[1]][[1]]$term)

coefs_stunting = ns401_summary[[1]][[1]]$estimate[str_detect(model_terms,"exposure_estimate")]
coefs_underweight = ns401_summary[[2]][[1]]$estimate[str_detect(model_terms,"exposure_estimate")]
coefs_wasting = ns401_summary[[3]][[1]]$estimate[str_detect(model_terms,"exposure_estimate")]

vcov_stunting = ns401_summary[[1]][[2]][[1]][which(str_detect(model_terms,"exposure_estimate")),str_detect(model_terms,"exposure_estimate")]
vcov_underweight = ns401_summary[[2]][[2]][[1]][which(str_detect(model_terms,"exposure_estimate")),str_detect(model_terms,"exposure_estimate")]
vcov_wasting = ns401_summary[[3]][[2]][[1]][which(str_detect(model_terms,"exposure_estimate")),str_detect(model_terms,"exposure_estimate")]

exposure_fit = data.frame(exposure_estimate = seq(0,5.4,by=0.1))  %>% 
  mutate(exposure_estimate_gt10 = case_when(exposure_estimate >= 1.0 ~ exposure_estimate - 1.0,
                                            TRUE ~ 0),
         exposure_estimate_gt20 = case_when(exposure_estimate >= 2.0 ~ exposure_estimate - 2.0,
                                            TRUE ~ 0),
         exposure_estimate_gt30 = case_when(exposure_estimate >= 3.0 ~ exposure_estimate - 3.0,
                                            TRUE ~ 0)
  )

row_1unit = which(exposure_fit$exposure_estimate == 1)

exposure_fit_matrix = as.matrix(exposure_fit)

exposure_fit$coef_stunting = as.numeric(exposure_fit_matrix %*% coefs_stunting)
exposure_fit$coef_underweight = as.numeric(exposure_fit_matrix %*% coefs_underweight)
exposure_fit$coef_wasting = as.numeric(exposure_fit_matrix %*% coefs_wasting)

# exposure_fit$se_stunting = sqrt(((exposure_fit_matrix>0)*1)%*%vcov_stunting%*% t(((exposure_fit_matrix>0)*1))) %>% diag(.) 
# exposure_fit$se_underweight = sqrt(((exposure_fit_matrix>0)*1)%*%vcov_underweight%*% t(((exposure_fit_matrix>0)*1))) %>% diag(.)
# exposure_fit$se_wasting = sqrt(((exposure_fit_matrix>0)*1)%*%vcov_wasting%*% t(((exposure_fit_matrix>0)*1))) %>% diag(.)


exposure_fit$se_stunting = sqrt(exposure_fit_matrix%*%vcov_stunting%*% t(exposure_fit_matrix)) %>% diag(.) %>% .[row_1unit]
exposure_fit$se_underweight = sqrt(exposure_fit_matrix%*%vcov_underweight%*% t(exposure_fit_matrix)) %>% diag(.) %>% .[row_1unit]
exposure_fit$se_wasting = sqrt(exposure_fit_matrix%*%vcov_wasting%*% t(exposure_fit_matrix)) %>% diag(.) %>% .[row_1unit]

figB = ggplot(data=exposure_fit) +
  geom_path(aes(x=exposure_estimate,y=coef_stunting,col="Stunting")) +
  geom_ribbon(aes(x=exposure_estimate,ymin=(coef_stunting - 1.96*se_stunting), ymax = (coef_stunting + 1.96*se_stunting),fill="Stunting"),alpha=0.15) +
  geom_path(aes(x=exposure_estimate,y=coef_underweight,col="Underweight")) +
  geom_ribbon(aes(x=exposure_estimate,ymin=(coef_underweight - 1.96*se_underweight), ymax = (coef_underweight + 1.96*se_underweight),fill="Underweight"),alpha=0.15) +
  geom_path(aes(x=exposure_estimate,y=coef_wasting,col="Wasting")) +
  geom_ribbon(aes(x=exposure_estimate,ymin=(coef_wasting - 1.96*se_wasting), ymax = (coef_wasting + 1.96*se_wasting),fill="Wasting"),alpha=0.15) +
  scale_color_manual(name = "",values=c("Stunting"="red","Underweight"="darkblue","Wasting"="darkgreen"))+
  scale_fill_manual(name="",values=c("Stunting"="red","Underweight"="darkblue","Wasting"="darkgreen")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Average Depth (in 10 pp)") +
  ylab("log(Prevalence Ratio)")

figB %>% 
  ggsave(.,filename=paste0(path_lockdown_folder,"/figures/figure_non linearity in average depth.png"),width=8,height=4)

