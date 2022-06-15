glm_stunting <- readRDS(paste0(path_lockdown_folder,"/working/nlma/nlma01u_glm_stunting.RDS"))
glm_underweight <- readRDS(paste0(path_lockdown_folder,"/working/nlma/nlma01u_glm_underweight.RDS"))
glm_wasting <- readRDS(paste0(path_lockdown_folder,"/working/nlma/nlma01u_glm_wasting.RDS"))


overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.)) %>% 
  mutate(term2 = factor(e_interaction,levels=c(1:19),labels=paste0("Table Order ",sprintf("%02d",c(1,10:2,19:11)))))

source("C:/code/external/functions/contrasts/contrasts_svyglm.R")


e2 <- paste0("Table Order ",sprintf("%02d",c(3:10,12:19)))
e1 <- paste0("Table Order ",sprintf("%02d",c(2:9,11:18)))

rr_summary <- map2_dfr(e1,e2,
                       function(x,y){
                         x1 <- overlaps_unique %>% 
                           dplyr::filter(term2 == x) %>% 
                           dplyr::select(e_interaction) %>% 
                           pull();
                         
                         y1 <- overlaps_unique %>% 
                           dplyr::filter(term2 == y) %>% 
                           dplyr::select(e_interaction) %>% 
                           pull();
                         
                         contrast_mat <- matrix(rep(0,times=length(glm_stunting$coefficients)),nrow = 1);
                         contrast_mat[1,regexpr(names(glm_stunting$coefficients),pattern = paste0("factor\\(e_interaction\\)",x1))>0] <- -1;
                         contrast_mat[1,regexpr(names(glm_stunting$coefficients),pattern = paste0("factor\\(e_interaction\\)",y1))>0] <-  1;
                         
                         stunting <- contrasts_svyglm(model_matrix = contrast_mat,fit=NULL,
                                                      row_names = "Stunting E2/E1",
                                                      vcov_fit = glm_stunting$naive.cov,
                                                      coef_fit = glm_stunting$coefficients,
                                                      df_fit = glm_stunting$df.residual);
                         underweight <- contrasts_svyglm(model_matrix = contrast_mat,fit=NULL,
                                                         row_names = "Underweight E2/E1",
                                                         vcov_fit = glm_underweight$naive.cov,
                                                         coef_fit = glm_underweight$coefficients,
                                                         df_fit = glm_underweight$df.residual);
                         wasting <- contrasts_svyglm(model_matrix = contrast_mat,fit=NULL,
                                                     row_names = "Wasting E2/E1",
                                                     vcov_fit = glm_wasting$naive.cov,
                                                     coef_fit = glm_wasting$coefficients,
                                                     df_fit = glm_wasting$df.residual);
                         
                         bind_rows(stunting,
                                   underweight,
                                   wasting) %>% 
                           mutate(e1 = x,
                                  e2 = y) %>% 
                           mutate(coef_ci = paste0(round(exp(Estimate),2)," (",
                                                   round(exp(LCI),2),", ",
                                                   round(exp(UCI),2),")"
                           )) %>% 
                           return(.)
                         
                         
                         
                         
                       }) %>% 
  dplyr::select(e1,e2,term,coef_ci) %>% 
  pivot_wider(names_from=term,values_from=coef_ci)

write_csv(rr_summary,paste0("main analysis/nlma04u_relative risk of consecutive urban.csv"))
