
overlaps_unique <- readRDS("data/overlaps.RDS") %>% 
  dplyr::select(ends_with("_d")) %>% 
  distinct_at(vars(starts_with("e"))) %>% 
  mutate(e_interaction = 1:nrow(.))

summary_e_interaction <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS")) %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states) %>% 
  left_join(overlaps_unique,
            by=c(names(overlaps_unique)[-11])) %>% 
  group_by(e_interaction) %>% 
  dplyr::summarise(daterange = paste0(format(min(c_dob),"%d-%m-%Y")," to ",format(max(c_dob),"%d-%m-%Y")))



source("C:/code/external/functions/contrasts/contrasts_svyglm.R")




out <- map_dfr(paste0(rep(c("nlxb02","nlxb03"),each=3),rep(c("_glm_stunting","_glm_underweight","_glm_wasting"),times=2)),
        
        function(m){
          
          nlxb <- readRDS(paste0(path_lockdown_folder,"/working/nlxb/",m,".RDS"));
          map_dfr (2:max(summary_e_interaction$e_interaction),
                   function(i){
                     contrast_matrix <- matrix(0,nrow=3,ncol=length(nlxb$coefficients))
                     
                     # Female, Male
                     contrast_matrix[1:2,regexpr(names(nlxb$coefficients),pattern = paste0("factor\\(e_interaction\\)",i,"$"))>0] <- 1;
                     # Male, Difference
                     contrast_matrix[2:3,regexpr(names(nlxb$coefficients),pattern = paste0("factor\\(e_interaction\\)",i,":"))>0] <- 1;
                     
                     summary_coef <- contrasts_svyglm(model_matrix = contrast_matrix,
                                                      fit=NULL,
                                                      row_names=NULL,
                                                      vcov_fit = nlxb$naive.cov,
                                                      coef_fit = nlxb$coefficients,
                                                      df_fit = nlxb$df.null) %>% 
                       mutate(coefficient = case_when(term == "Contrast 1" ~ "Female",
                                                      term == "Contrast 2" ~ "Male",
                                                      term == "Contrast 3" ~ "Interaction")) %>% 
                       mutate(e_interaction = i);
                     
                     return(summary_coef)
                   }) %>% 
            mutate(model = m) %>% 
            return(.)
          
          
          
        })






nlxb02_outcomes <- out %>% 
  left_join(summary_e_interaction,
            by=c("e_interaction")) %>% 
   mutate(term2 = factor(e_interaction,labels=paste0("Table Order ",sprintf("%02d",c(10:2,19:11)))) %>% as.character(.)) %>% 
  arrange(term2) %>% 
  mutate(coef_ci = paste0(round(exp(Estimate),2)," (",
                          round(exp(LCI),2),", ",
                          round(exp(UCI),2),")")) %>% 
  dplyr::select(term2,e_interaction,coefficient,model,coef_ci)
  
  
  nlxb02_outcomes %>% 
  dplyr::filter(coefficient == "Female") %>% 
  pivot_wider(names_from = "model",values_from=coef_ci) %>% 
  write_csv(.,("models sequential sex/nlxb_unique combinations coefficients Female.csv"))

  nlxb02_outcomes %>% 
    dplyr::filter(coefficient == "Male") %>% 
    pivot_wider(names_from = "model",values_from=coef_ci) %>% 
    write_csv(.,("models sequential sex/nlxb_unique combinations coefficients Male.csv"))
  


                             