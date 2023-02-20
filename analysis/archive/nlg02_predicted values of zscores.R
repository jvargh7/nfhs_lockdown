source(".Rprofile")
source("analysis/nlg_analytic sample processing.R")
source("analysis/nlg_prediction_dfs.R")

# Main control -------------

nlr01_summary <- readRDS(paste0(path_lockdown_folder,"/working/nlr01_summary.RDS"))
nlu01_summary <- readRDS(paste0(path_lockdown_folder,"/working/nlu01_summary.RDS"))

c0 = p0 = p1 = c2 = 0
c1 = p2 = c3 = p3 = -2.5

labels_change = c("Baseline","Current minus 2500pp","Previous minus 2500pp","Both minus 2500pp")

# Rural -------------------
pred_dfs_00 = nlg_prediction_dfs(r = 1, curr_change = c0, pre_change = p0)
pred_dfs_10 = nlg_prediction_dfs(r = 1, curr_change = c1, pre_change = p1)
pred_dfs_01 = nlg_prediction_dfs(r = 1, curr_change = c2, pre_change = p2)
pred_dfs_11 = nlg_prediction_dfs(r = 1, curr_change = c3, pre_change = p3)

# Specify number of age groups
n_groups = 7

pred_y_rural = map_dfr(1:n_groups,
                function(i){
                  x0 = pred_dfs_00[[i]];
                  x1 = pred_dfs_10[[i]];
                  x2 = pred_dfs_01[[i]];
                  x3 = pred_dfs_11[[i]];
                  
                  bind_rows(
                    run_prediction_dfs(x0,g = i,rural = 1) %>% 
                      mutate(curr_change = c0,pre_change = p0),
                    run_prediction_dfs(x1,g = i,rural = 1) %>% 
                      mutate(curr_change = c1,pre_change = p1),
                    run_prediction_dfs(x2,g = i,rural = 1) %>% 
                      mutate(curr_change = c2,pre_change = p2),
                    run_prediction_dfs(x3,g = i,rural = 1) %>% 
                      mutate(curr_change = c3,pre_change = p3)
                    ) %>% 
                    mutate(group = paste0("G",i)) %>% 
                    return(.)

                  
                  
                })


# Urban -------------------
pred_dfs_00 = nlg_prediction_dfs(r = 0, curr_change = c0, pre_change = p0)
pred_dfs_10 = nlg_prediction_dfs(r = 0, curr_change = c1, pre_change = p1)
pred_dfs_01 = nlg_prediction_dfs(r = 0, curr_change = c2, pre_change = p2)
pred_dfs_11 = nlg_prediction_dfs(r = 0, curr_change = c3, pre_change = p3)

# Specify number of age groups
n_groups = 7

pred_y_urban = map_dfr(1:n_groups,
                       function(i){
                         x0 = pred_dfs_00[[i]];
                         x1 = pred_dfs_10[[i]];
                         x2 = pred_dfs_01[[i]];
                         x3 = pred_dfs_11[[i]];
                         
                         bind_rows(
                           run_prediction_dfs(x0,g = i,rural = 0) %>% 
                             mutate(curr_change = c0,pre_change = p0),
                           run_prediction_dfs(x1,g = i,rural = 0) %>% 
                             mutate(curr_change = c1,pre_change = p1),
                           run_prediction_dfs(x2,g = i,rural = 0) %>% 
                             mutate(curr_change = c2,pre_change = p2),
                           run_prediction_dfs(x3,g = i,rural = 0) %>% 
                             mutate(curr_change = c3,pre_change = p3)
                         ) %>% 
                           mutate(group = paste0("G",i)) %>% 
                           return(.)
                         
                         
                         
                       })


bind_rows(pred_y_rural %>% mutate(region = "Rural"),
          pred_y_urban %>% mutate(region = "Urban")) %>% 
  mutate(lci = mean - 1.96*se,
         uci = mean + 1.96*se) %>% 
  mutate(group = factor(group,levels=paste0("G",1:7),
                        labels=c("0-6","7-12",
                                 "13-18","19-24",
                                 "25-36","37-48",
                                 "49-60"))) %>% 
  mutate(exposure_type = case_when(curr_change == c0 & pre_change == p0 ~ 1,
                                   curr_change == c1 & pre_change == p1 ~ 2,
                                   curr_change == c2 & pre_change == p2 ~ 3,
                                   curr_change == c3 & pre_change == p3 ~ 4)) %>% 
  mutate(exposure_type = factor(exposure_type,
                                levels = c(1:4),
                                labels=labels_change)) %>% 
  saveRDS(.,paste0("analysis/nlg02_predicted values of zscores.RDS"))
