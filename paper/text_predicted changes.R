source(".Rprofile")
source("analysis/nlg_analytic sample processing.R")
source("analysis/nlg_prediction_dfs.R")

# Main control -------------

nlr01_summary <- readRDS(paste0(path_lockdown_folder,"/working/nlr01_summary.RDS"))
nlu01_summary <- readRDS(paste0(path_lockdown_folder,"/working/nlu01_summary.RDS"))

c1 = c2 = 0
p1 = -7.976 
p2 = -6.064

labels_change = c("Previous minus 7976pp","Previous minus 6064pp")
pred_dfs_01r = nlg_prediction_dfs(r = 1, curr_change = c1, pre_change = p1)
pred_dfs_02r = nlg_prediction_dfs(r = 1, curr_change = c2, pre_change = p2)

pred_dfs_01u = nlg_prediction_dfs(r = 0, curr_change = c1, pre_change = p1)
pred_dfs_02u = nlg_prediction_dfs(r = 0, curr_change = c2, pre_change = p2)

# Specify number of age groups
n_groups = 7

pred_y_rural = map_dfr(1:n_groups,
                       function(i){
                         x1r = pred_dfs_01r[[i]];
                         x2r = pred_dfs_02r[[i]];
                         
                         bind_rows(
                           run_prediction_dfs(x1r,g = i,rural = 1) %>% 
                             mutate(curr_change = c1,pre_change = p1),
                           run_prediction_dfs(x2r,g = i,rural = 1) %>% 
                             mutate(curr_change = c2,pre_change = p2)
                         ) %>% 
                           mutate(group = paste0("G",i)) %>% 
                           return(.)
                         
                         
                         
                       })


pred_y_urban = map_dfr(1:n_groups,
                       function(i){
                         x1u = pred_dfs_01u[[i]];
                         x2u = pred_dfs_02u[[i]];
                         
                         bind_rows(
                           run_prediction_dfs(x1u,g = i,rural = 0) %>% 
                             mutate(curr_change = c1,pre_change = p1),
                           run_prediction_dfs(x2u,g = i,rural = 0) %>% 
                             mutate(curr_change = c2,pre_change = p2)
                         ) %>% 
                           mutate(group = paste0("G",i)) %>% 
                           return(.)
                         
                         
                         
                       })


bind_rows(pred_y_rural %>% mutate(region = "Rural"),
          pred_y_urban %>% mutate(region = "Urban")) %>% 
  mutate(lci = mean - 1.96*se,
         uci = mean + 1.96*se) %>% 
  View()
