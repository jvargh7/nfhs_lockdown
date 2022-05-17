set.seed(2022)

shock <- readxl::read_excel("data/NFHS Demonetization 2016.xlsx",sheet = "Shock by District April 4") %>% 
  rename(demonetization_shock = 'Demonetization Shock') %>% 
  bind_cols(
            map_dfc(c("[.06,.25]","(.33,.39]","(.39,.45]","(.45,.53]","(.53,.64]","(.64,1.83]"),
                    function(s){
                      ds = {.} %>% 
                        dplyr::select(demonetization_shock) %>% 
                        pull();
                      x = case_when(ds == s ~ 1,
                                    is.na(ds) ~ NA_real_,
                                    TRUE ~ 0);
                      
                      
                      return(list(s = x))
                    }) %>% 
              rename_all(~paste0("level",1:6))) %>% 
  dplyr::select(sdistri,contains("level"))


nfhs4_factsheets <- read_csv("C:/code/external/nfhs-unhealthy-weight/data/districts_plus_uts.csv") %>% 
  dplyr::filter(id %in% c("S14","S16","S20_rev",
                  "S09","S08",
                  "S07","S10","S12")) %>% 
  dplyr::select(id,sdistri,nfhs4d_total) %>% 
  pivot_wider(names_from=id,values_from=nfhs4d_total)

require(mice)

shock_df <- left_join(shock,
                      nfhs4_factsheets,
                      by="sdistri")

mi_iter = 10

mi_null <- mice(shock_df,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("sdistri"),] <- 0
pred[,c("sdistri")] <- 0

# method[which(method == "pmm")] <- "rf"

mi_dfs <- mice(shock_df,
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

mi_dfs_long <- complete(mi_dfs,action = "long") %>% 
  dplyr::select(sdistri,.imp,contains("level"))

saveRDS(mi_dfs_long, "data/nfhs4 shocks imputed.RDS")

# NFHS5 ------------

sdist_nfhs4 <- readRDS("data/sdist_nfhs4.RDS")

n5_mi_dfs_long <- sdist_nfhs4 %>% 
  mutate(sdistri = as.numeric(sdistri)) %>% 
  full_join(mi_dfs_long,
            by="sdistri") %>% 
  dplyr::select(-sdistri) %>% 
  group_by(sdist,.imp) %>% 
  summarize_all(~mean(.))
saveRDS(n5_mi_dfs_long, "data/nfhs5 shocks imputed.RDS")
