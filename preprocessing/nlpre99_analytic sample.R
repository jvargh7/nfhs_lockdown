
nfhs5_exposure <- readRDS(paste0(path_lockdown_folder,"/working/nfhs5 child.RDS"))   %>% 
  mutate(m_alcohol = case_when(is.na(m_alcohol) ~ 0,
                               TRUE ~ m_alcohol)) %>% 
  mutate_at(vars(hh_wealthq,hh_wealthqur), function(x) factor(x,levels=c(1:5),labels=c("Lowest","Low","Medium","High","Highest")))

nfhs5_exposure %>% 
  dplyr::filter(!v024_nfhs5 %in% v024_nfhs5_14states) %>% 
saveRDS(.,paste0(path_lockdown_folder,"/working/excluded_states.RDS"))




# Step 2 ---- Restrict to 14 states --------
analytic_sample_s2 <- nfhs5_exposure  %>% 
  dplyr::filter(v024_nfhs5 %in% v024_nfhs5_14states)

table(analytic_sample_s2$phase,useNA="always")
analytic_sample_s2 %>% 
  dplyr::filter(is.na(c_haz) | is.na(c_waz) | is.na(c_whz) | is.na(m_height)) %>% 
  saveRDS(.,paste0(path_lockdown_folder,"/working/excluded_anthro.RDS"))

# Step 3 -----Restrict to non-NA values --------
analytic_sample_s3 <- analytic_sample_s2 %>% 
  dplyr::filter(!is.na(c_haz) & !is.na(c_waz) & !is.na(c_whz) & !is.na(m_height))

table(analytic_sample_s3$phase,useNA="always")

# Adding exposure data ------------

source("functions/average_mobility_restriction.R")




require(furrr)
require(progressr)
gc()
options(future.globals.maxSize= (6*1024*1024)^3) #6GB
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize

analytic_sample_s3p2 <- analytic_sample_s3 %>% 
  dplyr::filter(phase == 2) %>% 
  # dplyr::filter(v024_nfhs5 == 4) %>%
  # .[1:100,] %>%
  # https://stackoverflow.com/questions/46899441/row-wise-iteration-like-apply-with-purrr
  bind_cols(
    future_pmap_dfr(.,.f=function(c_dob,v024_nfhs5,sdist,c_interview,...){
      tryCatch({
        average_mobility_restriction(c_dob,v024_nfhs5,sdist,c_interview)},
        error = function(e){
          data.frame(exposure_estimate = NA,
                     p1_estimate = NA,
                     p2_estimate = NA,
                     p3_estimate = NA,
                     p4_estimate = NA,
                     p1_n = NA,
                     p2_n = NA,
                     p3_n = NA,
                     p4_n = NA)
        })
    })
  )  %>% 
  # Impute number of observations used as 0
  mutate_at(vars(matches("(_n|_gt20)$")),.funs = function(x) case_when(is.na(x) ~ 0,
                                                               TRUE ~ as.numeric(x)))


analytic_sample_s3p1 <- analytic_sample_s3 %>% 
  dplyr::filter(phase == 1) %>% 
  # dplyr::filter(v024_nfhs5 == 4) %>%
  # .[1:100,] %>%
  # https://stackoverflow.com/questions/46899441/row-wise-iteration-like-apply-with-purrr
  bind_cols(
    future_pmap_dfr(.,.f=function(c_dob,v024_nfhs5,sdist,c_interview,...){
      tryCatch({
        average_mobility_restriction(c_dob,v024_nfhs5,sdist,c_interview) %>% 
          dplyr::select(starts_with("bm"),starts_with("bp"))},
        error = function(e){
          data.frame(bp0_estimate = NA_real_)
        })
    })
  )  %>% 
  # Impute number of observations used as 0
  mutate_at(vars(starts_with("bp"),starts_with("bm")),.funs = function(x) case_when(is.na(x) ~ NA_real_,
                                                                                    TRUE ~ 0))



analytic_sample_s4 <- bind_rows(
  analytic_sample_s3p2,
  analytic_sample_s3p1)




saveRDS(analytic_sample_s4,paste0(path_lockdown_folder,"/working/analytic_sample.RDS"))

# analytic_sample_s4 <- readRDS(paste0(path_lockdown_folder,"/working/analytic_sample.RDS"))

