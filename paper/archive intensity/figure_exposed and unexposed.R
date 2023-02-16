source("analysis/nla_analytic sample processing.R")
# df = analytic_sample %>% dplyr::filter(phase == 2) %>% dplyr::filter(p1_estimate == 0, p2_estimate==0,p3_estimate ==0)
# summary(df$c_age)
require(plotly)

fig_df <- analytic_sample  %>% 
  mutate(c_dob_15th = case_when(day(c_dob) > 15 ~ ymd(paste0(year(c_dob),"-",month(c_dob),"-",15)),
                                TRUE ~ ymd(paste0(year(c_dob),"-",month(c_dob),"-",1))),
         c_interview_15th = case_when(day(c_interview) > 15 ~ ymd(paste0(year(c_interview),"-",month(c_interview),"-",15)),
                                TRUE ~ ymd(paste0(year(c_interview),"-",month(c_interview),"-",1)))
         
         ) %>% 
  group_by(c_dob_15th,c_interview_15th) %>% 
  summarize_at(vars(exposure_estimate),.funs = list(m=~mean(., na.rm = TRUE), n=~n()))

fig <- fig_df %>% 
  plot_ly(data=.,x = ~c_dob_15th,y=~c_interview_15th,z=~m,color=~n,type="scatter3d") %>%
  layout(scene = list(xaxis = list(title = "Date of Birth",range=c("2014-01-01","2021-05-01")), 
                      yaxis = list(title = "Date of Measurement",range=c("2019-01-01","2021-05-01")),
                      zaxis = list(title = "Mobility Restriction")))
                    

fig
