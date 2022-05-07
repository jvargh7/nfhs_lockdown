require(DescTools)
# https://rdrr.io/cran/DescTools/man/overlaps.html
overlaps <- data.frame(dates = seq.Date(as_date("2010-02-02"),as_date("2021-04-30"),by=1),
                       d_start = as_date(demonetization_start),
                       d_stop = as_date(demonetization_stop),
                       l_start = as_date(lockdown_start),
                       l_stop = as_date(lockdown_stop))


list_overlaps <- map_dfr(1:nrow(overlaps),
                         function(i){
                           dob = overlaps[i,]$dates;
                           e1_p1 = Overlap(c(dob-38*7,dob),
                                           c(as_date(demonetization_start),as_date(demonetization_stop)));
                           e1_p2 = Overlap(c(dob,dob+180),
                                           c(as_date(demonetization_start),as_date(demonetization_stop)));
                           e1_p3 = Overlap(c(dob+181,dob+360),
                                           c(as_date(demonetization_start),as_date(demonetization_stop)));
                           e1_p4 = Overlap(c(dob+361,dob+540),
                                           c(as_date(demonetization_start),as_date(demonetization_stop)));
                           e1_p5 = Overlap(c(dob+541,dob+720),
                                           c(as_date(demonetization_start),as_date(demonetization_stop)));
                           
                           e2_p1 = Overlap(c(dob-38*7,dob),
                                           c(as_date(lockdown_start),as_date(lockdown_stop)));
                           e2_p2 = Overlap(c(dob,dob+180),
                                           c(as_date(lockdown_start),as_date(lockdown_stop)));
                           e2_p3 = Overlap(c(dob+181,dob+360),
                                           c(as_date(lockdown_start),as_date(lockdown_stop)));
                           e2_p4 = Overlap(c(dob+361,dob+540),
                                           c(as_date(lockdown_start),as_date(lockdown_stop)));
                           e2_p5 = Overlap(c(dob+541,dob+720),
                                           c(as_date(lockdown_start),as_date(lockdown_stop)));
                           
                           data.frame(e1_p1,
                                      e1_p2,
                                      e1_p3,
                                      e1_p4,
                                      e1_p5,
                                      e2_p1,
                                      e2_p2,
                                      e2_p3,
                                      e2_p4,
                                      e2_p5) %>% 
                             
                             return(.)
                           
                         })


bind_cols(overlaps,list_overlaps) %>% 
  mutate_at(vars(starts_with("e")),.f = list(d = function(x) case_when(x > 90 ~ 1,
                                                                       TRUE ~ 0))) %>%
  saveRDS(.,paste0("data/overlaps.RDS"))