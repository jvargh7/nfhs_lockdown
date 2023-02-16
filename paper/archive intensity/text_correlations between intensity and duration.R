source("analysis/nla_analytic sample processing.R")

v1 <- c("exposure_estimate","p1_estimate","p2_estimate","p3_estimate","p4_estimate")
v2 <- c("exposure_gt20","p1_gt20","p2_gt20","p3_gt20","p4_gt20")

out = map2_dfr(v1,v2,
         function(x,y){
           data.frame(
             v1 = x,
             v2 = y,
             cor = cor(analytic_sample[,x],analytic_sample[,y],use = "complete.obs") %>% as.numeric(.)
           )
           
           
         })
