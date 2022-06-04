# https://cran.r-project.org/web/packages/modmarg/vignettes/delta-method.html

delta_method <- function(model,pred_df){
  
  pred_e = predict(model,newdata=pred_df,type="link")
  
  pred_e_response = 1 / (1 + exp(-pred_e))
  mean_pred = Hmisc::wtd.mean(pred_e_response,weights = pred_df$combined_sampleweight,normwt = TRUE)
  
  
  deriv <- as.vector(exp(-pred_e) / (1 + exp(-pred_e))^2)
  
  x = model.matrix(model)
  j <- deriv %*% x / nrow(x)
  variance <- j %*% vcov(model) %*% t(j)
  se_pred <- sqrt(diag(variance))
  
  return(list(mean = mean_pred,
         se = se_pred))
  
}