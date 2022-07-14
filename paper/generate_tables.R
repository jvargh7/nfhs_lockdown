rm(list=ls()); gc(); source(".Rprofile")

source("paper/table_counts by period of exposure.R")
rm(list=ls()); gc(); source(".Rprofile")

coef_lists <- c("nlma","nls5","nlsa","nlsc","nlse","nlsy","nlsz","nlxb","nlxg","nlsd")

for (c in coef_lists){
  print(c)
  source(paste0("paper/table_",c," coefficients.R"))
  rm(list=ls()); gc(); source(".Rprofile")
}

