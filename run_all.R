rm(); gc(); source(".Rprofile")

print("starting gaussian urban")

source("sensitivity zscores/nlsz01u_gaussian regression urban.R")
rm(); gc(); source(".Rprofile")
print("starting gaussian rural")
source("sensitivity zscores/nlsz01r_gaussian regression rural.R")
rm(); gc(); source(".Rprofile")

folder_list <- c(
                 "main analysis/nlma01",
                 "sensitivity additive/nlsa01",
                 "sensitivity boys/nlxb01",
                 "sensitivity girls/nlxg01",
                 "sensitivity conception/nlsc01",
                 
                 "sensitivity nfhs5/nls501",
                 "sensitivity young/nlsy01",
                 "sensitivity demonetization/nlsd01")

for(f in folder_list){
  for(r in c("u","r")){
    print(paste0("Running ",f,"-",r))
    if(r == "u"){
      source(paste0(f,"u_poisson regression urban.R"))
      rm(); gc(); source(".Rprofile")
    }
    if(r == "r"){
      source(paste0(f,"r_poisson regression rural.R"))
      rm(); gc(); source(".Rprofile")
    }
    
  }
}




