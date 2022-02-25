# CURTA Simulation : Merging function 

# Packages 
library(PCVI)

# Function to be used

#Filename 
filename_rMerge_bonf <- paste0("result_r/Supplementary/power_comp_merge_bonf",".csv")
filename_rMerge_geo <- paste0("result_r/Supplementary/power_comp_merge_geo",".csv")
filename_rMerge_harm <- paste0("result_r/Supplementary/power_comp_merge_harm",".csv")


#Functions 
hcl4 <- function(x){
  clust <- quantile(x, probs = seq(0,1, length.out = 5))
  cl <- cut(x, clust, include.lowest = T)
  cl <- factor(cl, labels = 1:4)
  return(as.factor(cl))
}

bonf_merge <- function(p){
  K <- length(p)
  return(K*min(p))
}

geometric_merge <- function(p){
  K <- length(p)
  return(exp(1)*(prod(p))^(1/K))
}

# Paramaters 
nsimu <- 2000
n <- 200
delta <- seq(0,8, 0.5)
pval_pow_harm <- pval_pow_geo <-pval_pow_bonf<- matrix(NA, ncol = length(delta), nrow = nsimu)

for (i in 1:length(delta)){
  for (j in 1:nsimu){
    #print(paste0("nsimu=", j))
    # debut <- Sys.time()
    X <- matrix(c(rnorm(n/2, mean = 0), rnorm(n/2, mean = delta[i])), ncol = 1)
    cl <- hcl4(X)
    
    test_merge <- merge_selective_inference(X, k1=1, k2=4, g=1, cl_fun=hcl4, cl = cl)
    pval_pow_harm[j,i] <- test_merge$pval
    pval_pow_bonf[j,i] <- bonf_merge(test_merge$pval_adj)
    pval_pow_geo[j,i] <- geometric_merge(test_merge$pval_adj)
    write.csv(pval_pow_harm, file = filename_rMerge_harm, row.names = F)
    write.csv(pval_pow_geo, file = filename_rMerge_geo, row.names = F)
    write.csv(pval_pow_bonf, file = filename_rMerge_bonf, row.names = F)
    
    # fin <- Sys.time()
  }
  # power_merge_harm[i, ] <- sum(pval_pow_harm<0.05, na.rm = T)/length(na.omit(pval_pow_harm))
  # power_merge_bonf[i, ] <- sum(pval_pow_bonf<0.05, na.rm = T)/length(na.omit(pval_pow_bonf))
  # power_merge_harm[i, ] <- sum(pval_pow_geo<0.05, na.rm = T)/length(na.omit(pval_pow_geo))
  

  
}