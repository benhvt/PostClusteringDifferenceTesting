# CURTA Simulation : Statistical Power of detection according to delta, the mean difference between two clusters

# Packages 
library(PCVI)

# Function to be used
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

hcl4 <- function(x){
  clust <- quantile(x, probs = seq(0,1, length.out = 5))
  cl <- cut(x, clust, include.lowest = T)
  cl <- factor(cl, labels = 1:4)
  return(as.factor(cl))
}

# Paramaters 
nsimu <- 2000
n <- 200
delta <- seq(0,8, 0.2)
alpha <- 0.05


# Save results
pval_delta_SI_4cl <- pval_delta_SI_2cl <- pval_delta_merge <- pval_delta_Dip <- matrix(NA, nrow = nsimu, ncol = length(delta))
colnames(pval_delta_SI_2cl) <- colnames(pval_delta_SI_4cl) <- colnames(pval_delta_merge) <- colnames(pval_delta_Dip) <- paste0("delta=", delta)
filename_rSI_4cl <- paste0("simulations-results/power_SI_4cl", ".csv")
filename_rSI_2cl <- paste0("simulations-results/power_SI_2cl", ".csv")
filename_rMerge <- paste0("simulations-results/power_merge", ".csv")
filename_rDip <- paste0("simulations-results/power_Dip", ".csv")



for (i in 1:length(delta)){
  #debut <- Sys.time()
  for (j in 1:nsimu){
    #print(paste0("nsimu=", j))
    X <- matrix(c(rnorm(n/2, mean = 0), rnorm(n/2, mean = delta[i])), ncol = 1)
    cl <- hcl4(X)
    cl2 <- hcl2(X)
    
    pval_delta_SI_2cl[j,i] <- test_selective_inference(X, k1=1,  k2=2, g=1, cl_fun = hcl2, cl=cl2, ndraws = 10000)$pval
    pval_delta_SI_4cl[j,i] <- test_selective_inference(X, k1=1,  k2=4, g=1, cl_fun = hcl4, cl=cl, ndraws = 10000)$pval
    pval_delta_merge[j,i] <- merge_selective_inference(X, k1=1, k2=4, g=1, cl_fun=hcl4, cl = cl, ndraws = 10000)$pval
    pval_delta_Dip[j,i] <- test_multimod(X, g=1, cl=cl, k1=1, k2=4)$pval
    write.csv(pval_delta_SI_2cl, file = filename_rSI_2cl, row.names = F)
    write.csv(pval_delta_SI_4cl, file = filename_rSI_4cl, row.names = F)
    write.csv(pval_delta_merge, file = filename_rMerge, row.names = F)
    write.csv(pval_delta_Dip, file = filename_rDip, row.names = F)
    }
  #fin <- Sys.time()
}
# time <- as.numeric(fin-debut)
# time
