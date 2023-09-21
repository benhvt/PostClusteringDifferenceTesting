# CURTA Simulation : Distributional assumption

# Packages 
library(PCVI)

# Function to be used
hcl2 <- function(x){
  distance <- dist(x, method = "euclidean")
  hcl <- hclust(distance, method="ward.D2")
  return(as.factor(cutree(hcl, k=2)))
}

#Filename 
filename_rH0_SI <- paste0("supplementary/simulations-results/results_figureS8/pval_distri_H0_SI", ".csv")
filename_rH0_dip <- paste0("supplementary/simulations-results/results_figureS8/pval_distri_H0_dip", ".csv")



# Paramaters 
nsimu <- 500
n <- 200
pval_H0.distri_SI <- pval_H0.distri_dip <-matrix(NA, nrow = nsimu, ncol = 7)
deb <- Sys.time()
  for (i in 1:nsimu){
    X <- matrix(NA, nrow = n, ncol = 7)
    X[,1] <- rnorm(n, sd=2)
    X[,2] <- runif(n, -10, 10)
    X[,3] <- rchisq(n, df=4)
    X[,4] <- rexp(n, rate = 0.5)
    X[,5] <- rgamma(n, shape = 4)
    X[,6] <- rpois(n, lambda = 4)
    X[,7] <- rpois(n, lambda = 50)
    for (j in 1:ncol(X)){
      cl <- hcl2(X[,j])
      pval_H0.distri_SI[i,j] <- test_selective_inference(X[,j, drop = F],
                                                         g=1, 
                                                         k1=1, 
                                                         k2=2,
                                                         cl = cl,
                                                         cl_fun = hcl2)$pval
      pval_H0.distri_dip[i,j] <- test_multimod(X[,j, drop = F], 
                                               g=1,
                                               k1=1, 
                                               k2=2, 
                                               cl=cl)$pval
    }
    write.csv(pval_H0.distri_SI, filename_rH0_SI)
    write.csv(pval_H0.distri_dip, filename_rH0_dip)
  }
fin <- Sys.time()