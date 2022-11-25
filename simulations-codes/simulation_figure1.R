# Presentation of the problems 

#Library 
library(clusterpval)

# function 
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

# Parameters
mu <- 0
n <- 200
nsimu <- 2000
pval_pb <- matrix(NA, nrow = nsimu, ncol = 2)

for (i in 1:nsimu){
  X <- matrix(rnorm(n, mean = mu), ncol = 1)
  cl <- hcl2(X)
  pval_pb[i, 1] <- t.test(X[cl==1,1], X[cl==2,1])$p.value
  pval_pb[i, 2] <- test_clusters_approx(X, k1=1, k2=2, cl_fun = hcl2, cl = cl)$pval
  write.csv(pval_pb, file = "simulations-results/results_figure1/pval_pb.csv", row.names = F)
}
