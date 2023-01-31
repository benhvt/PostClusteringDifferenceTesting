# ------- Simulations : Impact of the dimension of the data-------#

#-p=4, 0 correlation, 0 clusters, ndraws = 2000 

library(VALIDICLUST)
library(dplyr)
library(mvtnorm)

#Paramaters 
nsimu <- 1000
n <- 100
p <- 10
delta <- 5
prop_var_cor <- 0
corr <- 0.5


#Clustering functions 
cl2_fun <- function(x){
  cah <- fastcluster::hclust(dist(x), method = "ward.D2")
  return(as.factor(cutree(cah, k=2)))
}

cl4_fun <-function(x){
  cah <- fastcluster::hclust(dist(x), method = "ward.D2")
  return(as.factor(cutree(cah, k=4)))
}

filename_SI_2cl  <- "results/nb_var/p=10/selective_inference_2cl_delta=5_prop_var_cor=0"
filename_SI_4cl  <- "results/nb_var/p=10/selective_inference_4cl_delta=5_prop_var_cor=0"
filename_multimod <- "results/nb_var/p=10/multimod_delta=5_prop_var_cor=0"
filename_merge_4cl <- "results/nb_var/p=10/merge_selective_inference_4cl_delta=5_prop_var_cor=0"
filename_ttest <- "results/nb_var/p=10/ttest_4cl_delta=5_prop_var_cor=0"

pval_multimod <- pval_ttest <- pval_SI_2cl <- pval_SI_4cl <- pval_merge_4cl <- matrix(NA, nrow = nsimu, ncol = p)
colnames(pval_multimod) <- colnames(pval_ttest) <- colnames(pval_SI_2cl) <- colnames(pval_SI_4cl) <- colnames(pval_merge_4cl) <- paste("p", 1:p)


for (i in 1:nsimu){
  X <- matrix(NA, nrow = n, ncol = p)
  for (k in 1:ncol(X)){
    X[,k] <- c(rnorm(n/2, mean = 0), rnorm(n/2, mean = delta))
  }
  
  
  cl2 <- cl2_fun(X)
  cl4 <- cl4_fun(X)
  
  
  #Aply tests on all variables
  for (k in 1:p){
    #Looking for the two most extrem clusters in the 4 clusters partitions 
    ord_mean <- data.frame(X=X[,k], Cluster = cl4) %>% group_by(Cluster) %>% summarise(meanCl = mean(X))
    cl4_1 <- as.numeric(ord_mean$Cluster[which.min(ord_mean$meanCl)])
    cl4_2 <- as.numeric(ord_mean$Cluster[which.max(ord_mean$meanCl)])
    
    pval_multimod[i,k] <- test_multimod(X, g=k, k1=1, k2=2, cl=cl2)$pval
    pval_SI_2cl[i,k] <- test_selective_inference(X, g=k, k1=1, k2=2, cl=cl2, cl_fun = cl2_fun)$pval
    pval_SI_4cl[i,k] <- test_selective_inference(X, g=k, k1=cl4_1, k2=cl4_2, cl=cl4, cl_fun = cl4_fun)$pval
    pval_merge_4cl[i,k] <- merge_selective_inference(X, g=k, k1=cl4_1, k2=cl4_2, cl=cl4, cl_fun = cl4_fun)$pval
    pval_ttest[i,k] <- try(t.test(X[cl2==1,k], X[cl2==2,k])$p.value)
    write.csv(pval_multimod, file = paste(filename_multimod, "csv", sep = "."))
    write.csv(pval_SI_2cl, paste(filename_SI_2cl, "csv", sep = "."))
    write.csv(pval_SI_4cl, paste(filename_SI_4cl, "csv", sep = "."))
    write.csv(pval_merge_4cl, paste(filename_merge_4cl, "csv", sep = "."))
    write.csv(pval_ttest, paste(filename_ttest, "csv", sep = "."))
  }
}
