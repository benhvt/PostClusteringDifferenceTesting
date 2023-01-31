# ------- Simulations : Impact of the sample size on false-positive and power -------#

library(VALIDICLUST)
library(dplyr)
#Paramaters 
nsimu <- 1000
n <- c(10, 15, 20, 25, 30, 50, 75, 100, 500)
p <- 1 
delta <- c(0, 5)

#Clustering functions 
cl2_fun <- function(x){
  cah <- fastcluster::hclust(dist(x), method = "ward.D2")
  return(as.factor(cutree(cah, k=2)))
}

cl4_fun <-function(x){
  cah <- fastcluster::hclust(dist(x), method = "ward.D2")
  return(as.factor(cutree(cah, k=4)))
}

for (d in 1:length(delta)){
  pval_multimod <- pval_ttest <- pval_SI_2cl <- pval_SI_4cl <- pval_merge_4cl <- matrix(NA, nrow = nsimu, ncol = length(n))
  colnames(pval_multimod) <- colnames(pval_ttest) <- colnames(pval_SI_2cl) <- colnames(pval_SI_4cl) <- colnames(pval_merge_4cl) <- paste("n", n, sep = "=")
  filename_SI_2cl <- paste("results/sample_size/selective_inference_2cl_", "d=", delta[d], sep = "")
  filename_SI_4cl <- paste("results/sample_size/selective_inference_4cl_", "d=", delta[d], sep = "") 
  filename_merge_4cl <- paste("results/sample_size/merge_selective_inference_4cl_", "d=", delta[d], sep = "") 
  filename_multimod <-  paste("results/sample_size/multimod_", "d=", delta[d], sep = "")
  filename_ttest <-  paste("results/sample_size/ttest_", "d=", delta[d], sep = "")
  for (i in 1:nsimu){
    for (j in 1:length(n)){
      X <- as.matrix(c(rnorm(n[j]/2, mean=0, sd=1),
                    rnorm(n[j]/2, mean = delta[d], sd=1)),
                    ncol=1)
      cl2 <- cl2_fun(X)
      cl4 <- cl4_fun(X)
      
      #Looking for the two most extrem clusters in the 4 clusters partitions 
      ord_mean <- data.frame(X=X, Cluster = cl4) %>% group_by(Cluster) %>% summarise(meanCl = mean(X))
      cl4_1 <- as.numeric(ord_mean$Cluster[which.min(ord_mean$meanCl)])
      cl4_2 <- as.numeric(ord_mean$Cluster[which.max(ord_mean$meanCl)])
      
      #Apply tests
      pval_multimod[i,j] <- test_multimod(X,g=1, cl = cl2, k1 = 1, k2=2)$pval
      pval_SI_2cl[i,j] <- test_selective_inference(X, k1=1, k2=2, g=1, cl_fun = cl2_fun, cl=cl2)$pval
      pval_SI_4cl[i,j] <- test_selective_inference(X, k1=cl4_1, k2=cl4_2, g=1, cl_fun = cl4_fun, cl=cl4)$pval
      pval_merge_4cl[i,j] <- merge_selective_inference(X, g=1, cl_fun = cl4_fun, cl = cl4, k1 = cl4_1, k2=cl4_2)$pval
      ttest <- try(t.test(X[cl2==1], X[cl2==2]), silent = TRUE)
      pval_ttest[i,j] <- ifelse(class(ttest) != 'try-error', ttest$p.value, NA)
      
      
      # Save results 
      write.csv(pval_multimod, file = paste(filename_multimod, "csv", sep = "."))
      write.csv(pval_SI_2cl, paste(filename_SI_2cl, "csv", sep = "."))
      write.csv(pval_SI_4cl, paste(filename_SI_4cl, "csv", sep = "."))
      write.csv(pval_merge_4cl, paste(filename_merge_4cl, "csv", sep = "."))
      write.csv(pval_ttest, paste(filename_ttest, "csv", sep = "."))
      
    }
  }
}