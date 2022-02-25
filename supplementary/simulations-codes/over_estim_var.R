# CURTA Simulation : Computation time 

# Packages 
library(PCVI)
library(dplyr)

# Function to be used
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

hcl3 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=3))
}


#Filename 
filename_r <- paste0("supplementary/simulations-results/estim_var", ".csv")


# Paramaters 
nsimu <- 2000
n <- 200

pval_var <- matrix(NA, nrow = nsimu, ncol = 2)
colnames(pval_var) <- c("Clusters", "All")

for (i in 1:nsimu){
  X <- matrix(c(rnorm(n/2, sd = 2), rnorm(n/2, mean = 10)), ncol = 1)
  cl <- as.factor(hcl3(X))
  mean_in_clust <- data.frame(X, Cluster = as.factor(cl)) %>%
    group_by(Cluster) %>%
    summarise(mean=mean(X)) %>%
    arrange(mean)
  
  cl_to_test <- as.numeric(mean_in_clust$Cluster[c(1,2)])
  
  pval_var[i,1] <- test_selective_inference(X, 
                                            g=1,
                                            k1=cl_to_test[1], 
                                            k2=cl_to_test[2],
                                            cl = cl, 
                                            cl_fun = hcl3)$pval
  pval_var[i,2] <- test_selective_inference(X, 
                                            g=1,
                                            k1=cl_to_test[1],
                                            k2=cl_to_test[2],
                                            cl = cl,
                                            cl_fun = hcl3,
                                            sig = sd(X))$pval 
  write.csv(pval_var, file = filename_r, row.names = F)
  
}