# CURTA Simulation : Estim VAR2

# Packages 
library(PCVI)
library(dplyr)

# Function to be used
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

hcl7 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=7))
}

#Filename 
filename_r <- paste0("result_r/estim_var_under", ".csv")


# Paramaters 
nsimu <- 2000
n <- 200

pval_var <- matrix(NA, nrow = nsimu, ncol = 2)
colnames(pval_var) <- c("Clusters", "All")

for (i in 1:nsimu){
  X <- matrix(rnorm(n), ncol = 1)
  cl <- as.factor(hcl7(X))
  mean_in_clust <- data.frame(X, Cluster = as.factor(cl)) %>%
    group_by(Cluster) %>%
    summarise(mean=mean(X)) %>%
    arrange(mean)
  
  cl_to_test <- as.numeric(mean_in_clust$Cluster[c(3,4)])
  
  pval_var[i,1] <- test_selective_inference(X, 
                                            g=1,
                                            k1=cl_to_test[1], 
                                            k2=cl_to_test[2],
                                            cl = cl, 
                                            cl_fun = hcl7)$pval
  pval_var[i,2] <- test_selective_inference(X,
                                            g=1,
                                            k1=cl_to_test[1],
                                            k2=cl_to_test[2],
                                            cl = cl,
                                            cl_fun = hcl7, 
                                            sig = sd(X))$pval 
  write.csv(pval_var, file = filename_r, row.names = F)
  
}