# CURTA Simulation : Merging function 

# Packages 
library(PCVI)
library(dplyr)

# Function to be used

#Filename 
filename_rH0.harm <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_harm_H0", ".csv")
filename_rH0.geo <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_geo_H0", ".csv")
filename_rH0.bonf <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_bonf_H0", ".csv")

filename_rH1.harm <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_harm_H1", ".csv")
filename_rH1.geo <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_geo_H1", ".csv")
filename_rH1.bonf <- paste0("supplementary/simulations-results/results_figureS4/pval_merge_bonf_H1", ".csv")


filename_rtime_adj <- paste0("supplementary/simulations-results/results_figureS4/time_adj", ".csv")


#Functions 
bonf_merge <- function(p){
  K <- length(p)
  return(K*min(p))
}

harmonic_merge <- function(p){
  K <- length(p)
  return((exp(1)*log(K))*(K/(sum(1/p))))
}

geometric_merge <- function(p){
  K <- length(p)
  return(exp(1)*(prod(p))^(1/K))
}

# Paramaters 
nsimu <- 2000
n <- 200
K <- 3:7
time_K <- matrix(NA, nrow = nsimu, ncol = length(K))
pval_H0.harm <- pval_H0.geom <- pval_H0.bonf <- pval_H1.harm <- pval_H1.geom <- pval_H1.bonf <-matrix(NA, nrow = nsimu, ncol = length(K))

for (k in 1:length(K)){
 hcl_K <- function(x){
    return(cutree(hclust(dist(x), method = "ward.D"), k=K[k]))
  }
  for (i in 1:nsimu){
    X.H0 <- matrix(rnorm(n), ncol = 1)
    X.H1 <- matrix(c(rnorm(n/2, -10), rnorm(n/2, 10)), ncol = 1)
    cl.H0 <- hcl_K(X.H0)
    cl.H1 <- hcl_K(X.H1)
    
    mean_in_clust.H0 <- data.frame(X.H0, Cluster = as.factor(cl.H0)) %>%
      group_by(Cluster) %>%
      summarise(mean=mean(X.H0)) %>%
      arrange(mean)
    
    cl_to_test.H0 <- as.numeric(mean_in_clust.H0$Cluster[c(1,K[k])])
    
    mean_in_clust.H1 <- data.frame(X.H1, Cluster = as.factor(cl.H1)) %>%
      group_by(Cluster) %>%
      summarise(mean=mean(X.H1)) %>%
      arrange(mean)
    
    cl_to_test.H1 <- as.numeric(mean_in_clust.H1$Cluster[c(1,K[k])])
    
    deb <- Sys.time()
    adj_pval.H0 <- merge_selective_inference(X.H0, 
                                             g=1,
                                             k1=cl_to_test.H0[1],
                                             k2=cl_to_test.H0[2],
                                             cl=cl.H0, 
                                             cl_fun=hcl_K)$pval_adj
    fin <- Sys.time()
    time_K[i,k] <- fin - deb
    
    adj_pval.H1 <- merge_selective_inference(X.H1, 
                                             g=1,
                                             k1=cl_to_test.H1[1],
                                             k2=cl_to_test.H1[2],
                                             cl=cl.H1, 
                                             cl_fun=hcl_K)$pval_adj
    pval_H0.harm[i,k] <- harmonic_merge(adj_pval.H0)
    pval_H0.geom[i,k] <- geometric_merge(adj_pval.H0)
    pval_H0.bonf[i,k] <- bonf_merge(adj_pval.H0)
    
    pval_H1.harm[i,k] <- harmonic_merge(adj_pval.H1)
    pval_H1.geom[i,k] <- geometric_merge(adj_pval.H1)
    pval_H1.bonf[i,k] <- bonf_merge(adj_pval.H1)
    
    write.csv(pval_H0.harm, file = filename_rH0.harm, row.names = F)
    write.csv(pval_H0.geom, file = filename_rH0.geo, row.names = F)
    write.csv(pval_H0.bonf, file = filename_rH0.bonf, row.names = F)
    
    write.csv(pval_H1.harm, file = filename_rH1.harm, row.names = F)
    write.csv(pval_H1.geom, file = filename_rH1.geo, row.names = F)
    write.csv(pval_H1.bonf, file = filename_rH1.bonf, row.names = F)
    
    write.csv(time_K, file=filename_rtime_adj, row.names = F)
  }
}