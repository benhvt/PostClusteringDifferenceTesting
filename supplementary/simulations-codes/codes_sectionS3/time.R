# CURTA Simulation : Computation time 

# ID of task
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Packages 
library(PCVI)
library(dplyr)

# Function to be used
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

hcl4 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=4))
}

#Filename 
filename_rtime_SI_4cl <- paste0("supplementary/simulations-results/results_figureS5/time_SI_4cl", "_", slar_taskid, ".csv")
filename_rtime_SI_2cl <- paste0("supplementary/simulations-results/results_figureS5/time_SI_2cl", "_", slar_taskid, ".csv")
filename_rtime_dip_2cl <- paste0("supplementary/simulations-results/results_figureS5/time_dip_2cl", "_", slar_taskid, ".csv")
filename_rtime_dip_4cl <- paste0("supplementary/simulations-results/results_figureS5/time_dip_4cl", "_", slar_taskid, ".csv")
filename_rtime_merge_2cl <- paste0("supplementary/simulations-results/results_figureS5/time_merge_2cl", "_", slar_taskid, ".csv")
filename_rtime_merge_4cl <- paste0("supplementary/simulations-results/results_figureS5/time_merge_4cl", "_", slar_taskid, ".csv")

# Paramaters 
nsimu <- 50
n <- 200
p <- c(2:10, 25, 50, 100)

time_dip_2cl <- time_dip_4cl <- time_SI_2cl <- time_SI_4cl <- time_merge_2cl <- time_merge_4cl <- 
  matrix(NA, nrow=nsimu, ncol = length(p))

for (i in 1:length(p)) {
  for (j in 1:nsimu){
    X <- matrix(rnorm(n*p[i]), ncol=p[i])
    cl2 <- hcl2(X)
    cl4 <- hcl4(X)
    mean_in_clust <- data.frame(X, Cluster = as.factor(cl4)) %>%
      group_by(Cluster) %>%
      summarise(mean=mean(X1)) %>%
      arrange(mean)
    
    cl4_to_test <- as.numeric(mean_in_clust$Cluster[c(1,4)])
    
    deb <- Sys.time()
    test_dip <- test_multimod(X, g=1, cl=cl2, k1=1, k2=2)
    fin <- Sys.time()
    time_dip_2cl[j,i] <- fin - deb
    
    deb <- Sys.time()
    test_dip <- test_multimod(X, g=1, cl=cl4, k1=cl4_to_test[1], k2=cl4_to_test[2])
    fin <- Sys.time()
    time_dip_4cl[j,i] <- fin - deb
    
    deb <- Sys.time()
    test_SI <- test_selective_inference(X, k1=1,k2=2, g=1, cl=cl2, cl_fun = hcl2)
    fin <- Sys.time()
    time_SI_2cl[j,i] <- fin - deb
    
    deb <- Sys.time()
    test_SI <- test_selective_inference(X, k1=cl4_to_test[1], k2=cl4_to_test[2], g=1, cl=cl4, cl_fun = hcl4)
    fin <- Sys.time()
    time_SI_4cl[j,i] <- fin - deb
    
    deb <- Sys.time()
    test_SI <- merge_selective_inference(X, k1=1,k2=2, g=1, cl=cl2, cl_fun = hcl2)
    fin <- Sys.time()
    time_merge_2cl[j,i] <- fin - deb
    
    deb <- Sys.time()
    test_merge <- merge_selective_inference(X, k1=cl4_to_test[1], k2=cl4_to_test[2], g=1, cl=cl4, cl_fun = hcl4)
    fin <- Sys.time()
    time_merge_4cl[j,i] <- fin - deb
  }
  write.csv(time_dip_2cl, file = filename_rtime_dip_2cl, row.names = F)
  write.csv(time_dip_4cl, file = filename_rtime_dip_4cl, row.names = F)
  
  write.csv(time_SI_2cl, file = filename_rtime_SI_2cl, row.names = F)
  write.csv(time_SI_4cl, file = filename_rtime_SI_4cl, row.names = F)

  write.csv(time_merge_2cl, file = filename_rtime_merge_2cl, row.names = F)
  write.csv(time_merge_4cl, file = filename_rtime_merge_4cl, row.names = F)
}