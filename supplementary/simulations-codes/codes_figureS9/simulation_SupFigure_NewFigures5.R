################################################################################
#                                                                              #
# Supplementary Figures 9 : with skew t-distribution                           #
#                                                                              #
################################################################################

source("codes/utils.R")
library(sn)
library(pbapply)
library(clustvarsel)
library(sparcl)
library(reshape2)
library(mvtnorm)
library(mclust)

#-- Parameter setting 
samp_size <- 200

p <- 5
delta_grid <- c(0, 8)
param <- list()

param$scenario1 <- list(pi = 0.5,
                        sig = diag(1,2),
                        beta = cbind(matrix(0, ncol = 3, nrow = 2)),
                        var_ir = 1,
                        alpha = 1,
                        nu = 100,
                        gr_truth = c(0,1,0,0,0),
                        Scenario = "S1")

param$scenario2 <- list(pi = 0.5,
                        sig = diag(1,2),
                        beta = cbind(matrix(0, ncol = 3, nrow = 2)),
                        var_ir = 1,
                        alpha = 10,
                        nu = 4,
                        gr_truth = c(0,1,0,0,0),
                        Scenario = "S2")

param$scenario3 <- list(pi = 0.5,
                        sig = diag(1,2),
                        beta = cbind(matrix(0, ncol = 3, nrow = 2)),
                        var_ir = 1,
                        alpha = 10,
                        nu = 100,
                        gr_truth = c(0,1,0,0,0),
                        Scenario = "S3")



param$scenario4 <- list(pi = 0.5,
                        sig = diag(1,2),
                        beta = cbind(matrix(0, ncol = 3, nrow = 2)),
                        var_ir = 1,
                        alpha = 1,
                        nu = 4,
                        gr_truth = c(0,1,0,0,0),
                        Scenario = "S4")


sim_fun<- function(delta){
  print(paste0("delta=", delta))
  
  res_scenario <- lapply(param, function(x){
    mu1 = c(0,0)
    mu2 = c(0, delta)
    n1 <- x$pi*samp_size
    n2 <- (1-x$pi)*samp_size
    true_label <- c(rep(1, n1), rep(2, n2))
    X12_1 <- rmst(n1, xi = mu1, Omega = x$sig, alpha = rep(x$alpha, 2), nu = x$nu)
    X12_2 <- rmst(n2, xi = mu2, Omega = x$sig, alpha = rep(x$alpha, 2), nu = x$nu)
    X12 <- rbind(X12_1, X12_2)
    Xirr <- X12 %*% x$beta + rmst(samp_size, xi = rep(0,3), Omega = x$var_ir*diag(1,3), alpha = rep(x$alpha, 3), nu = x$nu)
    X <- cbind(X12, Xirr)
    
    
    # Post-clustering inference with kmeans
    cl <- kmean_fun(X)
    pval <- apply_post_clustering_tests(X, cl=cl, cl_fun = kmean_fun, method = c("SI", "multimod"))
    var_sel_PostClust_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                       Pvalues = c(pval$SI, pval$multimod),
                                       FeatureSelection = rep(c("SI", "multimod"), each = p),
                                       Scenario = x$Scenario, 
                                       delta = delta,
                                       Clustering = "k-means",
                                       ARI = adjustedRandIndex(cl, true_label))
    
    # Post-clustering inference with CAH
    cl <- hcl_fun(X)
    pval_cah <- apply_post_clustering_tests(X, cl=cl, cl_fun = hcl_fun, method = c("SI", "multimod"))
    var_sel_PostClust_cah_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                           Pvalues = c(pval_cah$SI, pval_cah$multimod),
                                           FeatureSelection = rep(c("SI", "multimod"), each = p),
                                           Scenario = x$Scenario, 
                                           delta = delta,
                                           Clustering = "CAH",
                                           ARI = adjustedRandIndex(cl, true_label))
    
    
    
    # Post-clustering inference with Mclust
    cl <- mclust_fun(X)
    pval_mclust <- apply_post_clustering_tests(X, cl=cl, cl_fun = mclust_fun, method = c("SI", "multimod"))
    var_sel_PostClust_mclust_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                              Pvalues = c(pval_mclust$SI, pval_mclust$multimod),
                                              FeatureSelection = rep(c("SI", "multimod"), each = p),
                                              Scenario = x$Scenario,
                                              delta = delta,
                                              Clustering = "MClust",
                                              ARI = adjustedRandIndex(cl, true_label))
    
    # Post-clustering infercen with Fuzzy clustering
    cl <- fuzzy_fun(X)
    pval_fuzzy <- apply_post_clustering_tests(X, cl=cl, cl_fun = fuzzy_fun, method = c("SI", "multimod"))
    var_sel_PostClust_fuzzy_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                             Pvalues = c(pval_fuzzy$SI, pval_fuzzy$multimod),
                                             FeatureSelection = rep(c("SI", "multimod"), each = p),
                                             Scenario = x$Scenario,
                                             delta = delta,
                                             Clustering = "Fuzzy c-means",
                                             ARI = adjustedRandIndex(cl, true_label))
    
    return(rbind.data.frame(var_sel_PostClust_df,
                            var_sel_PostClust_cah_df,
                            var_sel_PostClust_mclust_df,
                            var_sel_PostClust_fuzzy_df))
    
  })
  res_temp <-  do.call("rbind.data.frame", res_scenario)
  return(res_temp)
}

res_delta <- lapply(delta_grid, sim_fun)

res <- do.call("rbind.data.frame", res_delta)

#-- Save Results 
# ID of task
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

write.csv(res, 
          file = paste0("supplementary/simulations-results/results_figureS9/results_", slar_taskid, ".csv"), 
          row.names = F)
