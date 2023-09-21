################################################################################
#                                                                              #
#             Supplementary Figures 6 : Comparison with clustvarsel            #
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


param$scenario1 <- list(pi = 0.3,
                        sig = diag(1,2),
                        beta = cbind(matrix(c(0, 0), nrow = 2), 
                                     matrix(c(0, 1), nrow = 2),
                                     matrix(0, ncol = 1, nrow = 2)),
                        var_ir = 1, 
                        gr_truth = c(0,1,0,1,0),
                        Scenario = "S1")


sim_fun<- function(delta){
  print(paste0("delta=", delta))
  
  res_scenario <- lapply(param, function(x){
    mu1 = c(0,0)
    mu2 = c(0, delta)
    n1 <- x$pi*samp_size
    n2 <- (1-x$pi)*samp_size
    true_label <- c(rep(1, n1), rep(2, n2))
    X12_1 <- rmvnorm(n1, mu1, x$sig)
    X12_2 <- rmvnorm(n2, mu2, x$sig)
    X12 <- rbind(X12_1, X12_2)
    Xirr <- X12 %*% x$beta + rmvnorm(samp_size, sigma = x$var_ir*diag(1,3))
    X <- cbind(X12, Xirr)
    
  
    # Post-clustering inference with CAH
    cl <- hcl_fun(X)
    start_SI <- Sys.time()
    pval_cah_SI <- apply_post_clustering_tests(X, cl=cl, cl_fun = hcl_fun, method = c("SI"))
    end_SI <- Sys.time()
    
    start_multimod <- Sys.time()
    pval_cah_multimod <- apply_post_clustering_tests(X, cl=cl, cl_fun = hcl_fun, method = c("multimod"))
    end_multimod <- Sys.time()
    
    var_sel_PostClust_cah_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                           Selection = 1*(c(pval_cah_SI$SI, pval_cah_multimod$multimod)<0.05),
                                           FeatureSelection = rep(c("SI", "multimod"), each = p),
                                           Scenario = x$Scenario, 
                                           delta = delta,
                                           Clustering = "CAH",
                                           ARI = adjustedRandIndex(cl, true_label), 
                                           Time = c(rep(end_SI-start_SI,p), rep(end_multimod - start_multimod,p)))
    
    
    
    # Post-clustering inference with Mclust
    cl <- mclust_fun(X)
    start_SI <- Sys.time()
    pval_cah_SI <- apply_post_clustering_tests(X, cl=cl, cl_fun = mclust_fun, method = c("SI"))
    end_SI <- Sys.time()
    
    start_multimod <- Sys.time()
    pval_cah_multimod <- apply_post_clustering_tests(X, cl=cl, cl_fun = mclust_fun, method = c("multimod"))
    end_multimod <- Sys.time()    
    var_sel_PostClust_mclust_df <- data.frame(Variable = rep(paste0("X",1:p), 2),
                                              Selection = 1*(c(pval_cah_SI$SI, pval_cah_multimod$multimod)<0.05),
                                              FeatureSelection = rep(c("SI", "multimod"), each = p),
                                              Scenario = x$Scenario,
                                              delta = delta,
                                              Clustering = "MClust",
                                              ARI = adjustedRandIndex(cl, true_label),
                                              Time = c(rep(end_SI-start_SI,p), rep(end_multimod - start_multimod,p)))
    
    # feature selection for mclust
    start_mclust <- Sys.time()
    res_mclust <- try(clustvarsel(X, verbose = F, G=2), silent = TRUE)
    end_mclust <- Sys.time()
    
    if (class(res_mclust) == "try-error"){
      var_sel_mclust <- rep(NA, p)
      ari_mclust <- NA
      time_mclust <- NA
    }
    
    else{
      cl_mclust <- res_mclust$model$classification
      ari_mclust <- mclust::adjustedRandIndex(cl_mclust, true_label)
      var_sel_mclust <- rep(0, p)
      var_sel_mclust[res_mclust$subset] <- 1
      time_mclust <- end_mclust-start_mclust
    }
    var_sel_mclust_df <- data.frame(Variable = paste0("X",1:p),
                                    Selection = as.numeric(var_sel_mclust),
                                    FeatureSelection = "Mclust", 
                                    Scenario = x$Scenario,
                                    Clustering = "Mclust",
                                    delta = delta,
                                    ARI = ari_mclust, 
                                    Time = rep(time_mclust, p))
    
    return(rbind.data.frame(var_sel_PostClust_cah_df,
                            var_sel_PostClust_mclust_df,
                            var_sel_mclust_df))
    
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
          file = paste0("supplementary/simulations-results/results_figureS6/results_", slar_taskid, ".csv"), 
          row.names = F)

