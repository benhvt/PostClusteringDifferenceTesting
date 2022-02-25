# CURTA Simulation : Behaviour of p-values for two cases 


# ID of task
#slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Packages 
library(PCVI)

# Function to be used
hcl3 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=3))
}

# Paramater 
nsimu <- 2000
n <- 200
muH0 <- 0
muH1X1 <- c(-5, 5, 0)
muH1X2 <- c(0,0,10)


# Save results
pval_fig2H0 <- pval_fig2H1 <- matrix(NA, nrow = nsimu, ncol = 24)
filename_rH0 <- paste0("simulations-results/simuH0", ".csv")
filename_rH1 <- paste0("simulations-results/simuH1",".csv")


for (i in 1:nsimu){
  XH0 <- data.frame(X1 = rnorm(n, muH0), 
                    X2 = rnorm(n, muH0))
  XH1 <- data.frame(X1 = c(rnorm(n/3, mean = muH1X1[1]),
                           rnorm(n/3, mean = muH1X1[2]),
                           rnorm(n/3, mean = muH1X1[3])),
                    X2 = c(rnorm(n/3, mean = muH1X2[1]),
                           rnorm(n/3, mean = muH1X2[2]),
                           rnorm(n/3, mean =muH1X2[3])))
  clH0 <- hcl3(XH0)
  clH1 <- hcl3(XH1)
  
  ## H0
  # Selective test
  pval_fig2H0[i,1] <- test_selective_inference(as.matrix(XH0), k1=1, k2=2, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H0[i,2] <- test_selective_inference(as.matrix(XH0), k1=1, k2=3, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H0[i,3] <- test_selective_inference(as.matrix(XH0), k1=2, k2=3, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  
  pval_fig2H0[i,4] <- test_selective_inference(as.matrix(XH0), k1=1, k2=2, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H0[i,5] <- test_selective_inference(as.matrix(XH0), k1=1, k2=3, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H0[i,6] <- test_selective_inference(as.matrix(XH0), k1=2, k2=3, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  
  #Multimodality test
  pval_fig2H0[i, 7] <- test_multimod(as.matrix(XH0), g=1, cl = clH0, k1=1, k2=2)$pval
  pval_fig2H0[i, 8] <- test_multimod(as.matrix(XH0), g=1, cl = clH0, k1=1, k2=3)$pval
  pval_fig2H0[i, 9] <- test_multimod(as.matrix(XH0), g=1, cl = clH0, k1=2, k2=3)$pval
  
  pval_fig2H0[i, 10] <- test_multimod(as.matrix(XH0), g=2, cl = clH0, k1=1, k2=2)$pval
  pval_fig2H0[i, 11] <- test_multimod(as.matrix(XH0), g=2, cl = clH0, k1=1, k2=3)$pval
  pval_fig2H0[i, 12] <- test_multimod(as.matrix(XH0), g=2, cl = clH0, k1=2, k2=3)$pval
  
  #t-test
  pval_fig2H0[i, 13] <- t.test(XH0[clH0==1,1], XH0[clH0==2,1])$p.value
  pval_fig2H0[i, 14] <- t.test(XH0[clH0==1,1], XH0[clH0==3,1])$p.value
  pval_fig2H0[i, 15] <- t.test(XH0[clH0==3,1], XH0[clH0==2,1])$p.value
  
  pval_fig2H0[i, 16] <- t.test(XH0[clH0==1,2], XH0[clH0==2,2])$p.value
  pval_fig2H0[i, 17] <- t.test(XH0[clH0==1,2], XH0[clH0==3,2])$p.value
  pval_fig2H0[i, 18] <- t.test(XH0[clH0==3,2], XH0[clH0==2,2])$p.value
  
  #Merging
  pval_fig2H0[i, 19] <- merge_selective_inference(as.matrix(XH0), k1=1, k2=2, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H0[i, 20] <- merge_selective_inference(as.matrix(XH0), k1=1, k2=3, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H0[i, 21] <- merge_selective_inference(as.matrix(XH0), k1=2, k2=3, cl=clH0, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  
  pval_fig2H0[i, 22] <- merge_selective_inference(as.matrix(XH0), k1=1, k2=2, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H0[i, 23] <- merge_selective_inference(as.matrix(XH0), k1=1, k2=3, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H0[i, 24] <- merge_selective_inference(as.matrix(XH0), k1=2, k2=3, cl=clH0, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  
  ## H1
  # Selective test
  pval_fig2H1[i,1] <- test_selective_inference(as.matrix(XH1), k1=1, k2=2, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H1[i,2] <- test_selective_inference(as.matrix(XH1), k1=1, k2=3, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H1[i,3] <- test_selective_inference(as.matrix(XH1), k1=2, k2=3, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  
  pval_fig2H1[i,4] <- test_selective_inference(as.matrix(XH1), k1=1, k2=2, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H1[i,5] <- test_selective_inference(as.matrix(XH1), k1=1, k2=3, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H1[i,6] <- test_selective_inference(as.matrix(XH1), k1=2, k2=3, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  
  #Multimodality test
  pval_fig2H1[i, 7] <- test_multimod(as.matrix(XH1), g=1, cl = clH1, k1=1, k2=2)$pval
  pval_fig2H1[i, 8] <- test_multimod(as.matrix(XH1), g=1, cl = clH1, k1=1, k2=3)$pval
  pval_fig2H1[i, 9] <- test_multimod(as.matrix(XH1), g=1, cl = clH1, k1=2, k2=3)$pval
  
  pval_fig2H1[i, 10] <- test_multimod(as.matrix(XH1), g=2, cl = clH1, k1=1, k2=2)$pval
  pval_fig2H1[i, 11] <- test_multimod(as.matrix(XH1), g=2, cl = clH1, k1=1, k2=3)$pval
  pval_fig2H1[i, 12] <- test_multimod(as.matrix(XH1), g=2, cl = clH1, k1=2, k2=3)$pval
  
  #t-test
  pval_fig2H1[i, 13] <- t.test(XH1[clH1==1,1], XH1[clH1==2,1])$p.value
  pval_fig2H1[i, 14] <- t.test(XH1[clH1==1,1], XH1[clH1==3,1])$p.value
  pval_fig2H1[i, 15] <- t.test(XH1[clH1==3,1], XH1[clH1==2,1])$p.value
  
  pval_fig2H1[i, 16] <- t.test(XH1[clH1==1,2], XH1[clH1==2,2])$p.value
  pval_fig2H1[i, 17] <- t.test(XH1[clH1==1,2], XH1[clH1==3,2])$p.value
  pval_fig2H1[i, 18] <- t.test(XH1[clH1==3,2], XH1[clH1==2,2])$p.value
  
  #Merging
  pval_fig2H1[i, 19] <- merge_selective_inference(as.matrix(XH1), k1=1, k2=2, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H1[i, 20] <- merge_selective_inference(as.matrix(XH1), k1=1, k2=3, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  pval_fig2H1[i, 21] <- merge_selective_inference(as.matrix(XH1), k1=2, k2=3, cl=clH1, cl_fun = hcl3, g=1, ndraws = 2000)$pval
  
  pval_fig2H1[i, 22] <- merge_selective_inference(as.matrix(XH1), k1=1, k2=2, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H1[i, 23] <- merge_selective_inference(as.matrix(XH1), k1=1, k2=3, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  pval_fig2H1[i, 24] <- merge_selective_inference(as.matrix(XH1), k1=2, k2=3, cl=clH1, cl_fun = hcl3, g=2, ndraws = 2000)$pval
  write.csv(pval_fig2H0, file = filename_rH0, row.names = F)
  write.csv(pval_fig2H1, file = filename_rH1, row.names = F)
}





