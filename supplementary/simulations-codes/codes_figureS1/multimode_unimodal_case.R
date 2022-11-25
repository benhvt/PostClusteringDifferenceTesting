# CURTA Simulation : Merging function 

# Packages 
library(PCVI)
library(multimode)

#Filename 
filename_r.uni.U <- paste0("supplementary/simulations-results/results_figureS1/multimode_test_unimodale_uniform", ".csv")
filename_r.uni.N <- paste0("supplementary/simulations-results/results_figureS1/multimode_test_unimodale_gaussian", ".csv")


#Parameters 
n <- 200
nsimu <- 500
met <- c("SI", "HH", "CH", "ACR")

# Unimodale cases 
pval_null_U <-pval_null_N <- matrix(NA, nrow = nsimu, ncol = length(met))

for (i in 1:nsimu){
  U <- runif(n)
  N <- rnorm(n)
  for (j in 1:length(met)){
    pval_null_U[i, j] <- modetest(U, method = met[j])$p.value
    pval_null_N[i, j] <- modetest(N, method = met[j])$p.value
  }
  write.csv(pval_null_U, file = filename_r.uni.U, row.names = F)
  write.csv(pval_null_N, file = filename_r.uni.N, row.names = F)
}

