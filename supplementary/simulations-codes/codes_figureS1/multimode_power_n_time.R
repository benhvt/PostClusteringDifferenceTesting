# CURTA Simulation : Merging function 

# Packages 
library(PCVI)
library(multimode)

#Filename 

filename_r.pow_n <- paste0("supplementary/simulations-results/results_figureS1/multimode_test_power_n", ".csv")
filename_rtime_multimode <- paste0("supplementary/simulations-results/results_figureS1/time_multimode", ".csv")

#Parameters 
nsimu <- 500
met <- c("SI", "HH", "CH", "ACR")
delta <- 3.5
n <- c(10,15, 25, 50, 75, 100, 200, 1000 )

#Computation time and power for delta fixed and n variables 
power_n <- matrix(NA, nrow = length(n), ncol = length(met))
time <- matrix(NA, nrow = length(n), ncol = length(met))

for (i in 1:length(n)){
  detect_n <- matrix(NA, nrow = nsimu, ncol = length(met))
  time_n <- matrix(NA, nrow = nsimu, ncol = length(met))
  for (j in 1:nsimu){
    Y_d <-c(rnorm(n[i]/2), rnorm(n[i]/2, mean = delta))
    for (k in 1:length(met)){
      deb <- Sys.time()
      test <- modetest(Y_d, method = met[k])
      fin <- Sys.time()
      time_n[j,k] <- fin - deb
      detect_n[j,k] <- test$p.value<0.05
    }
    power_n[i,] <- apply(detect_n, 2, mean)
    time[i,] <- apply(time_n, 2, mean)
  }
  write.csv(power_n, file = filename_r.pow_n)
  write.csv(time, file = filename_rtime_multimode)
}


