# CURTA Simulation : Merging function 

# Packages 
library(PCVI)
library(multimode)

#Filename 

filename_r.pow <- paste0("supplementary/simulations-results/multimode_test_power", ".csv")


#Parameters 
n <- 200
nsimu <- 500
met <- c("SI", "HH", "CH", "ACR")
delta <- seq(0,8, 0.5)


# Bimodale cases 
power <- matrix(NA, nrow = length(delta), ncol = length(met))

for (i in 1:length(delta)){
  detect_delta <- matrix(NA, nrow = nsimu, ncol = length(met))
  for (j in 1:nsimu){
    Y_d <-c(rnorm(n/2), rnorm(n/2, mean = delta[i]))
    for (k in 1:length(met)){
      detect_delta[j,k] <-  modetest(Y_d, method = met[k])$p.value<0.05
    }
    power[i,] <- apply(detect_delta, 2, mean)
  }
  write.csv(power, file = filename_r.pow, row.names = F)
}

