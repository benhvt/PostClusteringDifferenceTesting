# Application :  Negative control dataset 
# Table 1 

library(PCVI)
library(palmerpenguins)

data("penguins")
penguins_NA <- na.omit(penguins)

# Subset for the speci Gentoo 
gentoo <- subset(penguins_NA, sex=="female"&species == 'Gentoo')

# Build 3 clusters
data_gentoo <- scale(gentoo[,3:6])

hcl3 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D2"), k=3))
}
data_gentoo <- scale(gentoo[,3:6])
cl <- hcl3(data_gentoo)

# Inference 
## For Cluster 1  vs Cluster 2 

pval_gentoo_C1C2 <- matrix(NA, nrow = ncol(data_gentoo), ncol=4)
for (i in 1:ncol(data_gentoo)){
  pval_gentoo_C1C2[i, 1] <- test_selective_inference(data_gentoo, k1=1, k2=2, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C1C2[i, 2] <- merge_selective_inference(data_gentoo, k1=1, k2=2, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C1C2[i, 3] <- test_multimod(data_gentoo, g=i, cl=cl, k1=1, k2=2)$pval
  pval_gentoo_C1C2[i, 4] <- t.test(data_gentoo[cl==1,i], data_gentoo[cl==2,i])$p.value
}
colnames(pval_gentoo_C1C2) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_gentoo_C1C2) <- colnames(data_gentoo)
round(pval_gentoo_C1C2,4) %>% htmlTable::htmlTable()

## For Cluster 1  vs Cluster 3 

pval_gentoo_C1C3 <- matrix(NA, nrow = ncol(data_gentoo), ncol=4)
for (i in 1:ncol(data_gentoo)){
  pval_gentoo_C1C3[i, 1] <- test_selective_inference(data_gentoo, k1=1, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C1C3[i, 2] <- merge_selective_inference(data_gentoo, k1=1, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C1C3[i, 3] <- test_multimod(data_gentoo, g=i, cl=cl, k1=1, k2=3)$pval
  pval_gentoo_C1C3[i, 4] <- t.test(data_gentoo[cl==1,i], data_gentoo[cl==3,i])$p.value
  
}
colnames(pval_gentoo_C1C3) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_gentoo_C1C3) <- colnames(data_gentoo)
round(pval_gentoo_C1C3,4) %>% htmlTable::htmlTable()

## For Cluster 2  vs Cluster 3 

pval_gentoo_C2C3 <- matrix(NA, nrow = ncol(data_gentoo), ncol=4)
for (i in 1:ncol(data_gentoo)){
  pval_gentoo_C2C3[i, 1] <- test_selective_inference(data_gentoo, k1=2, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C2C3[i, 2] <- merge_selective_inference(data_gentoo, k1=2, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_gentoo_C2C3[i, 3] <- test_multimod(data_gentoo, g=i, cl=cl, k1=2, k2=3)$pval
  pval_gentoo_C2C3[i, 4] <- t.test(data_gentoo[cl==2,i], data_gentoo[cl==3,i])$p.value
  
}
colnames(pval_gentoo_C2C3) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_gentoo_C2C3) <- colnames(data)
round(pval_gentoo_C2C3,4) %>% htmlTable::htmlTable()

pval_gentoo <- rbind(pval_gentoo_C1C2, pval_gentoo_C1C3, pval_gentoo_C2C3)
