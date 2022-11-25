# Application :  Negative control dataset 
# Table 2

library(PCVI)
library(palmerpenguins)

data("penguins")
penguins_NA <- na.omit(penguins)
hcl3 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D2"), k=3))
}
data <- scale(penguins_NA[,3:6])
cl <- hcl3(data)

## For Cluster 1 (adelie) vs Cluster 2 (Gentoo)

pval_C1C2 <- matrix(NA, nrow = ncol(data), ncol=4)
for (i in 1:ncol(data)){
  pval_C1C2[i, 1] <- test_selective_inference(data, k1=1, k2=2, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C1C2[i, 2] <- merge_selective_inference(data, k1=1, k2=2, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C1C2[i, 3] <- test_multimod(data, g=i, cl=cl, k1=1, k2=2)$pval
  pval_C1C2[i, 4] <- t.test(data[cl==1,i], data[cl==2,i])$p.value
}
colnames(pval_C1C2) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_C1C2) <- colnames(data)
round(pval_C1C2,4) %>% htmlTable::htmlTable()

## For Cluster 1 (adelie) vs Cluster 3 (Chinstrap)

pval_C1C3 <- matrix(NA, nrow = ncol(data), ncol=4)
for (i in 1:ncol(data)){
  pval_C1C3[i, 1] <- test_selective_inference(data, k1=1, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C1C3[i, 2] <- merge_selective_inference(data, k1=1, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C1C3[i, 3] <- test_multimod(data, g=i, cl=cl, k1=1, k2=3)$pval
  pval_C1C3[i, 4] <- t.test(data[cl==1,i], data[cl==3,i])$p.value
  
}
colnames(pval_C1C3) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_C1C3) <- colnames(data)
round(pval_C1C3,4) %>% htmlTable::htmlTable()

## For Cluster 2 (Gentoo) vs Cluster 3 (Chinstrap)

pval_C2C3 <- matrix(NA, nrow = ncol(data), ncol=4)
for (i in 1:ncol(data)){
  pval_C2C3[i, 1] <- test_selective_inference(data, k1=2, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C2C3[i, 2] <- merge_selective_inference(data, k1=2, k2=3, g=i, ndraws = 10000, cl_fun = hcl3, cl = cl)$pval
  pval_C2C3[i, 3] <- test_multimod(data, g=i, cl=cl, k1=2, k2=3)$pval
  pval_C2C3[i, 4] <- t.test(data[cl==2,i], data[cl==3,i])$p.value
  
}
colnames(pval_C2C3) <- c("SI", "Merge SI", "Multimodality", "t-test")
rownames(pval_C2C3) <- colnames(data)
round(pval_C2C3,4) %>% htmlTable::htmlTable()

pval_appli <- rbind(pval_C1C2, pval_C1C3, pval_C2C3)