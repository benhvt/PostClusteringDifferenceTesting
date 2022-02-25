library(ggplot2)
#Functions 

#qq-plot
qqplot_unif <- function(x){
  ggplot()+ stat_qq(aes(sample=x), distribution=qunif) + 
    geom_abline(slope=1, intercept=0, col="red") + xlab("Theoretical Quantiles") + 
    ylab("Empirical Quantiles") + 
    xlim(c(0, 1)) + ylim(c(0, 1)) + theme_classic(base_size=17) 
}

# Fonction de clustering
hcl2 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=2))
}

hcl3 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=3))
}

hcl10 <- function(x){
  clust <- quantile(x, probs = seq(0,1, length.out = 10))
  cl <- cut(x, clust, include.lowest = T)
  cl <- factor(cl, labels = 1:9)
  return(as.factor(cl))
}

hcl4 <- function(x){
  clust <- quantile(x, probs = seq(0,1, length.out = 5))
  cl <- cut(x, clust, include.lowest = T)
  cl <- factor(cl, labels = 1:4)
  return(as.factor(cl))
}
