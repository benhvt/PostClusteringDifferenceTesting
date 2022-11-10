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


apply3test <- function(x, cl_fun, cl, k1, k2, sig = NULL){
  # Apply our three proposed test for a pair of clusters on all the variables of the data 
  pval <- matrix(NA, nrow = ncol(x), ncol = 3)
  if (is.null(sig)){
    for (j in 1:ncol(x)){
      pval[j,1] <- VALIDICLUST::test_selective_inference(x,
                                                         g=j,
                                                         k1 = k1,
                                                         k2 = k2,
                                                         cl = cl,
                                                         cl_fun = cl_fun,
                                                         ndraws = 2000)$pval
      xg <- data.frame(Xg=x[,j], Cluster = cl)
      Ck1k2 <- xg %>% group_by(Cluster) %>% summarise(Mean = mean(Xg)) %>% arrange(Mean)
      adjacent <- as.numeric(Ck1k2$Cluster[which(Ck1k2$Cluster == k1):which(Ck1k2$Cluster == k2)])
      if (length(adjacent) == 2){
        pval[j,2] <- pval[j,1]
      }
      else{
        pval[j,2] <- VALIDICLUST::merge_selective_inference(x,
                                                            g=j,
                                                            k1 = k1,
                                                            k2 = k2,
                                                            cl = cl,
                                                            cl_fun = cl_fun,
                                                            ndraws = 2000)$pval
      }
      pval[j,3] <- VALIDICLUST::test_multimod(x,
                                              g=j,
                                              k1=k1,
                                              k2=k2,
                                              cl = cl
      )$pval
    }
  }
  else {
    for (j in 1:ncol(x)){
      pval[j,1] <- VALIDICLUST::test_selective_inference(x,
                                                         g=j,
                                                         k1 = k1,
                                                         k2 = k2,
                                                         cl = cl,
                                                         cl_fun = cl_fun,
                                                         ndraws = 2000,
                                                         sig = sd(x[,j]))$pval
      
      xg <- data.frame(Xg=x[,j], Cluster = cl)
      Ck1k2 <- xg %>% group_by(Cluster) %>% summarise(Mean = mean(Xg)) %>% arrange(Mean)
      adjacent <- as.numeric(Ck1k2$Cluster[which(Ck1k2$Cluster == k1):which(Ck1k2$Cluster == k2)])
      if (length(adjacent) == 2){
        pval[j,2] <- pval[j,1]
      }
      else{
        pval[j,2] <- VALIDICLUST::merge_selective_inference(x,
                                                            g=j,
                                                            k1 = k1,
                                                            k2 = k2,
                                                            cl = cl,
                                                            cl_fun = cl_fun,
                                                            ndraws = 2000)$pval
      }
      
      pval[j,3] <- VALIDICLUST::test_multimod(x,
                                              g=j,
                                              k1=k1,
                                              k2=k2,
                                              cl = cl
      )$pval
    }
  }
  pval.adj <- apply(pval, 2, function(x){p.adjust(x, method = "BH")})
  return(list(pval = pval, pval.adj = pval.adj))
}

box_pval <- function(data, test, var, clust, add = "sig"){
  # Function to plot boxplot with p-values
  bxp <- data.frame(data) %>% 
    mutate(Cluster = clust) %>% 
    ggboxplot(x="Cluster", y = var, fill = "Cluster") +
    scale_colour_met_d("Hokusai1") +
    scale_fill_met_d("Hokusai1") +
    theme(strip.text = element_text(size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))
  formule <- as.formula(paste(var, "Cluster", sep = "~"))
  stat.test <- data.frame(data) %>% 
    mutate(Cluster = clust) %>%
    t_test(formule) %>%
    add_significance()
  ind.test <- ifelse(test == "SI", 1, ifelse(test == "Merge", 2, 3))
  ind.var <- which(var == colnames(data))
  stat.test$p.adj <- c(C1C2$pval[ind.var,ind.test],
                       C1C3$pval[ind.var,ind.test],
                       C1C4$pval[ind.var,ind.test],
                       C2C3$pval[ind.var,ind.test],
                       C2C4$pval[ind.var,ind.test],
                       C3C4$pval[ind.var,ind.test])
  
  stat.test$p.adj.signif <- ifelse(stat.test$p.adj<0.01, "**", 
                                   ifelse(stat.test$p.adj>=0.01 & stat.test$p.adj<0.05, "*", "ns"))
  
  stat.test <- stat.test %>% add_xy_position(x = "Cluster")
  if (add == "pval"){
    plt <- bxp + stat_pvalue_manual(stat.test, label = "p = {round(p.adj,3)}")
  }
  else {
    plt <- bxp + stat_pvalue_manual(stat.test, label = "p.adj.signif", label.size = 8, size = 8, hide.ns = T)
  }
  return(plt)
  
}