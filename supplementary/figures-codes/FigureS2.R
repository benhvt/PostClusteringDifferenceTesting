#Figure S2

library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(viridis)
library(paletteer)
library(patchwork)
library(cowplot)
library(dplyr)
library(ggcorrplot)
library(latex2exp)
source(file = "utils.R")

theme_set(theme_bw())

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")
pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])
pal8 <- c(pal_illu[1], pal_illu[4], pal_illu[3], pal_illu[2], "#D0B49F", "#AB6B51", "#2F435A", "#A47786")

hcl2 <- function(x){
  distance <- dist(x, method = "euclidean")
  hcl <- hclust(distance, method="ward.D2")
  return(as.factor(cutree(hcl, k=2)))
}

hcl8<- function(x){
  clust <- quantile(x, probs = seq(0,1, length.out = 9))
  cl <- cut(x, clust, include.lowest = T)
  cl <- factor(cl, labels = 1:8)
  return(as.factor(cl))
}

hcl7 <- function(x){
  return(cutree(hclust(dist(x), method = "ward.D"), k=7))
}

# Illustration 
set.seed(16112021)
n <- 200
X.illu.over <-  data.frame(X=c(rnorm(n/2, sd = 2), rnorm(n/2, mean = 10)))
X.illu.over$Cluster <- as.factor(hcl3(X.illu.over$X))
X.illu.over$Cas <- "Overestimation"

X.illu.under <- data.frame(X=rnorm(n))
X.illu.under$Cluster <- as.factor(hcl7(X.illu.under$X))
X.illu.under$Cas <- "Underestimation"
X.illu.under$Cluster <- forcats::fct_recode(X.illu.under$Cluster, 
                                            "3"="1",
                                            "2"="2", 
                                            "1"="3",
                                            "4"="7",
                                            "5"="5",
                                            "6"="6",
                                            "7"="4")
X.illu  <- rbind(X.illu.under, X.illu.over)



p_var_illu <- ggplot(X.illu) + aes(x=X, fill = Cluster, colour = Cluster) + 
  geom_histogram(aes(y=..density..), colour = "white", bins = 25, alpha = 0.9) +
  geom_density(alpha = 0.2) +
  scale_colour_manual(values =pal8) +
  scale_fill_manual(values = pal8) +
  facet_wrap(~Cas, scale = "free") +
  ylab("Density") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.position = "bottom") 

# Results over 2000 simulations of the data 
nsimu <- 2000
pval_var_over <- read.csv(file = "supplementary/simulations-results/results_figureS2/estim_var.csv")
pval_var_under <- read.csv(file = "supplementary/simulations-results/results_figureS2/estim_var_under.csv")
pval_var_over.df <- data.frame(pvalues = as.numeric(as.matrix(pval_var_over)),
                               Estimation = c(rep("In Cluster", nsimu), rep("With all", nsimu)))
pval_var_over.df$Cas = "Overestimation"

pval_var_under.df <- data.frame(pvalues = as.numeric(as.matrix(pval_var_under)),
                                Estimation = c(rep("In Cluster", nsimu), rep("With all", nsimu)))
pval_var_under.df$Cas = "Underestimation"

pval_var.df <- rbind(pval_var_over.df, pval_var_under.df)

p_estim_var <- ggplot(pval_var.df)+ stat_qq(aes(sample=pvalues, colour = Estimation), distribution=qunif) + 
  geom_abline(slope=1, intercept=0, col="red") + xlab("Theoretical Quantiles") + 
  ylab("Empirical Quantiles") + 
  xlim(c(0, 1)) + ylim(c(0, 1)) + theme_classic(base_size=17) +
  facet_wrap(~Cas) +
  scale_colour_manual(name = "Variance estimation",  
                      values = c(pal2[2], "#1F456E"), 
                      labels = lapply(c(r'($\hat{\sigma}^2_g = \frac{1}{|C_k| + |C_l|-1}\sum_{i\in C_k, C_l}\left(X_{gi} - \bar{X}_g^{C_k, C_l}\right)^2$)',
                                        r'($\hat{\sigma}^2_g = \frac{1}{n-1}\sum_{i=1}^n\left(X_{gi}-\bar{X}_g\right)^2$)'), TeX))  +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(axis.title = element_text(size = 14), 
        strip.text = element_text(size=12), 
        legend.position = "bottom")

# Make figure
p_pb_var <-plot_grid(p_var_illu, p_estim_var, nrow = 2, labels = "AUTO", rel_widths =c(0.3,1), rel_heights = c(0.4,0.5) ) 
p_pb_var

ggsave(p_pb_var, filename = "supplementary/figures/FigureS2.pdf", dpi = 600, width = 225, height = 337.5, units = "mm")
ggsave(p_pb_var, filename = "supplementary/figures/FigureS2.png", dpi = 600, width = 225, height = 337.5, units = "mm")

