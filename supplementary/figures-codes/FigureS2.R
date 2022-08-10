# Figure S2 
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(viridis)
library(paletteer)
library(patchwork)
library(grid)
library(cowplot)
library(dplyr)
library(ggcorrplot)
library(latex2exp)
source(file = "utils.R")
library(PCVI)

theme_set(theme_bw())
pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")

hcl3 <- function(x){
  clust <- quantile(x[,1], probs = seq(0,1, length.out = 4))
  cl <- cut(x[,1], clust, include.lowest = T)
  cl <- factor(cl, labels = 1:3)
  return(as.factor(cl))
}


pal <- wes_palette("Darjeeling1", 3, type = "continuous")
pal2 <- c(wes_palette("GrandBudapest2", 2, type = "continuous"), "#cad3fa", pal[2])

pal_illu <- c("#399283", "#e1d936", "#e09c6c", "#dc3c07")

# Illustration

# Parameters 
n <- 200
deltaC1C2 <- c(0.5, 1, 1.5, 3, 6, 12, 15, 18, 19, 19.5)
muX1 <- c(-5, -5+deltaC1C2[5], 15)
muX2 <- c(0,0, 0)

set.seed(15112021)
X <- data.frame(X1 = c(rnorm(n/3, mean = muX1[1]),
                         rnorm(n/3, mean = muX1[2]),
                         rnorm(n/3, mean = muX1[3])),
                  X2 = c(rnorm(n/3, mean = muX2[1]),
                         rnorm(n/3, mean = muX2[2]),
                         rnorm(n/3, mean =muX2[3])))
X$Cluster <- hcl3(X[,1:2])
pbehav_illu <- ggplot(X) + aes(x=X1, y = X2, colour = Cluster) +
  geom_point(alpha = .8) +
  scale_colour_manual(values = c(pal_illu[1], pal_illu[4], pal_illu[3])) +
  ggnewscale::new_scale_colour() +
  geom_vline(data = X %>% group_by(Cluster) %>% summarise(Mean = mean(X1)), aes(xintercept = Mean, colour = Cluster),
             linetype = "dashed", size = 1.2)+
  scale_colour_manual(values = c("#22574e", "#9a2a04", "#b37c56")) +
  guides(colour = "none") +
  annotate("segment", x = mean(X$X1[X$Cluster==1]), xend = mean(X$X1[X$Cluster==2]), y = 0, yend = 0,
           colour = "darkorange", size = 1, arrow = arrow(ends = "both")) +
  annotate('text', x=mean(X$X1[X$Cluster %in% c(1,2)]), y = .6, label = TeX(r'($\delta$)'), size = 8, colour = "darkorange") +
  labs(x=expression(X[1]), 
       y=expression(X[2])) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        strip.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))
pbehav_illu 


# Simulation 
nsimu <- 500
pvalC1C2 <- pvalC1C3 <- matrix(NA, nrow = nsimu, ncol = length(deltaC1C2))

for (i in 1:length(deltaC1C2)){
  res.simu.delta<- replicate(nsimu, expr = {
    muX1 <- c(-5, -5+deltaC1C2[i], 15)
    muX2 <- c(0,0, 0)
    X <- data.frame(X1 = c(rnorm(n/3, mean = muX1[1]),
                           rnorm(n/3, mean = muX1[2]),
                           rnorm(n/3, mean = muX1[3])),
                    X2 = c(rnorm(n/3, mean = muX2[1]),
                           rnorm(n/3, mean = muX2[2]),
                           rnorm(n/3, mean =muX2[3])))
    X$Cluster <- hcl3(X[,1:2])
    res1 <- test_selective_inference(as.matrix(X[,1:2]), k1=1, k2=2, g=1, cl_fun = hcl3, cl = X$Cluster)$pval
    res2 <- test_selective_inference(as.matrix(X[,1:2]), k1=1, k2=3, g=1, cl_fun = hcl3, cl = X$Cluster)$pval
    c(res1, res2)
  })
  pvalC1C2[,i] <- res.simu.delta[1,]
  pvalC1C3[,i] <- res.simu.delta[2,]
}

write.csv(pvalC1C2, file = "supplementary/simulations-results/suppS2_C1C2.csv", row.names = F)
write.csv(pvalC1C3, file = "supplementary/simulations-results/suppS2_C1C3.csv", row.names = F)

# Figures Results 

pvalC1C2.res <- read.csv(file = "supplementary/simulations-results/suppS2_C1C2.csv")
pvalC1C3.res <- read.csv(file = "supplementary/simulations-results/suppS2_C1C3.csv")

power.df <- data.frame(Power = c(apply(pvalC1C2.res, 2, function(x){mean(x<0.05)}),
                                 apply(pvalC1C3.res, 2, function(x){mean(x<0.05)})),
                       delta = rep(deltaC1C2, 2), 
                       Test = rep(c("Cluster 1 vs Cluster 2", "Cluster 1 vs Cluster 3"), each = length(deltaC1C2)))

p_res <- ggplot(power.df) + aes(x=delta, y = Power, colour = Test) + 
  geom_point(size = 2) + 
  geom_line(size = 0.8) + 
  scale_colour_manual(values = c("#f865b0", "#8a1048")) +
  xlab(TeX(r'($\delta$)')) +
 # ylab(expression(atop("Statistical power at the"~alpha ,"% level")))+
  ylab(TeX(r'(Statistical power at the $\alpha = 5\%$ level)')) +
  guides(colour = guide_legend(TeX(r'(Test on $X_1$)'))) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        strip.text = element_text(size=14), 
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))
p_res

# Combine the two figures
pbehav_illu + p_res + plot_annotation(tag_levels = "A")  + plot_layout(widths = c(6,8))  & theme(plot.tag = element_text(face = 'bold'))
ggsave(filename = "supplementary/figures/FigureS2.pdf", dpi = 600, height = 125, width = 260, units = "mm")
        